// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.


//! This module is responsible for discovering all items that will contribute to
//! to code generation of the crate. The important part here is that it not only
//! needs to find syntax-level items (functions, structs, etc) but also all
//! their monomorphized instantiations. Every non-generic function (that is not
//! dead code) will produce one machine-code artifact. Every generic function
//! can produce from zero to N artifacts, depending with which type arguments it
//! is instantiated.
//! This also applies to generic items from other crates: A generic definition
//! in crate X might produce monomorphizations that are compiled into crate Y.
//! We also have to collect these here.
//!
//! Functions and methods are not the only things that result in machine code
//! being generated. There is also drop-glue, shim functions, and vtables.
//!
//! - Statics and consts can all be discovered during the initial pass through
//!   the HIR, since they are always monomorphic.
//!
//! - Closures are discovered through their constructing expression in MIR.
//!   This should find all closure instances, as opposed to call-expression,
//!   where we would miss closures that are created but never called and ones
//!   that are called through and abstraction like `&Fn()`.
//!
//! - Vtables - NYI
//!
//! - Object shims - NYI
//!
//! - Statically dispatched trait methods
//!
//! - Default implementations of trait methods
//!
//!
//! ## Drop Glue
//!
//! TODO: get_drop_glue_type() will map the drop-glue of several types onto one
//!       is this deterministic enough to be tested manually?
//!
//! TODO: instantiation policy, eager (for incremental compilation) vs lazy
//!       (for non-incremental compilation).

use rustc_front::hir;
use rustc_front::intravisit as hir_visit;

use rustc::front::map as hir_map;
use rustc::middle::def_id::DefId;
use rustc::middle::{ty, traits};
use rustc::middle::subst::{self, Substs};
use rustc::session;

use rustc::mir::repr as mir;
use rustc::mir::visit as mir_visit;
use rustc::mir::visit::Visitor as MirVisitor;

use syntax::ast::{self, NodeId};
use syntax::codemap::DUMMY_SP;
use trans::context::CrateContext;
use trans::common::{type_needs_drop, fulfill_obligation};
use trans::monomorphize;
use trans::inline;
use util::nodemap::FnvHashSet;


#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum CodeGenItem<'tcx> {
    DropGlue(ty::Ty<'tcx>),
    Fn(NodeId, &'tcx Substs<'tcx>),
    Const(NodeId),
    Static(NodeId)
}

/// Same as `unique_type_name()` but with the result pushed onto the given
/// `output` parameter.
pub fn push_unique_type_name<'a, 'tcx>(cx: &CrateContext<'a, 'tcx>,
                                          t: ty::Ty<'tcx>,
                                          qualified: bool,
                                          output: &mut String) {
    match t.sty {
        ty::TyBool              => output.push_str("bool"),
        ty::TyChar              => output.push_str("char"),
        ty::TyStr               => output.push_str("str"),
        ty::TyInt(ast::TyIs)    => output.push_str("isize"),
        ty::TyInt(ast::TyI8)    => output.push_str("i8"),
        ty::TyInt(ast::TyI16)   => output.push_str("i16"),
        ty::TyInt(ast::TyI32)   => output.push_str("i32"),
        ty::TyInt(ast::TyI64)   => output.push_str("i64"),
        ty::TyUint(ast::TyUs)   => output.push_str("usize"),
        ty::TyUint(ast::TyU8)   => output.push_str("u8"),
        ty::TyUint(ast::TyU16)  => output.push_str("u16"),
        ty::TyUint(ast::TyU32)  => output.push_str("u32"),
        ty::TyUint(ast::TyU64)  => output.push_str("u64"),
        ty::TyFloat(ast::TyF32) => output.push_str("f32"),
        ty::TyFloat(ast::TyF64) => output.push_str("f64"),
        ty::TyStruct(adt_def, substs) |
        ty::TyEnum(adt_def, substs) => {
            push_item_name(cx, adt_def.did, qualified, output);
            push_type_params(cx, substs, output);
        },
        ty::TyTuple(ref component_types) => {
            output.push('(');
            for &component_type in component_types {
                push_unique_type_name(cx, component_type, true, output);
                output.push_str(", ");
            }
            if !component_types.is_empty() {
                output.pop();
                output.pop();
            }
            output.push(')');
        },
        ty::TyBox(inner_type) => {
            output.push_str("Box<");
            push_unique_type_name(cx, inner_type, true, output);
            output.push('>');
        },
        ty::TyRawPtr(ty::TypeAndMut { ty: inner_type, mutbl } ) => {
            output.push('*');
            match mutbl {
                hir::MutImmutable => output.push_str("const "),
                hir::MutMutable => output.push_str("mut "),
            }

            push_unique_type_name(cx, inner_type, true, output);
        },
        ty::TyRef(_, ty::TypeAndMut { ty: inner_type, mutbl }) => {
            output.push('&');
            if mutbl == hir::MutMutable {
                output.push_str("mut ");
            }

            push_unique_type_name(cx, inner_type, true, output);
        },
        ty::TyArray(inner_type, len) => {
            output.push('[');
            push_unique_type_name(cx, inner_type, true, output);
            output.push_str(&format!("; {}", len));
            output.push(']');
        },
        ty::TySlice(inner_type) => {
            output.push('[');
            push_unique_type_name(cx, inner_type, true, output);
            output.push(']');
        },
        ty::TyTrait(ref trait_data) => {
            let principal = cx.tcx().erase_late_bound_regions(&trait_data.principal);
            push_item_name(cx, principal.def_id, false, output);
            push_type_params(cx, principal.substs, output);
        },
        ty::TyBareFn(_, &ty::BareFnTy{ unsafety, abi, ref sig } ) => {
            if unsafety == hir::Unsafety::Unsafe {
                output.push_str("unsafe ");
            }

            if abi != ::syntax::abi::Rust {
                output.push_str("extern \"");
                output.push_str(abi.name());
                output.push_str("\" ");
            }

            output.push_str("fn(");

            let sig = cx.tcx().erase_late_bound_regions(sig);
            if !sig.inputs.is_empty() {
                for &parameter_type in &sig.inputs {
                    push_unique_type_name(cx, parameter_type, true, output);
                    output.push_str(", ");
                }
                output.pop();
                output.pop();
            }

            if sig.variadic {
                if !sig.inputs.is_empty() {
                    output.push_str(", ...");
                } else {
                    output.push_str("...");
                }
            }

            output.push(')');

            match sig.output {
                ty::FnConverging(result_type) if result_type.is_nil() => {}
                ty::FnConverging(result_type) => {
                    output.push_str(" -> ");
                    push_unique_type_name(cx, result_type, true, output);
                }
                ty::FnDiverging => {
                    output.push_str(" -> !");
                }
            }
        },
        ty::TyClosure(..) => {
            output.push_str("closure");
        }
        ty::TyError |
        ty::TyInfer(_) |
        ty::TyProjection(..) |
        ty::TyParam(_) => {
            cx.sess().bug(&format!("debuginfo: Trying to create type name for \
                unexpected type: {:?}", t));
        }
    }

    fn push_item_name(ccx: &CrateContext,
                      def_id: DefId,
                      qualified: bool,
                      output: &mut String) {
        let def_path = ccx.tcx().def_path(def_id);

        if qualified {
            if def_id.is_local() {
                output.push_str(&ccx.link_meta().crate_name);
                output.push_str("::");
            }

            for part in ccx.tcx().def_path(def_id) {
                output.push_str(&format!("{}[{}]::",
                                part.data.as_interned_str(),
                                part.disambiguator));
            }

            output.pop();
            output.pop();
        }
        else {
            let name = def_path.last()
                               .expect("debuginfo: Empty item path?");
            output.push_str(&name.data.as_interned_str());
        }
    }
}

// Pushes the type parameters in the given `Substs` to the output string.
// This ignores region parameters, since they can't reliably be
// reconstructed for items from non-local crates. For local crates, this
// would be possible but with inlining and LTO we have to use the least
// common denominator - otherwise we would run into conflicts.
fn push_type_params<'a, 'tcx>(cx: &CrateContext<'a, 'tcx>,
                              substs: &Substs<'tcx>,
                              output: &mut String) {
    if substs.types.is_empty() {
        return;
    }

    output.push('<');

    for &type_parameter in &substs.types {
        push_unique_type_name(cx, type_parameter, true, output);
        output.push_str(", ");
    }

    output.pop();
    output.pop();

    output.push('>');
}

fn push_def_id_as_string<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                              def_id: DefId,
                              substs: Option<&Substs<'tcx>>,
                              output: &mut String) {
    if def_id.is_local() {
        let node_id = ccx.tcx().map.as_local_node_id(def_id).unwrap();
        let inlined_from = ccx.external_srcs()
                              .borrow()
                              .get(&node_id)
                              .map(|&def_id| def_id);

        if let Some(def_id) = inlined_from {
            push_def_id_as_string(ccx, def_id, substs, output);
            return;
        }

        output.push_str(&ccx.link_meta().crate_name);
        output.push_str("::");
    }

    for part in ccx.tcx().def_path(def_id) {
        output.push_str(&format!("{}[{}]::",
                        part.data.to_string(),
                        part.disambiguator));
    }

    output.pop();
    output.pop();

    if let Some(substs) = substs {
        push_type_params(ccx, substs, output);
    }
}

fn def_id_to_string<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                              def_id: DefId,
                              substs: Option<&Substs<'tcx>>)
                              -> String {
    let mut output = String::new();
    push_def_id_as_string(ccx, def_id, substs, &mut output);
    output
}

fn type_to_string<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                            ty: ty::Ty<'tcx>)
                            -> String {
    let mut output = String::new();
    push_unique_type_name(ccx, ty, true, &mut output);
    output
}

impl<'tcx> CodeGenItem<'tcx> {

    pub fn to_string<'a>(&self, ccx: &CrateContext<'a, 'tcx>) -> String {
        let hir_map = &ccx.tcx().map;

        return match *self {
            CodeGenItem::DropGlue(t) => {
                let mut s = String::with_capacity(32);
                s.push_str("drop-glue ");
                push_unique_type_name(ccx, t, true, &mut s);
                s
            }
            CodeGenItem::Fn(node_id, ref substs) => {
                let def_id = hir_map.local_def_id(node_id);
                to_string_internal(ccx, "fn ", def_id, Some(substs))
            },
            CodeGenItem::Const(node_id) => {
                let def_id = hir_map.local_def_id(node_id);
                to_string_internal(ccx, "const ", def_id, None)
            },
            CodeGenItem::Static(node_id) => {
                let def_id = hir_map.local_def_id(node_id);
                to_string_internal(ccx, "static ", def_id, None)
            },
        };

        fn to_string_internal<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                                        prefix: &str,
                                        def_id: DefId,
                                        substs: Option<&Substs<'tcx>>)
                                        -> String {
            let mut result = String::with_capacity(32);
            result.push_str(prefix);
            push_def_id_as_string(ccx, def_id, substs, &mut result);
            result
        }
    }
}

pub fn collect_crate_codegen_items<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>)
                                             -> FnvHashSet<CodeGenItem<'tcx>> {
    let roots = collect_roots(ccx);
    debug!("Starting to collect from roots");
    let mut visited = FnvHashSet();
    for root in roots {
        collect_items_rec(ccx, root, &mut visited);
    }

    return visited;
}

// Find all non-generic items by walking the HIR. These items serve as roots to
// start monomorphizing from.
fn collect_roots<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>)
                           -> Vec<CodeGenItem<'tcx>> {
    debug!("Collecting roots");
    let mut roots = Vec::new();

    {
        let mut visitor = RootCollector {
            ccx: ccx,
            output: &mut roots,
            enclosing_item: None,
            trans_empty_substs: ccx.tcx().mk_substs(Substs::trans_empty()),
        };

        ccx.tcx().map.krate().visit_all_items(&mut visitor);
    }

    roots
}

// Collect all monomorphized codegen-items reachable from `starting_point`
fn collect_items_rec<'a, 'tcx: 'a>(ccx: &CrateContext<'a, 'tcx>,
                                   starting_point: CodeGenItem<'tcx>,
                                   visited: &mut FnvHashSet<CodeGenItem<'tcx>>) {
    if !visited.insert(starting_point.clone()) {
        // We have been here already, no need to search again.
        return;
    }
    debug!("BEGIN collect_items_rec({})", starting_point.to_string(ccx));
    debug!("Determining neighbours:");
    // TODO: avoid this allocation?
    let mut neighbours = Vec::new();

    match starting_point {
        CodeGenItem::DropGlue(_) |
        CodeGenItem::Const(_) |
        CodeGenItem::Static(_) => {}
        CodeGenItem::Fn(node_id, ref param_substs) => {
            // Scan the MIR in order to find function calls, closures,
            // and drop-glue
            let mir_not_found_error_message = || {
                format!("Could not find MIR for node_id: {}",
                        ccx.tcx().map.node_to_string(node_id))
            };

            let external_mir = ccx.external_srcs()
                                  .borrow()
                                  .get(&node_id)
                                  .map(|did| ccx.sess().cstore.maybe_get_item_mir(ccx.tcx(), *did))
                                  .unwrap_or(None);

            let mir_opt = match external_mir {
                Some(ref mir) => {
                    Some(mir)
                }
                None => {
                    ccx.mir_map().get(&node_id)
                }
            };

            let mir = session::expect(ccx.sess(),
                                      mir_opt,
                                      mir_not_found_error_message);

            let mut visitor = MirNeighborCollector {
                ccx: ccx,
                mir: mir,
                output: &mut neighbours,
                param_substs: param_substs,
                starting_point: starting_point,
            };

            visitor.visit_mir(mir);
        }
    }

    debug!("collecting neighbours of {}", starting_point.to_string(ccx));
    for neighbour in neighbours {
        collect_items_rec(ccx, neighbour, visited);
    }
    debug!("END collect_items_rec({})", starting_point.to_string(ccx));
}

struct MirNeighborCollector<'a, 'tcx: 'a> {
    ccx: &'a CrateContext<'a, 'tcx>,
    mir: &'a mir::Mir<'tcx>,
    output: &'a mut Vec<CodeGenItem<'tcx>>,
    param_substs: &'tcx Substs<'tcx>,
    starting_point: CodeGenItem<'tcx>
}

impl<'a, 'tcx> MirVisitor<'tcx> for MirNeighborCollector<'a, 'tcx> {

   fn visit_terminator(&mut self,
                       _: mir::BasicBlock,
                       terminator: &mir::Terminator<'tcx>) {
        let (callee_def_id, callee_substs) = match *terminator {
            mir::Terminator::Call { ref data, .. } => {
                match data.func {
                    mir::Operand::Consume(_) => {
                        // Callable lvalues like closures have to be constructed
                        // somewhere, which is how we track resulting codegen
                        // items for them.
                        return
                    }
                    mir::Operand::Constant(mir::Constant { ref literal, .. }) => {
                        match *literal {
                            mir::Literal::Item { def_id, substs } => {
                                (def_id, substs)
                            }
                            mir::Literal::Value { .. } => {
                                // Constant values should not occur as callees.
                                unreachable!();
                            }
                        }
                    }
                }
            }
            mir::Terminator::Goto { .. } |
            mir::Terminator::Panic { .. } |
            mir::Terminator::If { .. } |
            mir::Terminator::Switch { .. } |
            mir::Terminator::SwitchInt { .. } |
            mir::Terminator::Diverge |
            mir::Terminator::Return => {
                // These don't introduce new monomorphizations
                return
            }
        };

        debug!("found neighbor of {}:", self.starting_point.to_string(self.ccx));
        if let Some(codegen_item) = create_fn_codegen_item(self.ccx,
                                                           callee_def_id,
                                                           callee_substs,
                                                           self.param_substs) {
            self.output.push(codegen_item);
        }
    }

    fn visit_rvalue(&mut self, rvalue: &mir::Rvalue<'tcx>) {
        match *rvalue {
            mir::Rvalue::Aggregate(mir::AggregateKind::Closure(def_id, ref substs), _) => {
                debug!("found neighbor of {}:", self.starting_point.to_string(self.ccx));
                if let Some(codegen_item) = create_inlined_fn_codegen_item(self.ccx,
                                                                           def_id,
                                                                           substs.func_substs,
                                                                           self.param_substs) {
                    self.output.push(codegen_item);
                }
            }
            _ => { /* not interesting */ }
        }

        self.super_rvalue(rvalue);
    }

    fn visit_lvalue(&mut self,
                    lvalue: &mir::Lvalue<'tcx>,
                    context: mir_visit::LvalueContext) {
        if let mir_visit::LvalueContext::Drop = context {
            let ty = self.mir.lvalue_ty(self.ccx.tcx(), lvalue)
                             .to_ty(self.ccx.tcx());

            let ty = monomorphize::apply_param_substs(self.ccx.tcx(),
                                                      self.param_substs,
                                                      &ty);
            let ty = self.ccx.tcx().erase_regions(&ty);

            debug!("found neighbor of {}:", self.starting_point.to_string(self.ccx));
            create_drop_glue_codegen_items(self.ccx,
                                           ty,
                                           self.param_substs,
                                           &mut self.output);
        }

        self.super_lvalue(lvalue, context);
    }
}

fn create_drop_glue_codegen_items<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                                            mono_ty: ty::Ty<'tcx>,
                                            param_substs: &'tcx Substs<'tcx>,
                                            output: &mut Vec<CodeGenItem<'tcx>>)
{
    visit_types_of_owned_components(ccx, mono_ty, &mut |ty| {
        debug!("create_drop_glue_codegen_items: {}", type_to_string(ccx, ty));
        if type_needs_drop(ccx.tcx(), ty) {
            debug!(" -> needs drop");
            // Add a codegen-item for the drop glue
            output.push(CodeGenItem::DropGlue(ty));

            // If the type implements Drop, also add a codegen-item for the
            // monomorphized Drop::drop() implementation.
            let destructor_did = match ty.sty {
                ty::TyStruct(def, _) |
                ty::TyEnum(def, _)   => def.destructor(),
                _ => None
            };

            if let Some(destructor_did) = destructor_did {
                use rustc::middle::ty::ToPolyTraitRef;

                let drop_trait_def_id = ccx.tcx()
                                           .lang_items
                                           .drop_trait()
                                           .unwrap();

                let self_type_substs = ccx.tcx().mk_substs(
                    Substs::trans_empty().with_self_ty(ty));

                let trait_ref = ty::TraitRef {
                    def_id: drop_trait_def_id,
                    substs: self_type_substs,
                }.to_poly_trait_ref();

                let vtbl = match fulfill_obligation(ccx, DUMMY_SP, trait_ref) {
                    traits::VtableImpl(data) => data,
                    _ => unreachable!()
                };

                if let Some(cg_item) = create_fn_codegen_item(ccx,
                                                              destructor_did,
                                                              ccx.tcx().mk_substs(vtbl.substs),
                                                              param_substs) {
                    output.push(cg_item);
                };
            }
        }
    });
}

// Returns the codegen item for the monomorphic, possibly inlined function
// specified by `fn_def_id`. Monomorphization is done by binding free variables
// in `fn_substs` with values from `param_substs`.
fn create_fn_codegen_item<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                                    fn_def_id: DefId,
                                    fn_substs: &'tcx Substs<'tcx>,
                                    param_substs: &'tcx Substs<'tcx>)
                                    -> Option<CodeGenItem<'tcx>> {
    let dispatched = {
        let impl_or_trait_items_map = ccx.tcx().impl_or_trait_items.borrow();
        match impl_or_trait_items_map.get(&fn_def_id) {
            Some(&ty::MethodTraitItem(ref method)) => {
                match method.container {
                    ty::TraitContainer(trait_def_id) => {
                        do_static_trait_method_dispatch(ccx,
                                                        method,
                                                        trait_def_id,
                                                        fn_substs,
                                                        param_substs)
                    }
                    ty::ImplContainer(_) => {
                        // This is already a concrete implementation
                        Some((fn_def_id, fn_substs))
                    }
                }
            }
            Some(_) => {
                // TODO: better error message
                unreachable!()
            }
            None => {
                // The function is not part of an impl or trait, no dispatching
                // to be done
                Some((fn_def_id, fn_substs))
            }
        }
    };

    if let Some((fn_def_id, fn_substs)) = dispatched {
        create_inlined_fn_codegen_item(ccx, fn_def_id, fn_substs, param_substs)
    }
    else {
        None
    }
}

// Given a trait-method and substitution information, find out the actual
// implementation of the trait method.
// TODO: This code duplicates quite a bit of logic from
//       trans::meth::trans_static_method_callee(). Maybe later, this could be
//       refactored so that this method stores the computed dispatching
//       information somewhere so it only has to be looked up later.
fn do_static_trait_method_dispatch<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                                             trait_method: &ty::Method,
                                             trait_id: DefId,
                                             callee_substs: &'tcx Substs<'tcx>,
                                             param_substs: &'tcx Substs<'tcx>)
                                             -> Option<(DefId, &'tcx Substs<'tcx>)> {
    let tcx = ccx.tcx();

    debug!("do_static_trait_method_dispatch(trait_method={}, trait_id={}, \
            callee_substs={:?}, param_substs={:?}",
           def_id_to_string(ccx, trait_method.def_id, None),
           def_id_to_string(ccx, trait_id, None),
           callee_substs,
           param_substs);

    // Find the substitutions for the fn itself. This includes
    // type parameters that belong to the trait but also some that
    // belong to the method:
    let rcvr_substs = monomorphize::apply_param_substs(tcx,
                                                       param_substs,
                                                       callee_substs);
    let subst::SeparateVecsPerParamSpace {
        types: rcvr_type,
        selfs: rcvr_self,
        fns: rcvr_method
    } = rcvr_substs.types.split();

    // Lookup the precise impl being called. To do that, we need to
    // create a trait reference identifying the self type and other
    // input type parameters. To create that trait reference, we have
    // to pick apart the type parameters to identify just those that
    // pertain to the trait. This is easiest to explain by example:
    //
    //     trait Convert {
    //         fn from<U:Foo>(n: U) -> Option<Self>;
    //     }
    //     ...
    //     let f = <Vec<int> as Convert>::from::<String>(...)
    //
    // Here, in this call, which I've written with explicit UFCS
    // notation, the set of type parameters will be:
    //
    //     rcvr_type: [] <-- nothing declared on the trait itself
    //     rcvr_self: [Vec<int>] <-- the self type
    //     rcvr_method: [String] <-- method type parameter
    //
    // So we create a trait reference using the first two,
    // basically corresponding to `<Vec<int> as Convert>`.
    // The remaining type parameters (`rcvr_method`) will be used below.
    let trait_substs = Substs::erased(subst::VecPerParamSpace::new(rcvr_type,
                                                                   rcvr_self,
                                                                   Vec::new()));
    let trait_substs = tcx.mk_substs(trait_substs);
    debug!("  - trait_substs={:?}", trait_substs);
    let trait_ref = ty::Binder(ty::TraitRef::new(trait_id, trait_substs));
    let vtbl = fulfill_obligation(ccx, DUMMY_SP, trait_ref);

    // Now that we know which impl is being used, we can dispatch to
    // the actual function:
    match vtbl {
        traits::VtableImpl(traits::VtableImplData {
            impl_def_id: impl_did,
            substs: impl_substs,
            nested: _ }) =>
        {
            use rustc::middle::ty::HasTypeFlags;
            assert!(!impl_substs.types.needs_infer());

            // Create the substitutions that are in scope. This combines
            // the type parameters from the impl with those declared earlier.
            // To see what I mean, consider a possible impl:
            //
            //    impl<T> Convert for Vec<T> {
            //        fn from<U:Foo>(n: U) { ... }
            //    }
            //
            // Recall that we matched `<Vec<int> as Convert>`. Trait
            // resolution will have given us a substitution
            // containing `impl_substs=[[T=int],[],[]]` (the type
            // parameters defined on the impl). We combine
            // that with the `rcvr_method` from before, which tells us
            // the type parameters from the *method*, to yield
            // `callee_substs=[[T=int],[],[U=String]]`.
            let subst::SeparateVecsPerParamSpace {
                types: impl_type,
                selfs: impl_self,
                fns: _
            } = impl_substs.types.split();
            let callee_substs =
                Substs::erased(subst::VecPerParamSpace::new(impl_type,
                                                            impl_self,
                                                            rcvr_method));
            let impl_method = tcx.get_impl_method(impl_did,
                                                  callee_substs,
                                                  trait_method.name);

            Some((impl_method.method.def_id, tcx.mk_substs(impl_method.substs)))
        }
        traits::VtableObject(ref _data) => {
            None
        }
        _ => {
            tcx.sess.bug(&format!("static call to invalid vtable: {:?}",
                                 vtbl));
        }
    }
}

// Creates a codegen_item for the function with `fn_def_id`. If the given DefId
// is from an external crate, an inline-instance of the definition will be
// created before moving on.
// This function returns `None` for some things that are callable
// (e.g. constructors) but for which no LLVM item needs to be created.
fn create_inlined_fn_codegen_item<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                                            fn_def_id: DefId,
                                            fn_substs: &'tcx Substs<'tcx>,
                                            param_substs: &'tcx Substs<'tcx>)
                                            -> Option<CodeGenItem<'tcx>> {
    return inline::get_local_instance(ccx, fn_def_id).and_then(|local_def_id| {
        let node_id = ccx.tcx().map.as_local_node_id(local_def_id).unwrap();
        if ignored(ccx.tcx().map.get(node_id)) {
            return None;
        }

        debug!("create_inlined_fn_codegen_item(fn_def_id={}, \
                                               fn_substs={:?}, \
                                               param_substs={:?})",
               def_id_to_string(ccx, fn_def_id, None),
               fn_substs,
               param_substs);

        // We only get here, if fn_def_id either designates a local item or
        // an inlineable external item. Non-inlineable external items are
        // ignored because we don't want to generate any code for them.
        let concrete_substs = monomorphize::apply_param_substs(ccx.tcx(),
                                                               param_substs,
                                                               fn_substs);
        let concrete_substs = ccx.tcx().erase_regions(&concrete_substs);
        let codegen_item = CodeGenItem::Fn(node_id,
                                           ccx.tcx().mk_substs(concrete_substs));
        Some(codegen_item)
    });

    fn ignored(node: hir_map::Node) -> bool {
        match node {
            hir_map::NodeItem(&hir::Item { node: hir::ItemFn(..), .. } ) |
            hir_map::NodeTraitItem(&hir::TraitItem { node: hir::MethodTraitItem(..), .. }) |
            hir_map::NodeImplItem(&hir::ImplItem { node: hir::ImplItemKind::Method(..), .. }) |
            hir_map::NodeExpr(&hir::Expr { node: hir::ExprClosure(..), .. }) => false,

            _ => true
        }
    }
}

struct RootCollector<'b, 'a: 'b, 'tcx: 'a + 'b> {
    ccx: &'b CrateContext<'a, 'tcx>,
    output: &'b mut Vec<CodeGenItem<'tcx>>,
    enclosing_item: Option<&'tcx hir::Item>,
    trans_empty_substs: &'tcx Substs<'tcx>
}

impl<'b, 'a, 'v> hir_visit::Visitor<'v> for RootCollector<'b, 'a, 'v> {
    fn visit_item(&mut self, item: &'v hir::Item) {
        let old_enclosing_item = self.enclosing_item;
        self.enclosing_item = Some(item);

        match item.node {
            hir::ItemExternCrate(..) |
            hir::ItemUse(..)         |
            hir::ItemForeignMod(..)  |
            hir::ItemTy(..)          |
            hir::ItemDefaultImpl(..) |
            hir::ItemTrait(..)       |
            hir::ItemMod(..)         => {
                // Just keep recursing
            }

            hir::ItemImpl(..) => {
                create_codegen_items_for_default_impls(self.ccx,
                                                       item,
                                                       self.trans_empty_substs,
                                                       self.output);
            }

            hir::ItemEnum(_, ref generics)        |
            hir::ItemStruct(_, ref generics)      => {
                if !generics.is_parameterized() {
                    let ty = {
                        let tables = self.ccx.tcx().tables.borrow();
                        tables.node_types[&item.id]
                    };

                    if type_needs_drop(self.ccx.tcx(), ty) {
                        debug!("RootCollector: ADT drop-glue for {}",
                               def_id_to_string(self.ccx,
                                                self.ccx.tcx().map.local_def_id(item.id),
                                                None));

                        create_drop_glue_codegen_items(self.ccx,
                                                       ty,
                                                       self.trans_empty_substs,
                                                       self.output);
                    }
                }
            }
            hir::ItemStatic(..) => {
                debug!("RootCollector: ItemStatic({})",
                       def_id_to_string(self.ccx,
                                        self.ccx.tcx().map.local_def_id(item.id),
                                        None));
                self.output.push(CodeGenItem::Static(item.id));
            }
            hir::ItemConst(..) => {
                debug!("RootCollector: ItemConst({})",
                       def_id_to_string(self.ccx,
                                        self.ccx.tcx().map.local_def_id(item.id),
                                        None));
                self.output.push(CodeGenItem::Const(item.id));
            }
            hir::ItemFn(_, _, _, _, ref generics, _) => {
                if !generics.is_parameterized() {
                    debug!("RootCollector: ItemFn({})",
                           def_id_to_string(self.ccx,
                                            self.ccx.tcx().map.local_def_id(item.id),
                                            None));
                    self.output.push(CodeGenItem::Fn(item.id,
                                                     self.trans_empty_substs));
                }
            }
        }

        hir_visit::walk_item(self, item);
        self.enclosing_item = old_enclosing_item;
    }

    fn visit_impl_item(&mut self, ii: &'v hir::ImplItem) {
        match ii.node {
            hir::ImplItemKind::Method(hir::MethodSig { ref generics, .. }, _) => {
                let hir_map = &self.ccx.tcx().map;
                let parent_node_id = hir_map.get_parent_node(ii.id);
                let is_impl_generic = match hir_map.expect_item(parent_node_id) {
                    &hir::Item {
                        node: hir::ItemImpl(_, _, ref generics, _, _, _),
                        ..
                    } => {
                        generics.is_parameterized()
                    }
                    _ => {
                        unreachable!()
                    }
                };

                if !generics.is_parameterized() && !is_impl_generic {
                    debug!("RootCollector: MethodImplItem({})",
                           def_id_to_string(self.ccx,
                                            self.ccx.tcx().map.local_def_id(ii.id),
                                            None));
                    self.output.push(CodeGenItem::Fn(ii.id, self.trans_empty_substs));
                }
            }
            _ => { /* Nothing to do here */ }
        }

        hir_visit::walk_impl_item(self, ii)
    }
}

fn create_codegen_items_for_default_impls<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                                                    item: &'tcx hir::Item,
                                                    trans_empty_substs: &'tcx Substs<'tcx>,
                                                    output: &mut Vec<CodeGenItem<'tcx>>) {
    match item.node {
        hir::ItemImpl(_,
                      _,
                      ref generics,
                      _,
                      _,
                      ref items) => {
            if generics.is_parameterized() {
                return
            }

            let tcx = ccx.tcx();
            let impl_def_id = tcx.map.local_def_id(item.id);

            debug!("create_codegen_items_for_default_impls(item={})",
                   def_id_to_string(ccx, impl_def_id, None));

            // TODO: Clean this up
            if let Some(trait_ref) = tcx.impl_trait_ref(impl_def_id) {
                debug!("  trait_ref={}",
                       def_id_to_string(ccx, trait_ref.def_id, Some(trait_ref.substs)));
                let default_impls = tcx.provided_trait_methods(trait_ref.def_id);
                let callee_substs = tcx.mk_substs(tcx.erase_regions(trait_ref.substs));
                debug!("  impl items:");
                for impl_item in items {
                    let did = tcx.map.local_def_id(impl_item.id);
                    debug!("  - {}", def_id_to_string(ccx, did, Some(callee_substs)));
                }

                debug!("  default_impls:");
                for default_impl in &default_impls {
                    debug!("  - {}", def_id_to_string(ccx, default_impl.def_id, Some(callee_substs)));
                }

                let overridden_methods: FnvHashSet<_> = items.iter()
                                                             .map(|item| item.name)
                                                             .collect();

                for default_impl in default_impls {
                    if overridden_methods.contains(&default_impl.name) {
                        debug!("  filtered b/c of name: {}", def_id_to_string(ccx, default_impl.def_id, Some(callee_substs)));
                        continue;
                    }

                    if default_impl.generics.has_type_params(subst::FnSpace) {
                        debug!("  filtered b/c of generics: {}", def_id_to_string(ccx, default_impl.def_id, Some(callee_substs)));
                        continue;
                    }

                    // if default_impl.generics.types.is_empty() &&
                    //    items.iter().all(|item| item.name != default_impl.name)
                    {

                        debug!("  instantiating: {}", def_id_to_string(ccx, default_impl.def_id, Some(callee_substs)));

                        if let Some(codegen_item) =
                            create_inlined_fn_codegen_item(ccx,
                                                           default_impl.def_id,
                                                           callee_substs,
                                                           trans_empty_substs) {
                            debug!("    -> pushed");
                            output.push(codegen_item);
                        }
                        else {
                            debug!("    -> filtered out during inlining");
                        }
                    }
                }
            }
        }
        _ => {
            unreachable!()
        }
    }
}

pub fn visit_types_of_owned_components<'a, 'tcx, F>(ccx: &CrateContext<'a, 'tcx>,
                                                    t: ty::Ty<'tcx>,
                                                    mut f: &mut F)
    where F: FnMut(ty::Ty<'tcx>)
{
    f(t);

    match t.sty {
        ty::TyBool       |
        ty::TyChar       |
        ty::TyInt(_)     |
        ty::TyUint(_)    |
        ty::TyStr        |
        ty::TyFloat(_)   |
        ty::TyRawPtr(_)  |
        ty::TyRef(..)    |
        ty::TyBareFn(..) |
        ty::TySlice(_)   |
        ty::TyTrait(_)   => {
            /* nothing to do */
        }
        ty::TyStruct(ref adt_def, substs) |
        ty::TyEnum(ref adt_def, substs) => {
            for field in adt_def.all_fields() {
                let field_type = monomorphize::apply_param_substs(ccx.tcx(),
                                                                  substs,
                                                                  &field.unsubst_ty());
                visit_types_of_owned_components(ccx, field_type, f);
            }
        }
        ty::TyClosure(_, ref substs) => {
            for upvar_ty in &substs.upvar_tys {
                visit_types_of_owned_components(ccx, upvar_ty, f);
            }
        }
        ty::TyBox(inner_type)      |
        ty::TyArray(inner_type, _) => {
            visit_types_of_owned_components(ccx, inner_type, f);
        }
        ty::TyTuple(ref args) => {
            for arg in args {
                visit_types_of_owned_components(ccx, arg, f);
            }
        }
        ty::TyProjection(_) |
        ty::TyParam(_)      |
        ty::TyInfer(_)      |
        ty::TyError         => {
            ccx.sess().bug("encountered unexpected type");
        }
    }
}
