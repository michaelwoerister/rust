
struct Struct<T> {
    x: T,
    f: fn(x: T) -> T,
}

fn id<T>(x: T) -> T { x }

impl<T> Struct<T> {

    fn new(x: T) -> Struct<T> {
        Struct {
            x: x,
            f: id
        }
    }

    fn get<T2>(self, x: T2) -> (T, T2) {
        (self.x, x)
    }
}

pub struct LifeTimeOnly<'a> {
    a: &'a u32
}

impl<'a> LifeTimeOnly<'a> {

    //~ CODEGEN_ITEM fn generic_impl::LifeTimeOnly<'a>[0]::foo[0]
    pub fn foo(&self) {}
    //~ CODEGEN_ITEM fn generic_impl::LifeTimeOnly<'a>[0]::bar[0]
    pub fn bar(&'a self) {}
    //~ CODEGEN_ITEM fn generic_impl::LifeTimeOnly<'a>[0]::baz[0]
    pub fn baz<'b>(&'b self) {}

    pub fn non_instantiated<T>(&self) {}
}


//~ CODEGEN_ITEM fn generic_impl::main[0]
fn main() {
    //~ CODEGEN_ITEM fn generic_impl::Struct<T>[0]::new[0]<i32>
    //~ CODEGEN_ITEM fn generic_impl::id[0]<i32>
    //~ CODEGEN_ITEM fn generic_impl::Struct<T>[0]::get[0]<i32, i16>
    let _ = Struct::new(0i32).get(0i16);

    //~ CODEGEN_ITEM fn generic_impl::Struct<T>[0]::new[0]<i64>
    //~ CODEGEN_ITEM fn generic_impl::id[0]<i64>
    //~ CODEGEN_ITEM fn generic_impl::Struct<T>[0]::get[0]<i64, i16>
    let _ = Struct::new(0i64).get(0i16);

    //~ CODEGEN_ITEM fn generic_impl::Struct<T>[0]::new[0]<char>
    //~ CODEGEN_ITEM fn generic_impl::id[0]<char>
    //~ CODEGEN_ITEM fn generic_impl::Struct<T>[0]::get[0]<char, i16>
    let _ = Struct::new('c').get(0i16);

    //~ CODEGEN_ITEM fn generic_impl::Struct<T>[0]::new[0]<&str>
    //~ CODEGEN_ITEM fn generic_impl::id[0]<&str>
    //~ CODEGEN_ITEM fn generic_impl::Struct<T>[0]::get[0]<generic_impl::Struct[0]<&str>, i16>
    let _ = Struct::new(Struct::new("str")).get(0i16);

    //~ CODEGEN_ITEM fn generic_impl::Struct<T>[0]::new[0]<generic_impl::Struct[0]<&str>>
    //~ CODEGEN_ITEM fn generic_impl::id[0]<generic_impl::Struct[0]<&str>>
    let _ = (Struct::new(Struct::new("str")).f)(Struct::new("str"));
}
