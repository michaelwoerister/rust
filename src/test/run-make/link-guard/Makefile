-include ../tools.mk

all:
	mkdir -p $(TMPDIR)/v1
	mkdir -p $(TMPDIR)/v2
	$(BARE_RUSTC) ./v1/lib.rs -C prefer-dynamic --out-dir="$(TMPDIR)/v1"
	$(BARE_RUSTC) ./v2/lib.rs -C prefer-dynamic --out-dir="$(TMPDIR)/v2"
	$(BARE_RUSTC) -L "$(TMPDIR)/v1" -C prefer-dynamic ./main.rs --out-dir="$(TMPDIR)"
	$(LD_LIB_PATH_ENVVAR)="$(TMPDIR)/v1:$(TARGET_RPATH_DIR):$($(LD_LIB_PATH_ENVVAR))" $(TMPDIR)/main
	# $(LD_LIB_PATH_ENVVAR)="$(TMPDIR)/v2:$(TARGET_RPATH_DIR):$($(LD_LIB_PATH_ENVVAR))" $(TMPDIR)/main
	# $(LD_LIB_PATH_ENVVAR)="$(TARGET_RPATH_DIR):$($(LD_LIB_PATH_ENVVAR))" $(TMPDIR)/main
