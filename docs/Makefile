#!/usr/bin/env make
# license: gnu gpl version 3 or higher.
# copyright 2016 rsiddharth <s@ricketyspace.net>

build_dir=_build

# docs
header=html/header.html
footer=html/footer.html

index_src=../README
index_out=$(build_dir)/index.html

install_guile_src=install-guile.md
install_guile_dir=$(build_dir)/install-guile
install_guile_out=$(install_guile_dir)/index.html

nothing:
	@echo "Give me something to make. "

html: $(index_out) $(install_guile_out)
.PHONY: html

$(build_dir):
	@mkdir $@

$(index_out): $(index_src) $(header) $(footer) $(build_dir)
	@cat $(header) > $@
	./bin/markdown $(index_src) >> $@
	@cat $(footer) >> $@

$(install_guile_dir):
	@mkdir -p $@

$(install_guile_out): $(install_guile_src) $(header) $(footer) \
	$(install_guile_dir)
	@cat $(header) > $@
	./bin/markdown $(install_guile_src) >> $@
	@cat $(footer) >> $@

clean: $(build_dir)
	@rm -rf $(build_dir)
.PHONY: clean
