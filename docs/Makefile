#!/usr/bin/env make
# license: gnu gpl version 3 or higher.
# copyright 2016 rsiddharth <s@ricketyspace.net>

build_dir=_build

# docs
header=assets/includes/header.html
footer=assets/includes/footer.html

index_src=index.org
index_out=$(build_dir)/index.html

install_guile_src=install-guile.org
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
	@pandoc -f org -t html $< >> $@
	@cat $(footer) >> $@

$(install_guile_dir):
	@mkdir -p $@

$(install_guile_out): $(install_guile_src) $(header) $(footer) \
	$(install_guile_dir)
	@cat $(header) > $@
	@pandoc -f org -t html $< >> $@
	@cat $(footer) >> $@

clean: $(build_dir)
	@rm -rf $<

.PHONY: clean
