#!/usr/bin/make
# license: gnu gpl version 3 or higher.
# copyright 2016 rsiddharth <s@ricketyspace.net>

guile=$(shell which guile)
bin=$(HOME)/.bin
git_difme=$(bin)/git-difme
git_difme_src=git-difme.scm

nothing:
	@echo "Give me something to make. "
.PHONY: nothing

git-difme: $(git_difme)
.PHONY: git-difme

$(git_difme): $(git_difme_src)
	@mkdir -p $(bin)
	@touch $@
	@echo "#!"$(guile)" \\ \n-e main -s\n!#" > $@
	@cat $< >> $@
	@chmod +x $@

	@echo "Script at" $@
	@echo "Add" $(bin) "to PATH"

