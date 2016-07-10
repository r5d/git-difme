#!/usr/bin/make
# license: gnu gpl version 3 or higher.
# copyright 2016 rsiddharth <s@ricketyspace.net>

guile=$(shell which guile)

bin=$(HOME)/.bin

git_difme=$(bin)/git-difme
git_difme_src=git-difme.scm

config_dir = $(HOME)/.config/git-difme
config_file=$(config_dir)/config
example_config=examples/config

nothing:
	@echo "Give me something to make. "

.PHONY: nothing

git-difme: $(git_difme)

.PHONY: git-difme

config: $(config_file)

.PHONY: config

$(git_difme): $(git_difme_src)
	@mkdir -p $(bin)
	@touch $@
	@echo "#!"$(guile)" \\ \n-e main -s\n!#" > $@
	@cat $< >> $@
	@chmod +x $@

	@echo "Script at" $@
	@echo "Add" $(bin) "to PATH"

$(config_dir):
	@mkdir -p $@

$(config_file): $(example_config) $(config_dir)
	@cp -i $< $@
