.PHONY: site clean watch watch-pandoc watch-browser-sync

# The bootstrap distribution is shipped with this template
framework := bootstrap
framework_dir := bootstrap-4.4.1-dist

# Arguments to Pandoc; these are reasonable defaults
pandoc_args += --template style/template.html
pandoc_args += --css css/$(framework).css
pandoc_args += --css css/mods.css
pandoc_args += -t html5 -s --mathjax --toc
pandoc_args += --toc-depth 1
pandoc_args += --filter pandoc-bootstrap
pandoc_args += -f markdown+multiline_tables+simple_tables

# Load syntax definitions for languages that are not supported
# by default. These XML files are in the format of the Kate editor.
# pandoc_args += --syntax-definition style/elm.xml
# pandoc_args += --syntax-definition style/pure.xml

# Any file in the `lit` directory that is not a Markdown source 
# is to be copied to the `docs` directory
static_files := $(shell find lit -type f -not -name '*.md')
static_targets := $(static_files:lit/%=docs/%)

lit_source := ./lit
lit_files := $(wildcard $(lit_source)/*.md)
pages := $(patsubst $(lit_source)/%.md, docs/%.html,$(lit_files))

# This should build everything needed to generate your web site. That includes
# possible Javascript targets that may need compiling.
site: page style/template.html Makefile docs/css/$(framework).css docs/js/$(framework).js docs/css/mods.css \
      $(static_targets)

clean:
	rm -rf docs

# Starts a tmux with Entangled, Browser-sync and an Inotify loop for running
# Pandoc.
watch:
	@tmux new-session make --no-print-directory watch-pandoc \; \
		split-window -v make --no-print-directory watch-browser-sync \; \
		split-window -v entangled daemon \; \
		select-layout even-vertical \;

watch-pandoc:
	@while true; do \
		inotifywait -e close_write style lit Makefile style/*; \
		make site; \
	done

watch-browser-sync:
	browser-sync start -w -s docs

docs/css/$(framework).css: style/$(framework_dir)/css/$(framework).css
	@mkdir -p docs/css
	cp $< $@

docs/css/mods.css: style/mods.css
	@mkdir -p docs/css
	cp $< $@

docs/js/$(framework).js: style/$(framework_dir)/js/$(framework).js
	@mkdir -p docs/js
	cp $< $@

page:
	@mkdir -p docs
	pandoc $(pandoc_args) $(lit_files) -o docs/index.html

docs/%.html: $(lit_source)/%.md
	@mkdir -p docs
	pandoc $(pandoc_args) $< -o $@

$(static_targets): docs/%: lit/%
	@mkdir -p $(dir $@)
	cp $< $@

