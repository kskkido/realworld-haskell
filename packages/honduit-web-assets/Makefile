default: all

.PHONY : all
all:
	npm i
	$(MAKE) scripts || exit
	$(MAKE) styles || exit

.PHONY: scripts
scripts:
	rm -rf ./output
	rm -rf ./public/scripts
	spago bundle-app --main Main --to ./public/scripts/index.js

.PHONY: styles
styles:
	npx postcss ./src/styles/index.css -d ./public/styles

.PHONY: watch
watch:
	find ./src -type f -name '*.purs' -o -name "*.css" | entr -r $(MAKE) build

