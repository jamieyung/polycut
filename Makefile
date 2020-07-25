.PHONY: clean dev sass build dist

SPAGO=PATH=$(PATH):./node_modules/.bin node_modules/.bin/spago
SASS=node_modules/.bin/sass

clean:
	rm -rf output
	rm -rf .spago
	rm web/app.css
	rm web/app.js

init:
	npm i
	$(SPAGO) install

dev:
	python3 -m http.server --directory web 9000

sass:
	$(SASS) --watch src/stylesheets:web --no-source-map

build:
	$(SPAGO) bundle-app --to web/app.js

dist:
	tools/dist.sh
