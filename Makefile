.PHONY: dev sass build dist

dev:
	cd web && python3 -m http.server 9000

sass:
	sass --watch src/stylesheets:web --no-source-map

build:
	spago bundle-app --to web/app.js

dist:
	tools/dist.sh
