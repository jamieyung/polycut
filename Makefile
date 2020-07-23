.PHONY: dev build watch

dev:
	cd web && python3 -m http.server 9000

build:
	spago bundle-app --to web/app.js

watch:
	spago bundle-app --to web/app.js -w
