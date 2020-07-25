.PHONY: dev sass build dist

dev:
	cd web && python3 -m http.server 9000

sass:
	sass --watch src/stylesheets:web --no-source-map

build:
	spago bundle-app --to web/app.js

# For now, still have to manually do cache-busting.
# Put the current unix timestamp before .css and .js in the dist files, and update the references in index.html.
dist:
	rm -rf dist \
		&& spago bundle-app --to dist/app.js \
		&& sass src/stylesheets:dist --no-source-map \
		&& node_modules/.bin/postcss web/app.css --use autoprefixer -o dist/app.css --no-map \
		&& cp web/index.html dist \
		&& node_modules/.bin/babel-minify dist/app.js -o dist/app.js --mangle --simplify --booleans --deadcode --builtIns --flipComparisons --memberExpressions --mergeVars --propertyLiterals
