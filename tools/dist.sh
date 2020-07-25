# # run from the root dir

# clear dist dir ===============================================================

rm -rf dist

# build dist/app.js ============================================================

PATH=$PATH:./node_modules/.bin node_modules/.bin/spago bundle-app --to dist/app.js

# build dist/app.css ===========================================================

node_modules/.bin/sass src/stylesheets:dist --no-source-map

# post-process dist/app.css ====================================================

node_modules/.bin/postcss dist/app.css --use autoprefixer -o dist/app.css --no-map

# copy index.html to dist ======================================================

cp web/index.html dist

# minifiy dist/index.html ======================================================

node_modules/.bin/html-minifier dist/index.html --collapse-whitespace -o dist/index.html

# minify dist/app.js ===========================================================

node_modules/.bin/babel-minify dist/app.js -o dist/app.js --mangle --simplify --booleans --deadcode --builtIns --flipComparisons --memberExpressions --mergeVars --propertyLiterals

# minify dist/app.css ==========================================================

node_modules/.bin/cssnano dist/app.css dist/app.css

# do cache busting =============================================================

cache_bust()
{
    FILENAME=$1
    EXT=$2
    HASH=`md5sum dist/$FILENAME.$EXT | awk '{ print $1 }'`
    FILENAME_WITH_HASH=$FILENAME.$HASH.$EXT
    mv dist/$FILENAME.$EXT dist/$FILENAME_WITH_HASH
    sed -i "s/$FILENAME.$EXT/$FILENAME_WITH_HASH/g" dist/index.html
}
cache_bust app css
cache_bust app js
