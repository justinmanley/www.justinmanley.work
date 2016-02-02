#!/bin/bash

# exit script with nonzero exit code if any command fails
set -e

git checkout master

cabal install --only-dependencies --enable-tests
bower install

cabal test

cabal run site clean
cabal run site build

# add generated site to git 
sed --in-place '/site\/*/d' .gitignore
git add --all _site

# deploy to gh-pages branch.  See: 
#   http://www.damian.oquanta.info/posts/one-line-deployment-of-your-site-to-gh-pages.html
#   http://lukecod.es/2014/08/15/deploy-a-static-subdirectory-to-github-pages/
git commit -m "Generate website on $(date +"%m-%d-%Y") at $(date +"%H:%M:%S")."

# check out a new local branch containing the site/ directory
git subtree split --prefix _site -b gh-pages

# force push the gh-pages branch
git push -f origin gh-pages:gh-pages

