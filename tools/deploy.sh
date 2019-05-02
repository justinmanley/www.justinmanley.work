#!/bin/bash

# This script clones the repository from Github, downloads dependencies, builds the site, and then
# deploys the site to Github Pages. The site is deployed by pushing the contents of the
# _site directory to Github Pages.

# exit script with nonzero exit code if any command fails
set -e

# echo each command
set -x

DIR=$(mktemp -d)
git clone --depth=1 https://github.com/justinmanley/www.justinmanley.work.git "${DIR}"
cd "${DIR}"

npm install
bower install
stack build && stack run site build 

# add generated site to git 
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS uses standard sed by default, not GNU sed.
    sed -i '' '/_site\/*/d' .gitignore
else
    # On other operating systems, use GNU sed.
    sed --in-place '/_site\/*/d' .gitignore
fi
git add --all _site

# deploy to gh-pages branch.  See: 
#   http://www.damian.oquanta.info/posts/one-line-deployment-of-your-site-to-gh-pages.html
#   http://lukecod.es/2014/08/15/deploy-a-static-subdirectory-to-github-pages/
git commit -m "Generate website on $(date +"%m-%d-%Y") at $(date +"%H:%M:%S")."

# check out a new local branch containing the site/ directory
git subtree split --prefix _site -b gh-pages-staging

# Revert changes to .gitignore in order to move to the gh-pages branch.
git checkout .gitignore

# Make non-master branches available again after cloning with --depth=1.
git remote set-branches origin '*'  
git fetch origin gh-pages

git checkout --track origin/gh-pages
git cherry-pick $(git log gh-pages-staging -1 --pretty=oneline | awk '{ print $1 }') --strategy-option=theirs 
git push origin gh-pages

cd -
rm -rf "${DIR}"
