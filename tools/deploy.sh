#!/bin/bash

# exit script with nonzero exit code if any command fails
set -e

set -x

# add generated site to git 
sed --in-place '/_site\/*/d' .gitignore
git add --all _site

# deploy to gh-pages branch.  See: 
#   http://www.damian.oquanta.info/posts/one-line-deployment-of-your-site-to-gh-pages.html
#   http://lukecod.es/2014/08/15/deploy-a-static-subdirectory-to-github-pages/
git commit -m "Generate website on $(date +"%m-%d-%Y") at $(date +"%H:%M:%S")."

# check out a new local branch containing the site/ directory
git subtree split --prefix _site -b gh-pages-staging

# Revert changes to .gitignore in order to move to the gh-pages branch.
git checkout .gitignore

git checkout gh-pages
git cherry-pick $(git log gh-pages-staging -1 --pretty=oneline | awk '{ print $1 }') --strategy-option=theirs 
git push origin gh-pages

# cleanup
git checkout master
git reset --hard HEAD~1 # delete the latest commit and remove added files from the index
git branch -D gh-pages-staging
