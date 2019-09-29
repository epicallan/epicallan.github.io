#!/usr/bin/env bash
set -eo pipefail

# Clean rebuild
cabal new-exec site rebuild

# Create deploy environment inside of .deploy directory
rm -rf .deploy
mkdir .deploy
cd .deploy

git init
git remote add origin git@github.com:epicallan/epicallan.github.io.git
git pull -r origin master

# Add built site files
rsync -a ../_site/ .

cp ../README.md README.md
cp ../CNAME CNAME

git add .
git add --force tags/*
git commit -m "$1"
git push origin master

# Cleanup .deploy directory after a successful push
cd ..
rm -rf .deploy
