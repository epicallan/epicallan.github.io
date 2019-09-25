#!/usr/bin/env bash
set -eo pipefail

rm -rf .deploy-dev

# Clean rebuild
cabal new-exec site rebuild

# Create deploy environment inside of .deploy directory
mkdir .deploy-dev
cd .deploy-dev

# git pull -r origin master

# Add built site files
rsync -a ../_site/ .

python -m SimpleHTTPServer
