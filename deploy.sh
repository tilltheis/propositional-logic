#!/bin/sh

# deploy project as github page
# usage: sh deploy.sh <msg>

if [[ -z "$1" ]] ; then
  echo "you forgot the commit message"
  exit 1
fi

(cd src && hastec -O0 --out=../app.js app.hs)

files="App.html app.js foundation.css"

for f in $files ; do
  cp "$f" /tmp
done

git checkout gh-pages

for f in $files ; do
  cp /tmp/"$f" .
done

mv App.html index.html

git commit -am "$1"
git push

git checkout master
