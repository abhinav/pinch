#!/bin/bash

findUp() {
	if [[ -f "$1/$2" ]]; then
		echo "$1"
	else
		findUp "$1/.." "$2"
	fi
}

ROOT=$(findUp "." "pinch.cabal")

BASE="https://github.com/abhinav/pinch/tree/master"

OPTIONS="\
	--source-base=\"$BASE/\" \
	--source-module=\"$BASE/%{FILE}\" \
	--source-entity=\"$BASE/%{FILE}#L%{LINE}\""

pushd "$ROOT/gh-pages"
git rm -rf .
popd

pushd "$ROOT"
cabal configure
cabal haddock \
	--haddock-options="--odir=gh-pages $OPTIONS" \
	--html-location="http://hackage.haskell.org/packages/archive/\$pkg/latest/doc/html"
popd

pushd "$ROOT/gh-pages"
git add .
git commit -m "Documentation update at $(date '+%FT%T%z')"
popd
