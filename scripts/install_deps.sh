#!/usr/bin/env bash

set -e
set -x

DIRS=(
	.
	./examples/keyvalue
)

for d in "${DIRS[@]}"; do
	( \
		cd "$d" && \
		stack --resolver "$RESOLVER" --no-terminal --skip-ghc-check test --only-snapshot \
	)
done
