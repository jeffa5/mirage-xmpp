#!/usr/bin/env bash

echo "mirage $(dune external-lib-deps @all | sed '/\./d' | awk 'NR > 1 { printf "%s ",$2 }')"
