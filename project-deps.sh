#!/usr/bin/env bash

echo "mirage $(dune external-lib-deps @all | sed 's/\..*//' | uniq | awk 'NR > 1 { printf "%s ",$2 }')"
