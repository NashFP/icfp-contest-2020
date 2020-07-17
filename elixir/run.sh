#!/bin/sh

_args="$@"

app/release/bin/app eval "Icfp2020.run(\"$_args\")" || echo "run error code: $?"
