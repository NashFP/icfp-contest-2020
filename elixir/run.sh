#!/bin/sh

_args="$@"

app/release/bin/app eval "ExampleApp.hello(\"$_args\")" || echo "run error code: $?"
