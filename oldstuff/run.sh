#!/bin/sh

/root/.local/bin/haskell-game-exe "$@" || echo "run error code: $?"
