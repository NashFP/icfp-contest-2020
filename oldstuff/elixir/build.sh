#!/bin/sh

cd app
mkdir -p bin
MIX_ENV=prod mix release
