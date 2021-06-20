#! /bin/bash

set -eux

git clone https://github.com/devinus/poison && cd poison
git checkout 4.0.1
patch -p1 < ../poison.patch
mix deps.get
MIX_ENV=bench mix run bench/run.exs
