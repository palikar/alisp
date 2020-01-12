#!/bin/bash

DIR=$(dirname "$(readlink -f "$0")")

cd $DIR/build

seq 1 | xargs -I -- ctest | grep -i "total test time" | grep "\d+\.\d+" -P -o | xargs -I {} -- echo -n {}, && echo ""
