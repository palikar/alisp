#!/bin/bash

DIR=$(dirname "$(readlink -f "$0")")


while IFS= read -r line; do

    FOLDER=$(echo $line | cut -d'|' -f1)
    CMAKE_ARGS=$(echo $line | cut -d'|' -f2)
    MAKE_ARGS=$(echo $line | cut -d'|' -f3)

    mkdir -p ${FOLDER}

    cd ${FOLDER} || exit
    echo "Building ${FOLDER}"
    cmake .. ${CMAKE_ARGS} &> cmake_output.txt
    make ${MAKE_ARGS} &> make_output.txt
    cd - &> /dev/null || exit

done < ${DIR}/configs_builds.txt