#!/bin/bash

DIR=$(dirname "$(readlink -f "$0")")

BUILDS_FILE=$1

while IFS= read -r line; do

    FOLDER=$(echo $line | cut -d'|' -f1)
    CMAKE_ARGS=$(echo $line | cut -d'|' -f2)
    MAKE_ARGS=$(echo $line | cut -d'|' -f3)
    CMAKE_ENV=$(echo $line | cut -d'|' -f4)

    mkdir -p ${FOLDER}

    cd ${FOLDER} || exit
    echo "Building ${FOLDER}"
    eval "${CMAKE_ENV} cmake .. ${CMAKE_ARGS} &> cmake_output.txt"
    make ${MAKE_ARGS} &> make_output.txt
    cd - &> /dev/null || exit

done < ${DIR}/${BUILDS_FILE}
