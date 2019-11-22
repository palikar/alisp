#!/bin/sh

DIR=$(dirname "$(readlink -f "$0")")

MODULE_NAME=$1
PROJECT_NAME=$(grep "project(\w*" CMakeLists.txt -o | grep "(.*" -o | cut -c 2-)

if [ -d $DIR/${PROJECT_NAME}/${MODULE_NAME} ]; then
    echo "The module already exists"
    exit 1
fi

cp -r $DIR/templates/template_app_module/ $DIR/src/
mv $DIR/src/template_app_module/ $DIR/src/${MODULE_NAME}
mv $DIR/src/${MODULE_NAME}/include/PROJECT/MODULE_NAME $DIR/src/${MODULE_NAME}/include/PROJECT/${MODULE_NAME}
mv $DIR/src/${MODULE_NAME}/include/PROJECT $DIR/src/${MODULE_NAME}/include/${PROJECT_NAME}

find $DIR/src/${MODULE_NAME} -type f -exec sed -i "s/MODULE_NAME/${MODULE_NAME}/g" {} \;
find $DIR/src/${MODULE_NAME} -type f -exec sed -i "s/PROJECT_NAME/${PROJECT_NAME}/g" {} \;

LINE="add_subdirectory(${MODULE_NAME})"
if [ ! $(grep $LINE $DIR/src/CMakeLists.txt) ]; then
    echo ${LINE} >> $DIR/src/CMakeLists.txt
fi

