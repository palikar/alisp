#!/bin/sh

DIR=$(dirname "$(readlink -f "$0")")

MODULE_NAME=$1


if [ -d $DIR/numler/${MODULE_NAME} ]; then
	echo "The module already exists"
	exit 1
fi


cp -r $DIR/templates/template_lib_module/ $DIR/numler/

mv $DIR/numler/template_lib_module/ $DIR/numler/${MODULE_NAME}

mv $DIR/numler/${MODULE_NAME}/include/numler/MODULE_NAME $DIR/numler/${MODULE_NAME}/include/numler/${MODULE_NAME}

find $DIR/numler/${MODULE_NAME} -type f -exec sed -i "s/MODULE_NAME/${MODULE_NAME}/g" {} \;


LINE="add_subdirectory(${MODULE_NAME})"
if [ ! $(grep $LINE $DIR/numler/CMakeLists.txt) ]; then
	echo ${LINE} >> $DIR/numler/CMakeLists.txt
fi




