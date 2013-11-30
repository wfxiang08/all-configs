#!/bin/bash

HOST_OS=`uname`

case $HOST_OS in
    Darwin|Linux)
        export HOST_OS=$HOST_OS
        ;;
    *)
        echo "Unknown HOST_OS=$HOST_OS"
        exit 1
        ;;
esac

SCRIPT=".setup.sh"

for dir in `ls`; do
    if [ -n "$dir" -a -d "$dir" ]; then
        (cd $dir;
            if [ -f "$SCRIPT" ]; then
                bash $SCRIPT
            fi
        );
    fi
done

git submodule update
