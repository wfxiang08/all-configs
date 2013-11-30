#!/bin/bash

HOST_OS=`uname`

list="gitconfig"
suffix=

case $HOST_OS in
    Darwin)
        suffix="_darwin"
        ;;
    Linux)
        suffix="_linux"
        ;;
esac

relpath() {
    case "$2" in
        *"/")
            eval $1="$2"
            ;;
        *)
            eval $1="$2/"
            ;;
    esac
}

relpath home "$HOME"
relpath path `pwd`

path=${path#$home}

cd ~
for file in $list; do
    ln_src="$path$file$suffix"; ln_dst=".$file"

    rm -f $ln_dst; ln -s $ln_src $ln_dst

    if [ $? -eq 0 ]; then
        e="*"
    else
        e="-"
    fi
    printf "%c %-20s -->     %s\n" "$e" "$ln_dst" "$ln_src"
done
