#!/bin/zsh
# Search repositories when a command was not found
file=/usr/share/doc/pkgfile/command-not-found.zsh 
[ -e $file ] && source $file
