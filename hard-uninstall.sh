#!/bin/bash
lib=`opam config var lib`
boot="$lib/drumaderian"
echo "erase [drumaderian] localised in [$boot]"
rm -r $boot
