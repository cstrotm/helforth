#! /bin/bash
OUT="$1"
HFBIN="`dirname "$0"`"
HFDIR="$HFBIN/.."
echo "#" >Deps.mk
echo "# autogenerated by mkdep.sh - DO NOT EDIT" >>Deps.mk
echo "#" >>Deps.mk
for a in "$@" ; do
  b="`basename "$a"`"
  echo "$b:" "`"$HFBIN/fdep" "$a"`" >>Deps.mk
done
