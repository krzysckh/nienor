#!/bin/sh
# -*- mode: sh; sh-basic-offset: 2 -*-

nienor=../bin/nienor
uxn=uxn11

fail() {
  echo "$1 failed to compile successfully :(" >/dev/stderr
  exit 1
}

# this simply compiles and runs all tests.
# this does NO checks to figure out if they're working properly, just if they're compilable
for f in `find . -type f -name '*.scm'`; do
  echo "$f"
  $nienor -O -o a.out "$f" >/dev/null || fail "$f"
  timeout --foreground -s 9 1 $uxn a.out >/dev/null 2>/dev/null
done
