#!/bin/sh

rm -rf xml html latex
doxygen
(cd html; firefox index.html)
