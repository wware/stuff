#!/bin/sh

convert -size 390x390 xc:transparent nyuck.png
composite -geometry -52-40 icon.png nyuck.png nyuck.png
