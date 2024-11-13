#!/bin/bash
convert -density 150 *.pdf page-%03d.png
convert -delay 100 -loop 0 page-*.png output.gif
