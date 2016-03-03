#!/bin/bash

# This command for resizing images while preserving perceived quality was taken from
# https://www.smashingmagazine.com/2015/06/efficient-image-resizing-with-imagemagick/
# by Dave Newton

# exit script with nonzero exit code if any command fails
set -e

# echo each command
set -x

IMAGES_DIR=$1

for image in $(ls $IMAGES_DIR); do mogrify \
    -path $IMAGES_DIR \
    -filter Triangle \
    -define filter:support=2 \
    -thumbnail 600 \
    -unsharp 0.25x0.25+8+0.065 \
    -dither None \
    -posterize 136 \
    -quality 82 \
    -define jpeg:fancy-upsampling=off \
    -define png:compression-filter=5 \
    -define png:compression-level=9 \
    -define png:compression-strategy=1 \
    -define png:exclude-chunk=all \
    -interlace none \
    -colorspace sRGB \
    -strip $IMAGES_DIR/$image;
done
