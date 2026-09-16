#!/bin/bash

# Compress the images belonging to a post, an artwork, or a pair: resize them
# to the width they are actually displayed at, and strip the metadata a camera
# leaves behind. Images are replaced in place.
#
#   tools/compress-images.sh posts/my-new-post
#   tools/compress-images.sh projects/photo-pairs/book/pair/sandy-hook-to-svalbard
#   tools/compress-images.sh path/to/one-image.jpg
#
# Given a directory, this looks for JPEGs in the directory itself and in its
# 'images' and 'static' subdirectories, which is where the images belonging to
# a post and to a pair live, respectively.
#
# The default width suits the 600px column the site sets its text and images
# in. Override it for images shown wider than that, or on a page where they
# deserve the extra detail:
#
#   WIDTH=1200 QUALITY=85 tools/compress-images.sh <path>
#
# An image already at or below the target width is left alone, so that running
# this a second time does not compress an image a second time.

# The command for resizing images while preserving perceived quality was taken from
# https://www.smashingmagazine.com/2015/06/efficient-image-resizing-with-imagemagick/
# by Dave Newton

# exit script with nonzero exit code if any command fails
set -e

WIDTH=${WIDTH:-600}
QUALITY=${QUALITY:-82}

if [ $# -eq 0 ]; then
    echo "usage: $0 <directory or image>..." >&2
    exit 1
fi

for command in magick mogrify; do
    if ! command -v "${command}" > /dev/null; then
        echo "$0: ${command} is required but not installed." >&2
        echo "Install it with: brew install imagemagick" >&2
        exit 1
    fi
done

# The images named by a single argument: the file itself, or every JPEG in the
# directory and in the subdirectories the site keeps images in.
images_in() {
    if [ -f "$1" ]; then
        echo "$1"
        return
    fi

    local directories=()
    for directory in "$1" "$1/images" "$1/static"; do
        if [ -d "${directory}" ]; then
            directories+=("${directory}")
        fi
    done

    find "${directories[@]}" -maxdepth 1 \
        \( -iname '*.jpg' -o -iname '*.jpeg' \) | sort
}

# Size of a file, for the summary printed as the script works.
filesize() {
    ls -l "$1" | awk '{
        if ($5 < 1048576) printf "%.0f KB", $5 / 1024;
        else printf "%.1f MB", $5 / 1048576
    }'
}

compressed=0
skipped=0

for argument in "$@"; do
    if [ ! -e "${argument}" ]; then
        echo "$0: no such file or directory: ${argument}" >&2
        exit 1
    fi

    while read -r image; do
        [ -z "${image}" ] && continue

        width=$(magick identify -format '%w' "${image}")

        if [ "${width}" -le "${WIDTH}" ]; then
            echo "${image}: already ${width}px wide; leaving it alone"
            skipped=$((skipped + 1))
            continue
        fi

        before=$(filesize "${image}")

        # '>' resizes only images wider than the target, so this never enlarges
        # an image to meet the width.
        mogrify \
            -filter Triangle \
            -define filter:support=2 \
            -thumbnail "${WIDTH}>" \
            -unsharp 0.25x0.25+8+0.065 \
            -dither None \
            -posterize 136 \
            -quality "${QUALITY}" \
            -define jpeg:fancy-upsampling=off \
            -define png:compression-filter=5 \
            -define png:compression-level=9 \
            -define png:compression-strategy=1 \
            -define png:exclude-chunk=all \
            -interlace none \
            -colorspace sRGB \
            -strip "${image}"

        echo "${image}: ${width}px ${before} -> ${WIDTH}px $(filesize "${image}")"
        compressed=$((compressed + 1))
    done <<< "$(images_in "${argument}")"
done

echo "Compressed ${compressed} image(s), left ${skipped} alone."
