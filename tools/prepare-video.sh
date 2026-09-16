#!/bin/bash

# Prepare a video for use on this site: re-encode it for fast delivery, and
# generate the poster image which stands in for it until it plays.
#
#   tools/prepare-video.sh projects/photo-pairs/static/book-mockup.mp4
#
# The video is replaced in place. The poster is written next to it as
# <name>-poster.webp, the name the pages on this site expect, and is taken
# from the first frame of the finished video, so that the moment the video
# replaces the poster passes unnoticed.
#
# Video exported from a camera or an editor is not ready for the web. Each of
# these transformations matters for a video which autoplays as a page loads:
#
#   -movflags +faststart  Moves the index (the 'moov' atom) to the front of
#                         the file. Without it the browser must download the
#                         whole video before it can show a single frame.
#   -an                   Drops the audio track. These videos play muted, so
#                         the audio is bytes nobody will ever hear.
#   -crf / -preset        Re-encodes at a bitrate suited to the web, which is
#                         typically a fraction of what came out of the editor.
#   -pix_fmt yuv420p      The pixel format every browser can decode.
#
# Both qualities can be overridden for a video which needs something else:
#
#   CRF=20 POSTER_QUALITY=90 tools/prepare-video.sh path/to/video.mp4
#
# Re-running this on a video it has already prepared would re-encode it a
# second time and lose quality for nothing, so it regenerates the poster and
# leaves the video alone. Pass FORCE=1 to re-encode anyway.

# exit script with nonzero exit code if any command fails
set -e

# Constant Rate Factor: lower means better quality and a larger file. At 23 the
# result is hard to tell from the original (SSIM ~0.99) at a quarter the size.
CRF=${CRF:-23}

# Quality of the WebP poster, on the same 0-100 scale that cwebp uses.
POSTER_QUALITY=${POSTER_QUALITY:-80}

VIDEO=$1

if [ -z "${VIDEO}" ]; then
    echo "usage: $0 <video>" >&2
    exit 1
fi

if [ ! -f "${VIDEO}" ]; then
    echo "$0: no such file: ${VIDEO}" >&2
    exit 1
fi

for command in ffmpeg ffprobe cwebp; do
    if ! command -v "${command}" > /dev/null; then
        echo "$0: ${command} is required but not installed." >&2
        echo "Install it with: brew install ffmpeg webp" >&2
        exit 1
    fi
done

POSTER="${VIDEO%.*}-poster.webp"

WORK_DIR=$(mktemp -d)
trap 'rm -rf "${WORK_DIR}"' EXIT

# Size of a file in bytes. `ls` rather than `stat`, whose flags differ between
# macOS and Linux.
bytes() {
    ls -l "$1" | awk '{ print $5 }'
}

# Size of a file, for the summary printed as the script works.
filesize() {
    bytes "$1" | awk '{
        if ($1 < 1048576) printf "%.0f KB", $1 / 1024;
        else printf "%.1f MB", $1 / 1048576
    }'
}

# Whether the video has already been through this script: no audio track, and
# the 'moov' atom at the front of the file rather than after the video data.
is_prepared() {
    local audio
    audio=$(ffprobe -v error -select_streams a -show_entries stream=codec_type -of csv=p=0 "$1")

    [ -z "${audio}" ] && head -c 512 "$1" | grep -qa moov
}

echo "Reading ${VIDEO} ($(filesize "${VIDEO}"))"

if [ -z "${FORCE:-}" ] && is_prepared "${VIDEO}"; then
    echo "  already prepared; leaving the video alone (FORCE=1 to re-encode)"
else
    echo "  re-encoding at crf ${CRF}..."
    ffmpeg -v error -i "${VIDEO}" \
        -an \
        -c:v libx264 \
        -crf "${CRF}" \
        -preset slow \
        -pix_fmt yuv420p \
        -movflags +faststart \
        -y "${WORK_DIR}/video.mp4"

    # Re-encoding a video which is already lean enough can come out larger than
    # it started. Keep whichever is smaller: the point is a faster page.
    if [ "$(bytes "${WORK_DIR}/video.mp4")" -lt "$(bytes "${VIDEO}")" ]; then
        mv -f "${WORK_DIR}/video.mp4" "${VIDEO}"
        echo "  wrote ${VIDEO} ($(filesize "${VIDEO}"))"
    else
        echo "  re-encoding gained nothing; keeping the original"
    fi
fi

echo "Generating ${POSTER}"

# The poster is the first frame of the video, so the two are indistinguishable
# at the moment playback begins.
ffmpeg -v error -i "${VIDEO}" -vf "select=eq(n\,0)" -vframes 1 -y "${WORK_DIR}/poster.png"
cwebp -quiet -q "${POSTER_QUALITY}" "${WORK_DIR}/poster.png" -o "${POSTER}"

echo "  wrote ${POSTER} ($(filesize "${POSTER}"))"
