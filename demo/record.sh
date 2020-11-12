#!/usr/bin/env bash

OUTPUT_FILE="detsys_1_part_5"
VIDEO_SIZE="1600x900"

ffmpeg -f alsa -i default \
       -f x11grab -video_size "${VIDEO_SIZE}" -i :0.0 \
       "${OUTPUT_FILE}".mkv

ffmpeg -i "${OUTPUT_FILE}".mkv -c:v libvpx-vp9 \
       -minrate 300k -maxrate 300k -b:v 300k \
       -c:a libvorbis "${OUTPUT_FILE}".webm
