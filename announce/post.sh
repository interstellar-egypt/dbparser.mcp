#!/bin/sh
# Cuts the recordings into the ~30 s demo with ffmpeg only: segments are cut,
# sped up or held on one real frame. Nothing on screen is changed or added.
# Run from the repo root after the three tapes:
#   vhs announce/card-open.tape && vhs announce/card-close.tape
#   vhs announce/dbparser-mcp-demo.tape     # -> announce/raw.mp4
#   vhs announce/dbparser-mcp-calls.tape    # -> announce/calls.mp4
#   sh announce/post.sh
# The times below are for the take of 14 Nov 2026 (session 052bec43); a new
# take needs new times.
set -eu
cd "$(dirname "$0")"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
enc="-an -c:v libx264 -preset slow -crf 18 -pix_fmt yuv420p -r 25"

# clip NAME SOURCE START END SPEED: cut [START, END) and play it SPEED times
clip() {
  ffmpeg -v error -ss "$3" -to "$4" -i "$2" \
    -vf "setpts=PTS/$5,fps=25,scale=1920:1080" $enc -y "$tmp/$1.mp4"
}
# hold NAME SOURCE AT SECONDS: show the frame at AT for SECONDS
hold() {
  ffmpeg -v error -ss "$3" -i "$2" -frames:v 1 -y "$tmp/$1.png"
  ffmpeg -v error -loop 1 -t "$4" -i "$tmp/$1.png" \
    -vf "fps=25,scale=1920:1080" $enc -y "$tmp/$1.mp4"
}

hold 00 card-open.mp4  1.5       2     # opening card
clip 01 raw.mp4        0    5.5  4     # start Claude Code with only dbparser
clip 02 raw.mp4       15.5 24.5  4     # prompt 1 typed
clip 03 raw.mp4       24.5 40.0 10     # "Calling dbparser 5 times..."
hold 04 calls.mp4      1.0       1.8   # the calls (ctrl+o): parse_file
hold 05 calls.mp4     31.5       2.2   #   read_table gene_name, name = PD1
hold 06 calls.mp4     40.5       2.2   #   read_table gene, hgnc_id in 3 ids
clip 07 raw.mp4       40.3 45.8  1     # answer 1: genes, source, licence
clip 08 raw.mp4      171.0 175.0 3     # prompt 2 typed
clip 09 raw.mp4      175.0 201.2 20    # export_csv running
clip 10 raw.mp4      201.2 205.7 1     # answer 2: file written, licence
hold 11 calls.mp4     81.0       2.5   # head -3 of the CSV
hold 12 card-close.mp4 1.5       2     # closing card

for f in "$tmp"/[0-9][0-9].mp4; do echo "file '$f'"; done > "$tmp/list.txt"
ffmpeg -v error -f concat -safe 0 -i "$tmp/list.txt" $enc -movflags +faststart \
  -y dbparser-mcp-demo.mp4

# GIF for the README: 960 px wide, 10 fps, one optimised palette
ffmpeg -v error -i dbparser-mcp-demo.mp4 \
  -vf "fps=10,scale=960:-1:flags=lanczos,split[a][b];[a]palettegen=max_colors=64:stats_mode=diff[p];[b][p]paletteuse=dither=none:diff_mode=rectangle" \
  -y ../man/figures/dbparser-mcp-demo.gif

# poster: answer 1 with the genes, source and licence
ffmpeg -v error -ss 42 -i raw.mp4 -frames:v 1 -y dbparser-mcp-demo-poster.png
