#!/bin/sh

DAY="$(date '+%-d')"

if [ ! -e "src/Day$DAY.hs" ]; then
  cat > "src/Day$DAY.hs" <<EOF
module Day$DAY where

part1 :: FilePath -> IO ()
part1 _ = return ()

part2 :: FilePath -> IO ()
part2 _ = return ()
EOF
fi

if [ ! -e "day-$DAY-input.txt" ]; then
  aocdl -output 'day-{{.Day}}-input.txt'
fi
