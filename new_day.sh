#!/bin/sh

DAY="$(TZ='America/New_York' date '+%-d')"

if [ ! -e "src/Day$DAY.hs" ]; then
  cat > "src/Day$DAY.hs" <<EOF
module Day$DAY where

import Common (Parser, parseFromFile)

inputParser :: Parser _
inputParser = _

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile inputParser inputPath

  print input

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile inputParser inputPath

  -- print input
  () <$ return input
EOF
fi

if [ ! -e "day-$DAY-input.txt" ]; then
  aocdl -day "$DAY" -output 'day-{{.Day}}-input.txt'
fi

python3 generate_solutions_list.py
