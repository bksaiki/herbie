#!/bin/bash

CORES=6

function run {
  bench=$1; shift
  name=$1; shift

  racket "src/herbie.rkt" report \
      --note "$name" \
      --profile \
      --threads $CORES \
      "$@" \
      "$bench" "reports/$name"
  bash infra/publish.sh upload "reports/$name"
}

function runEach {
  for bench in bench/*; do
    name=$(basename "$bench" .fpcore)
    # add cases to skip large or misbehaving benchmarks
    case $name in
      haskell) ;;
      random) ;;
      *) run "$bench" "$name" "$@" ;;
    esac
  done
}

# use common seed across every 4 day cycle
d=$(date "+%Y%j")
qseed=$(racket -e "(random-seed $d)")
seed="${qseed:1}" # :1 removes leading quote

mkdir -p reports
runEach --seed "$seed" "$@"
