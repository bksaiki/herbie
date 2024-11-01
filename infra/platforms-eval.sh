#!/bin/bash

# exit immediately upon first error
set -e -x

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done

INFRA_DIR="$(cd -P "$(dirname "$src")" && pwd)"
DEFAULT_START_SEED=1

# check arguments
if [ "$#" -lt 2 ]; then
  echo "Usage: $0 <output_dir> <bench> ..."
  exit 1
else
  OUT_DIR="$(realpath $1)"; shift
fi

# check for multi-threading
if [ -z "$THREADS" ]; then
  echo "Running with a single-thread"
  THREADS=1
  HERBIE_THREADS=1
else
  echo "Running with $THREADS threads"
  HERBIE_THREADS=$THREADS
fi

# advise user of execution plan
if [ -z "$PARALLEL_SEEDS" ]; then
  echo "Using Herbie concurrency only."
  PARALLEL_SEEDS=1
else
  # support for exporting bash environment to parallel
  echo "Using multiple concurrent Herbie runs in parallel."
  echo "Restricting to $PARALLEL_SEEDS parallel concurrent Herbie runs."
fi

# check for multiple seeds
if [ -z "$NUM_SEEDS" ]; then
  echo "Running on 1 seed"
  NUM_SEEDS=1
else
  echo "Running on $NUM_SEEDS seed"
fi

# check for start seed
if [ -z "$START_SEED" ]; then
  START_SEED=$DEFAULT_START_SEED
  echo "Start seed not specified. Starting at seed $START_SEED"
else
  echo "Starting at seed $START_SEED"
fi

# check if AVX should be disabled
if [ -z "$NO_AVX" ]; then
  avx_str=""
else
  echo "Disabling AVX"
  avx_str="--no-avx"
fi


echo "Running platforms evaluation"
date

function run() {
  bench=$1
  key=$2
  num_runs=$3

  python3 $INFRA_DIR/platforms-eval.py \
    --key $key \
    --parallel $PARALLEL_SEEDS \
    --threads $THREADS \
    --herbie-threads $HERBIE_THREADS \
    --start-seed $START_SEED \
    $avx_str \
    $bench \
    "$OUT_DIR/platforms" \
    $num_runs
}

# Run configs
for path in "$@"
do
  filename="${path##*/}"
  basename="${filename%.*}"
  run $(realpath $path) $basename $NUM_SEEDS
done

# Plotting
python3 $INFRA_DIR/platforms/cross-plot.py "$OUT_DIR/platforms/output"

echo "Finished platforms evaluation"
date

make clean

# clean up cache and build files
if [ -n "$RM_CACHE" ]; then
  echo "removing cache and drivers"
  rm -rf "$OUT_DIR/platforms/herbie-2.0"
  rm -rf "$OUT_DIR/platforms/cache"
  rm -rf "$OUT_DIR/platforms/drivers"
fi
