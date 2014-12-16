#!/bin/bash
# RUN this from herbie root!
echo "" > test/num-points-test-results.csv
for power in `seq 2 11`; do
    points=`awk 'BEGIN{print 2^'$power'}'`
    racket reports/make-report.rkt -s $points bench/hamming/
    racket compile/results-to-csv.rkt graphs/results.herbie.dat graphs/results.herbie.csv
    awk -F , 'BEGIN{totalPointsRecovered=0; totalTime=0}\
{ totalPointsRecovered += ($8 - $7); totalTime += $6} \
END {print '$points',",",(log(totalPointsRecovered)/log(2)), ",", (totalTime / 1000)}' graphs/results.herbie.csv >> test/num-points-test-results.csv
done
