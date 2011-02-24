#!/bin/bash

TIME=/usr/bin/time

bench_log=/tmp/bench.tmp
csv=/tmp/csv1.tmp
csv_tmp=/tmp/csv2.tmp
touch $csv

for threads in `seq 1 16`; do
  for iterations in `seq 1 20`; do
    $TIME -f "%e" ./mccompiled 0 $threads > /dev/null 2>> $bench_log
  done
  # add column
  paste -d ';' $csv $bench_log > $csv_tmp
  # swap vars
  mv -f $csv_tmp $csv
  # cleanup
  rm $bench_log  
done
# cut out the first character (being the ; seperator)
cut -c1 --complement $csv > $csv_tmp
mv -f $csv_tmp benchmark_data.csv
rm $csv
