#!/bin/bash

TIME=/usr/bin/time

bench_log=/tmp/bench.tmp
csv=/tmp/csv1.tmp
csv_tmp=/tmp/csv2.tmp
rm -f $bench_log $csv $csv_tmp
touch $csv

for threads in `seq 1 8`; do
	echo "T-$threads " > $bench_log
  for iterations in `seq 1 20`; do
	  echo "$threads threads, iteration $iterations";
    ./mccompiled -t $threads > /dev/null 2>> $bench_log;
    echo " " >> $bench_log;
  done
  # add column
  paste -d ',' $csv $bench_log > $csv_tmp
  # swap vars
  mv -f $csv_tmp $csv
  # cleanup
  rm $bench_log  
done
# cut out the first character (being the ; seperator)
cut -c1 --complement $csv > $csv_tmp
mv -f $csv_tmp benchmark_data.csv
rm $csv
