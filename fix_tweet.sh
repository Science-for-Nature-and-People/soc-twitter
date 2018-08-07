#!/bin/bash
for filename in *.csv; do
  sed -e "s///g" $filename > "fixed_"$(basename "$filename")
done
