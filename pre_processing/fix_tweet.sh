#!/bin/bash
for filename in API_csv/*.csv; do
  sed -e "s///g" $filename > "fixed_"$(basename "$filename")
mv fixed_*.csv API_csv
done
