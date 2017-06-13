#!/usr/bin/env zsh
./a.out "$(grep " $1\$" /scratch/idsAndTitles | cut -d" " -f1)"  "$(grep " $2\$" /scratch/idsAndTitles | cut -d" " -f1)" | tac | while read id
do
       grep "^$id " /scratch/idsAndTitles
done | cut -d" " -f2
