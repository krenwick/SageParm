#!/bin/bash

# IF RUNNING ON HYALITE:
# Divide ins files into folders with 32 each
cd ins
i=1
while read l
do mkdir set_$i
mv $l set_$((i++))
done< <(ls|xargs -n32)