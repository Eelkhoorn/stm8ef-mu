#!/bin/bash
dir="/home/pi/build/muforth/mu/tools/"
if [[ $1 = "-i" ]]
then
sed -r -f "$dir"ef_to_mu < "$2" > /tmp/"$2"
mv /tmp/"$2" "$2"
else
sed -r -f "$dir"ef_to_mu < "$1"
fi