#!/bin/bash
./header.awk --non-decimal-data forth.rst > $1
./words-imm.awk forth.rst >> $1
./words.awk --non-decimal-data forth.rst >> $1
./constants.awk forth.rst > constants
