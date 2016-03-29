#!/bin/bash

if [ -z "$1" ]
then
  BFT_PORT=10008
else
  BFT_PORT=$1
fi

rlwrap stack exec -- junoclient -p public_keys.txt -k private_keys/$BFT_PORT.txt -s 127.0.0.1:$BFT_PORT 127.0.0.1:10000 127.0.0.1:10001 127.0.0.1:10002 127.0.0.1:10003
