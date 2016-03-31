#!/bin/bash

if [ -z "$1" ]
then
  BFT_PORT=10008
else
  BFT_PORT=$1
fi

rlwrap stack exec -- junoclient -c conf/10004.yaml
