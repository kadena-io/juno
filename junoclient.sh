#!/bin/bash

stack exec -- rlwrap -A junoclient -c "conf/$(ls conf | sort -r | grep -m 1 client)"
