#!/bin/bash

stack exec -- rlwrap -A junoclient -c "conf/$(ls conf | grep -m 1 client)"
