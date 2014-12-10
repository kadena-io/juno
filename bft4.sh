#!/bin/bash

dist/build/bftserver/bftserver -d -p public_keys.txt -k private_keys/10004.txt -s 10004 10001 10002 10003 10000
