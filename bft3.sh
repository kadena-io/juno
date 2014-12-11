#!/bin/bash

dist/build/bftserver/bftserver -d -p public_keys.txt -c client_public_keys.txt -k private_keys/10003.txt -s 10003 10001 10002 10000
