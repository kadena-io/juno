#!/bin/bash

dist/build/bftserver/bftserver -d -p public_keys.txt -c client_public_keys.txt -k private_keys/10002.txt -s 10002 10001 10000 10003
