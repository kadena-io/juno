#!/bin/bash

dist/build/bftserver/bftserver -d -p public_keys.txt -c client_public_keys.txt -k private_keys/10001.txt -s 10001 10000 10002 10003
