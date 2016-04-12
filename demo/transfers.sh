#!/bin/sh
newman -c payments.json.postman_collection -f "SWIFT Transfers" -s
