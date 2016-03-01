#!/bin/sh
newman -c payments.json.postman_collection -f "Account Creation" -s
