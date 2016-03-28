#/bin/sh

## Happy path
# Create accounts TSLA, KALE
curl -H "Content-Type: application/json" -X POST -d '{ "payload" : { "account": "TSLA" }, "digest": { "hash" : "mhash", "key" : "mykey"} }' http://localhost:8000/api/juno/v1/accounts/create
curl -H "Content-Type: application/json" -X POST -d '{ "payload" : { "account": "KALE" }, "digest": { "hash" : "mhash", "key" : "mykey"} }' http://localhost:8000/api/juno/v1/accounts/create

# Add 100 to TSLA, subtract 50
curl -H "Content-Type: application/json" -X POST -d '{ "payload": { "account": "TSLA", "amount": 100.0 }, "digest": { "hash": "myhash", "key": "string" } }' http://localhost:8000/api/juno/v1/accounts/adjust
curl -H "Content-Type: application/json" -X POST -d '{ "payload": { "account": "TSLA", "amount": -50.0 }, "digest": { "hash": "myhash", "key": "string" } }' http://localhost:8000/api/juno/v1/accounts/adjust

## Error path
curl -H "Content-Type: application/json" -X POST -d '{ "payloadGarb" : { "account": "KALE" }, "digest": { "hash" : "mhash", "key" : "mykey", "garbage" : "jsonError"} }' http://localhost:8000/api/juno/v1/accounts/create