#!/bin/bash
nohup ./.stack-work/install/x86_64-osx/lts-5.12/7.10.3/bin/junoserver +RTS -N4 -T -RTS -c conf/10000-cluster.yaml --apiPort 8000 >log/10000.log 2>&1 &
sleep 1
nohup ./.stack-work/install/x86_64-osx/lts-5.12/7.10.3/bin/junoserver +RTS -N4 -T -RTS -c conf/10001-cluster.yaml --apiPort 8001 >log/10001.log 2>&1 &
sleep 1
nohup ./.stack-work/install/x86_64-osx/lts-5.12/7.10.3/bin/junoserver +RTS -N4 -T -RTS -c conf/10002-cluster.yaml --apiPort 8002 >log/10002.log 2>&1 &
sleep 1
nohup ./.stack-work/install/x86_64-osx/lts-5.12/7.10.3/bin/junoserver +RTS -N4 -T -RTS -c conf/10003-cluster.yaml --apiPort 8003 >log/10003.log 2>&1 &
sleep 1
