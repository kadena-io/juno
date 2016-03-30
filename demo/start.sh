#!/bin/sh
tmux new-window
tmux split-window -h
tmux send-keys 'stack exec -- junoserver -d -p public_keys.txt -c client_public_keys.txt -k private_keys/10000.txt -s 127.0.0.1:10000 127.0.0.1:10001 127.0.0.1:10002 127.0.0.1:10003 +RTS -T 2>log/10000.log' C-m
sleep 1
tmux split-window -v -p 75
tmux send-keys 'stack exec -- junoserver -d -p public_keys.txt -c client_public_keys.txt -k private_keys/10001.txt -s 127.0.0.1:10001 127.0.0.1:10000 127.0.0.1:10002 127.0.0.1:10003 +RTS -T 2>log/10001.log' C-m
sleep 1
tmux split-window -v -p 66
tmux send-keys 'stack exec -- junoserver -d -p public_keys.txt -c client_public_keys.txt -k private_keys/10002.txt -s 127.0.0.1:10002 127.0.0.1:10001 127.0.0.1:10000 127.0.0.1:10003 +RTS -T 2>log/10002.log' C-m
sleep 1
tmux split-window -v -p 50
tmux send-keys 'stack exec -- junoserver -d -p public_keys.txt -c client_public_keys.txt -k private_keys/10003.txt -s 127.0.0.1:10003 127.0.0.1:10001 127.0.0.1:10002 127.0.0.1:10000 +RTS -T 2>log/10003.log' C-m
sleep 1
tmux select-pane -L
tmux send-keys './junoclient.sh'
