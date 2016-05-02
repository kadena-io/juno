#!/bin/sh
tmux new-window
tmux split-window -h
tmux send-keys 'stack exec -- junoserver +RTS -N4 -T -RTS -c conf/10000-cluster.yaml --apiPort 8000' C-m
sleep 1
tmux split-window -v -p 75
tmux send-keys 'stack exec -- junoserver +RTS -N4 -T -RTS -c conf/10001-cluster.yaml --apiPort 8001' C-m
sleep 1
tmux split-window -v -p 66
tmux send-keys 'stack exec -- junoserver +RTS -N4 -T -RTS -c conf/10002-cluster.yaml --apiPort 8002' C-m
sleep 1
tmux split-window -v -p 50
tmux send-keys 'stack exec -- junoserver +RTS -N4 -T -RTS -c conf/10003-cluster.yaml --apiPort 8003' C-m
sleep 1
tmux select-pane -L
tmux send-keys './junoclient.sh'
