#!/bin/sh
tmux new-window
tmux split-window -h
tmux send-keys 'stack exec -- junoserver +RTS -N2 -T -RTS -c conf/10000-cluster.yaml' C-m
tmux split-window -v -p 75
tmux send-keys 'stack exec -- junoserver +RTS -N2 -T -RTS -c conf/10001-cluster.yaml' C-m
tmux split-window -v -p 66
tmux send-keys 'stack exec -- junoserver +RTS -N2 -T -RTS -c conf/10002-cluster.yaml' C-m
tmux split-window -v -p 50
tmux send-keys 'stack exec -- junoserver +RTS -N2 -T -RTS -c conf/10003-cluster.yaml' C-m
tmux select-pane -L
tmux send-keys 'stack exec -- junoserver +RTS -N2 -T -RTS -c conf/10004-cluster.yaml' C-m
tmux split-window -v -p 75
tmux send-keys 'stack exec -- junoserver +RTS -N2 -T -RTS -c conf/10005-cluster.yaml' C-m
tmux split-window -v -p 66
tmux send-keys 'stack exec -- junoserver +RTS -N2 -T -RTS -c conf/10006-cluster.yaml' C-m
tmux split-window -v -p 50
tmux send-keys 'stack exec -- junoserver +RTS -N2 -T -RTS -c conf/10007-cluster.yaml' C-m
