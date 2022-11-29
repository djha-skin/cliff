#!/bin/sh

tmux new-window
tmux send-keys -l -t @0 "docker-compose up"
tmux send-keys -l -t @1 "./tools/repl.sh"
tmux split-window -h
#tmux bind-key k send-keys -t :1.0 -l "i(rove:run-test *)"
tmux bind-key k send-keys -t :1.0 -l "(rove:run-test *)"