#!/bin/sh

#tmux new-window
#tmux send-keys -l -t @0 "docker-compose up"
tmux split-window -h
tmux send-keys -l -t :.0 "./tools/repl.sh"
#tmux bind-key k send-keys -t :.0 -l "i(rove:run-test *)"
tmux bind-key k send-keys -t :.0 -l "(rove:run-test *)"