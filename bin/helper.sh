#! /usr/bin/env bash

# get tmux info

tmux ls -F "#{session_created} #{session_name} #{session_attached} #{session_path}"