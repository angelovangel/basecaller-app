#! /usr/bin/env bash

nvidia-smi -l 2 --query-gpu=utilization.gpu --format=csv,noheader,nounits