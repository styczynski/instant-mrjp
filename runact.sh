#!/bin/bash

IMAGE_SIZE="act"
BUILD_ARCH="linux/amd64"

act \
-P ubuntu-22.04=catthehacker/ubuntu:${IMAGE_SIZE}-22.04 \
-P ubuntu-latest=catthehacker/ubuntu:${IMAGE_SIZE}-latest \
-P ubuntu-20.04=catthehacker/ubuntu:${IMAGE_SIZE}-20.04 \
--container-architecture ${BUILD_ARCH} \
--no-cache-server \
-v