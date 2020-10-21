#!/bin/bash
set -o errexit
rclone copy videos-class nau-gdrive:teaching/cs499-599-fall-2020/videos-class
rm -r videos-class/*.mp4 ~/Documents/Zoom/*
