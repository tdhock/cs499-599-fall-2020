#!/bin/bash
rclone copy videos-class nau-gdrive:teaching/cs499-599-fall-2020/videos-class
rm videos-class/*.mp4
