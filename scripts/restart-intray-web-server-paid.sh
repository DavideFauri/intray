#!/usr/bin/env bash
set -x

cd $HOME

pkill -f 'intray-web-server serve' || true

set -e

env | sort

export INTRAY_SERVER_LOG_LEVEL=LevelDebug

intray-web-server serve \
  --admin admin &
