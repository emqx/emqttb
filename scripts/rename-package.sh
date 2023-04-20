#!/usr/bin/env bash

set -euo pipefail

cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

UNAME="$(uname -s)"

case "$UNAME" in
    Darwin)
        DIST='macos'
        VERSION_ID=$(sw_vers -productVersion | cut -d '.' -f 1)
        SYSTEM="${DIST}${VERSION_ID}"
        ;;
    Linux)
        DIST="$(sed -n '/^ID=/p' /etc/os-release | sed -r 's/ID=(.*)/\1/g' | sed 's/"//g')"
        VERSION_ID="$(sed -n '/^VERSION_ID=/p' /etc/os-release | sed -r 's/VERSION_ID=(.*)/\1/g' | sed 's/"//g')"
        SYSTEM="$(echo "${DIST}${VERSION_ID}" | sed -r 's/([a-zA-Z]*)-.*/\1/g')"
        ;;
    CYGWIN*|MSYS*|MINGW*)
        SYSTEM="windows"
        ;;
esac

ARCH="$(uname -m)"
case "$ARCH" in
    x86_64)
        ARCH='amd64'
        ;;
    aarch64)
        ARCH='arm64'
        ;;
    arm*)
        ARCH=arm
        ;;
esac

BASE=$(find ./_build/emqttb/rel/emqttb -name "*.tar.gz" | tail -1)
VSN="$(echo "$BASE" | sed -E -e 's|.+emqttb-(.+)\.tar\.gz|\1|')"
QUIC=$(find ./_build/emqttb/rel/emqttb -name "quicer-*" | grep -q quicer && echo '-quic' || echo '')
cp "$BASE" "./emqttb-${VSN}-${SYSTEM}-${ARCH}${QUIC}.tar.gz"
