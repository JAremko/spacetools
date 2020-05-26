#!/usr/bin/env bash

pushd "$(dirname "$0")"

if ! hash clj 2>/dev/null; then
    echo "Installing clojure..."
    pushd /tmp
    curl -O https://download.clojure.org/install/linux-install-1.10.1.536.sh
    chmod +x linux-install-1.10.1.536.sh
    sudo ./linux-install-1.10.1.536.sh
    popd
fi

clj -m ticketer.run

popd
