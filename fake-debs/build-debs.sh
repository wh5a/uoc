#!/bin/bash

names=(coreutils libgcc1 libstdc++6 libc6 libgmp3c2 zlib1g debianutils libncurses5)

for name in ${names[@]}
do
    ../tools/FakePkg $name
    sudo dpkg -i "$name.deb"
done
