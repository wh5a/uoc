#!/bin/bash

names=(coreutils libgcc1 libstdc++6 libc6 libgmp3c2 zlib1g debianutils libncurses5 \
       bsdutils libdb4.7 libbz2-1.0 libreadline6 base-files passwd)

for name in ${names[@]}
do
    ../tools/FakePkg $name
#    sudo dpkg -i "$name.deb"
done
