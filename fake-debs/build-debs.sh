#!/bin/bash

names=(coreutils libgcc1 libstdc++6 libc6 libgmp3c2 zlib1g debianutils libncurses5 \
       bsdutils libdb4.7 libbz2-1.0 libreadline6 base-files passwd \
       ca-certificates libgcrypt11 libpam0g libpam-modules consolekit dbus libpng12-0 \
       libsm6 libx11-6 libxcursor1 libxext6 libxft2 libxkbfile1 libxmu6 libxmuu1 libxrender1 libxt6 \
       e2fsprogs libblkid1 libdbus-1-3 libuuid1 mount sed ncurses-bin upstart x11-common \
       file libasound2 libjpeg62 libmagic1 libncursesw5 libsqlite3-0 libssl0.9.8 \
       libudev0 net-tools \
      )

for name in ${names[@]}
do
    ../tools/FakePkg $name
#    sudo dpkg -i "$name.deb"
done
