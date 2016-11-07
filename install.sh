#!/bin/sh

# if [ "$(whoami)" != "root"  ]; then
#     echo "Sorry, you are not root."
#     exit 1
# fi

# Для снятия блокировки необходимо выполнить
# sudo fuser -vki /var/lib/dpkg/lock

if [ "$(id -u)" != "0"  ]; then
    echo "Sorry, you are not root."
    exit 1
fi

apt update && apt -y upgrade && apt -y install \
    awesome \
    awesome-extra \
    chromium-browser \
    cloc \
    emacs \
    fim \
    firefox \
    firefox-locale-ru \
    git \
#    libncurses5-dev \
#    libncursesw5-dev \
    lightdm \
    lightdm-webkit-greeter \
    mc \
    pass \
    prelink \
    preload \
    ranger \
    silversearcher-ag \
    stow \
    termonology \
    tmux \
    xterm \
    vim \
    vim-nox \
    vim-nox-py2 \
    vlc \
    zram-config \
    zsh

git clone https://github.com/antares-me/dotfiles/
cd ./dotfiles
cp ./lightdm/lightdm.conf /etc/lightdm/lightdm.conf
chsh -s /bin/zsh
prelink -amfR
service lightdm restart

#Для сборки debugclient понадобится libtool-bin

#Doom
#sudo apt install prboom-plus doom-wad-shareware
