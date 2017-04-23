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
    abook \
    alsa \
    awesome \
    awesome-extra \
    chromium-browser \
    cloc \
    cowsay \
    dunst \
    emacs \
    festival \
    festvox-ru \
    fim \
    firefox \
    firefox-locale-ru \
    git \
    git-extras \
    hddtemp \
    htop \
    kbdd \
    libncurses5-dev \
    libncursesw5-dev \
    libnotify4 \
    libnotify-bin \
    lightdm \
    lightdm-webkit-greeter \
    mc \
    mediainfo \
    pass \
    prelink \
    preload \
    ranger \
    rxvt-unicode-256color \
    scrot \
    silversearcher-ag \
    stow \
    termonology \
    tmux \
    vim \
    vim-nox \
    vim-nox-py2 \
    vlc \
    w3m \
    wicd \
    wicd-curses \
    wicd-cli \
    xterm \
    zathura \
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

pip install img2txt.py

sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
