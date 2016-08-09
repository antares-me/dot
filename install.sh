#!/bin/sh

# if [ "$(whoami)" != "root"  ]; then
#     echo "Sorry, you are not root."
#     exit 1
# fi

if [ "$(id -u)" != "0"  ]; then
    echo "Sorry, you are not root."
    exit 1
fi

apt update && apt -y upgrade && apt -y install awesome awesome-extra lightdm \
    lightdm-webkit-greeter xterm vim vim-nox emacs ranger git cloc \
    libncurses5-dev libncursesw5-dev chromium-browser firefox firefox-locale-ru \
    preload prelink
mkdir -p ~/.config/autostart/
ln -s /usr/share/xsessions/awesome.desktop ~/.config/autostart/
git clone https://github.com/antares-me/dotfiles/
service lightdm restart
chsh -s /bin/zsh
prelink -amfR
