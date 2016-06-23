#!/bin/sh

sudo apt-get install awesome awesome-extra lightdm ubuntu-session xterm vim vim-nox emacs ranger git cloc libncurses5-dev libncursesw5-dev chromium-browser firefox firefox-locale-ru
mkdir -p ~/.config/autostart/
ln -s /usr/share/xsessions/awesome.desktop ~/.config/autostart/
git clone https://github.com/antares-me/dotfiles/
sudo service lightdm restart
chsh -s /bin/zsh
