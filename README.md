# dotfiles

Для корректной работы необходимо установить дополнительные пакеты:
- awesome
- [cowsay][cowsay]
- git
- prelink
- [preload][preload]
- ranger
- stow
- tmux
- vim
- w3m
- zsh

`sudo apt-get install cowsay ranger vim vim-nox awesome awesome-extra git zsh
zsh-common w3m w3m-img curl tmux lightdm ubuntu-session xterm emacs cloc
libncurses5-dev libncursesw5-dev terminology libssl-dev xserver-xephyr`

## Установка

Для корректной работы всех настроек необходимо установить и настроить
сопутствующие библиотеки и программы

### [Awesome](awesome)
### LightDM
### xTerm
### Preload
Preload - это демон, который работает в фоновом режиме, и определяет наиболее 
часто используемые приложения и хранит их в кэше, благодаря этому они быстрее 
запускаются. Демон кеширует часто запускаемые программы и библиотеки в памяти 
системы. После некоторого использования демон составляет список  часто 
запускаемых программ и будет держать их в памяти для повторных запусков.
`sudo apt install preload`

### Prelink
Prelink преобразовывает разделяемые библиотеки и выполняемые файлы таким 
образом, чтобы уменьшить количество требуемых перераспределений памяти при 
разрешении зависимостей и, таким образом, ускоряет запуск программ.
`sudo apt install prelink`

## Настройки

### Preload
Редактирование файла настроек
`sudo vim /etc/preload.conf`

Просмотр логов демона
`sudo tail -f /var/log/preload.log`

[cowsay]:https://ru.wikipedia.org/wiki/Cowsay
[preload]:http://preload.sourceforge.net/
