# dotfiles

Для корректной работы необходимо установить дополнительные пакеты:
- awesome
- [cowsay][cowsay]
- git
- [lightdm][lightdm]
- prelink
- [preload][preload]
- ranger
- [silversearcher-ag][silversearcher-ag]
- stow
- [tmux][tmux]
- vim
- w3m
- [zsh][zsh]
- ledger

`sudo apt-get install cowsay ranger vim vim-nox awesome awesome-extra git zsh
zsh-common w3m w3m-img curl tmux lightdm ubuntu-session xterm emacs
emacs-goodies-el cloc libncurses5-dev libncursesw5-dev terminology libssl-dev
xserver-xephyr silversearcher-ag ledger libnotify-bin`

## Установка

Для корректной работы всех настроек необходимо установить и настроить
сопутствующие библиотеки и программы

### [Awesome](awesome)
### LightDM
LightDM — это дисплейный менеджер X, который стремится быть лёгким, быстрым,
расширяемым и поддерживающим множество рабочих столов. Фронтенд Unity Greeter
из состава Ubuntu использует WebKit для отображения основанного на HTML
интерфейса входа в систему.
`sudo apt install lightdm`

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

### The Silver Searcher
Инструмент поиска кода, похожий на `ack`, но более скоростной.
`sudo apt install silversearcher-ag`

### [Tmux](tmux)
tmux — свободная консольная утилита-мультиплексор, предоставляющая пользователю
доступ к нескольким терминалам в рамках одного экрана. tmux может быть отключен
от экрана: в этом случае он продолжит исполняться в фоновом режиме; имеется
возможность вновь подключиться к tmux, находящемуся в фоне. tmux является
штатным мультиплексором терминалов операционной системы OpenBSD. Программа tmux
задумывалась как замена программы GNU Screen.
`sudo apt install tmux`

### [ZSH](zsh)
Z shell, zsh — одна из современных командных оболочек UNIX, можетhell, использоваться как
интерактивная оболочка, либо как мощный скриптовой интерпретатор. Zsh является
расширенным bourne shell с большим количеством улучшений.

## Настройки

### Preload
Редактирование файла настроек
`sudo vim /etc/preload.conf`

Просмотр логов демона
`sudo tail -f /var/log/preload.log`

### Zsh
`sudo usermod eax -s /usr/bin/zsh`
`chsh -s /bin/zsh your_user`

[cowsay]:https://ru.wikipedia.org/wiki/Cowsay
[lightdm]:https://ru.wikipedia.org/wiki/LightDM
[preload]:http://preload.sourceforge.net/
[tmux]:https://ru.wikipedia.org/wiki/Tmux
[zsh]:https://ru.wikipedia.org/wiki/Zsh
[silversearcher-ag]:https://github.com/ggreer/the_silver_searcher
