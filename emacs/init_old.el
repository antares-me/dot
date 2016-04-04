
;; Это хук для запутывания нового =~/.emacs.d/init.el= каждый раз при изменении
;; файла

;; оригинал взят с
;; https://github.com/larstvei/dot-emacs/blob/master/init.org
(defun antares-tangle-init ()
  "Если текущий буфер 'readme.org' блоки кода запутываются, и запутанный файл
   компилируется."
  (when (or
         (equal (buffer-file-name)
                (expand-file-name (concat user-emacs-directory "readme.org")))
         (equal (buffer-file-name)
                (expand-file-name "~/gitRepos/dotfiles/emacs/readme.org")))
    (call-process-shell-command
     "emacs ~/.emacs.d/readme.org --batch --eval='(org-babel-tangle)' && notify-send -a 'Emacs' 'init file tangled'" nil 0)))
;; (byte-compile-file (concat user-emacs-directory "init.el")))

(add-hook 'after-save-hook 'antares-tangle-init)

;; Репозитории

;; ELPA репозитории, откуда берутся пакеты

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; use-package и bind-key

;; Макрос [[https://github.com/jwiegley/use-package][use-package]] позволяет
;; использовать изолированные конфигурации пакетов в наших настройках emacs
;; таким образом, это становится более производительно и, ну, просто аккуратно.
;; А это позволяет нам установить автоматически те пакеты, которые еще не
;; установлены (с использованием ключевого слова =:ensure t=) и освобождая нас
;; от использования пользовательского процесса начальной загрузки.

;; Он поставляется также с модулем =bind-key=, который помогает нам управлять
;; привязками клавиш более простоым способом. С помощью этих двух утилит,
;; работающих совместно, мы можем установить пакеты атомарно, как острова,
;; будучи в состоянии добавить/отключить/удалить пакеты, не вмешиваясь в другие.

;; Во избежание проблем с файлами более новые, чем их байт скомпилированные
;; аналоги, лучше более медленный запуск, чем загрузка устаревших и, возможно,
;; сломанных пакетов
(setq load-prefer-newer t)
;; Инициализация пакетов и создание списка пакетов если он не существует
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Установка use-package если не существует
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; Если используется :diminish
(require 'bind-key)                ;; Если используется другой :bind variant

;; Сообщения отладки с метками времени

;; Временные метки (timestamp) в *Messages*
;; via https://www.reddit.com/r/emacs/comments/3hagxf/how_to_automatically_timestamp_messages_in/
(defun current-time-microseconds ()
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d] " now-ms))))

(defadvice message (before test-symbol activate)
  (if (not (string-equal (ad-get-arg 0) "%s%s"))
      (let ((inhibit-read-only t)
            (deactivate-mark nil))
        (with-current-buffer (messages-buffer)
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert (current-time-microseconds))))))

;; Некоторые настройки по умолчанию

;; Значения по умолчанию, которые я считаю хорошим началом.

(setq inhibit-startup-screen t                ;; Убираем экран приверствия
      initial-scratch-message nil             ;; Убираем сообщения из scratch буфера
      visible-bell t                          ;; Убираем пищалки
      apropos-do-all t                        ;; Вспомогательные команды выполняют более обширные поиски, чем по умолчанию
      large-file-warning-threshold 100000000) ;; Предупреждать только если открываемый файл больше 100MB
;; Убираем менюшки и панельки
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Заменяем вопросы yes/no на y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; Показывать пустые строки в конце (внизу) буфера
(toggle-indicate-empty-lines)
;; Удалить предыдущий выбор если он перезаписывается новой вставкой
(delete-selection-mode)
;; Мигающий курсор раздражает. Отключим его.
(blink-cursor-mode -1)
;; Более тонкие границы окон
(fringe-mode '(1 . 1))

;; Использование ibuffer по умолчанию
(defalias 'list-buffers 'ibuffer)

 ;; Убедимся что UTF-8 используется везде
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

;; Отключим автосохранение файлов и бэкапы
;; Я предпочитаю использовать дерево отмен (undo-tree) с ветвлением вместо
;; автосохранения файлов. Так как я использую gpg для авторизации и подписи
;; файлов, более безопасно не использовать резервные копии этих файлов.
;; Используйте DCVS и регулярно бэкапьте файлы!
(setq auto-save-default nil
      auto-save-list-file-prefix nil
      make-backup-files nil)

 ;; Всегда использовать для отступов пробелы
(setq-default indent-tabs-mode  nil
              default-tab-width 4
              c-basic-offset 4)

;; Подсвечивать парные скобки когда курсор находится над одной из них
(setq show-paren-delay 0)
(show-paren-mode t)

;; Подсвечивать текущую строку
(global-hl-line-mode 1)

 ;; Настройки строки режима
(column-number-mode t)
(setq size-indication-mode t)
(which-function-mode 1)

;; backward-kill-word альтернатива Backspace:
;; Удалить слово целиком вместо нескольких нажатий Backspace
;; Для этого привяжем =backward-kill-region= к комбинации =C-w=
(global-set-key "\C-w" 'backward-kill-word)
;; Теперь перепривяжем оригинальные биндинги этой комбинации к новым
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Перенос текстапри 80 колонках по умолчанию (только текст)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
          '(lambda() (set-fill-column 80)))

;; Настройки браузера
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;; Отключим предупреждения о тесноте
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Установим календарь на стандарты моей страны и города
(setq-default calendar-week-start-day  1
              calendar-latitude        51.5
              calendar-longitude       46.0
              calendar-location-name   "Саратов, Россия")

 ;; Установим информацию о пользователе по умолчанию.
(setq user-full-name    "Anton Salnikov"
      user-mail-address "antares@antares.me")

;; Временные файлы

;; Я люблю хранить все временные файлы и папки (cache, backups, ...) в
;; уникальных директориях. Так чище, меньше ошибок и проще управлять.

;; Сначала создадим переменную, в которую поместим путь к этой директории и если
;; она не существует создадим её.

(defvar antares-emacs-temporal-directory (concat user-emacs-directory "tmp/"))
(unless (file-exists-p antares-emacs-temporal-directory)
  (make-directory antares-emacs-temporal-directory))

;; Сохраним все временные файлы во временных каталогах вместотого, чтобы плодить
;; их в $HOME директории

(setq-default
 ;; Tramp история
 tramp-persistency-file-name (concat antares-emacs-temporal-directory "tramp")
 ;; Файл закладок
 bookmark-default-file (concat antares-emacs-temporal-directory "bookmarks")
 ;; Файлы SemanticDB
 semanticdb-default-save-directory (concat antares-emacs-temporal-directory "semanticdb")
 ;; Файлы ссылок
 url-configuration-directory (concat antares-emacs-temporal-directory "url")
 ;; eshell файлы
 eshell-directory-name (concat antares-emacs-temporal-directory "eshell" ))

;; История

;; Поддерживать историю прошлых действий в списке с разумными пределами.

(setq-default history-length 1000)
(setq savehist-file (concat antares-emacs-temporal-directory "history")
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(savehist-mode t)

;; Недавние файлы

;; Recentf - это второстепенный режим, который строит список недавно открытых
;; файлов. Этот список автоматически сохраняется во время сеанса Emacs.
;; Вы можете получить доступ к этому списку через меню.

(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat antares-emacs-temporal-directory "recentf")
          recentf-max-saved-items 100
          recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG"))
    (recentf-mode t)))

;; Сохранить сессию между запусками Emacs (Desktop)

;; Desktop Save Mode - функция сохранения состояния Emacs от одного сеанса
;; к другому.

;; У меня отключен пока эта конфигурация не стабильна
(use-package desktop
  :config
  :disabled t
  (progn
    (setq desktop-path '("~/.emacs.d/tmp/"))
    (setq desktop-dirname "~/.emacs.d/tmp/")
    (setq desktop-base-file-name "emacs-desktop")
    (setq desktop-globals-to-save
          (append '((extended-command-history . 50)
                    (file-name-history . 200)
                    (grep-history . 50)
                    (compile-history . 50)
                    (minibuffer-history . 100)
                    (query-replace-history . 100)
                    (read-expression-history . 100)
                    (regexp-history . 100)
                    (regexp-search-ring . 100)
                    (search-ring . 50)
                    (shell-command-history . 50)
                    tags-file-name
                    register-alist)))
    (desktop-save-mode 1)))

;; Сохранение позиции курсора между сеансами

;; Сохранить позицию курсора для каждого открытого файла. Так при повторном
;; открытии файла, курсор будет в той позиции, в которой вы последний раз его
;; открыли.

(use-package saveplace
  :config
  (progn
    (setq save-place-file (concat antares-emacs-temporal-directory "saveplace.el") )
    (setq-default save-place t)))

;; Цветовая тема

;; Определяем тему по умолчанию.
;; Конфигурация работает в терминальном/графическом режиме
;; Конфигурация работает в режиме клиент/серверного и автономном

;; *Не забудьте: когда тестируете новую тему, сначала отключите текущую или
;; используйте =helm-themes=.*

;; Этот код служит для избежания перезагрузки темы каждый раз, когда вы
;; открываете новый клиент в режиме сервера (с GUI или из терминала)

(defvar antares-color-theme (if (package-installed-p 'monokai-theme)
                            'monokai
                          'tango))

(setq myGraphicModeHash (make-hash-table :test 'equal :size 2))
(puthash "gui" t myGraphicModeHash)
(puthash "term" t myGraphicModeHash)

(defun emacsclient-setup-theme-function (frame)
  (let ((gui (gethash "gui" myGraphicModeHash))
        (ter (gethash "term" myGraphicModeHash)))
    (progn
      (select-frame frame)
      (when (or gui ter)
        (progn
          (load-theme antares-color-theme t)
          (if (display-graphic-p)
              (puthash "gui" nil myGraphicModeHash)
            (puthash "term" nil myGraphicModeHash))))
      (when (not (and gui ter))
        (remove-hook 'after-make-frame-functions 'emacsclient-setup-theme-function)))))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'emacsclient-setup-theme-function)
  (progn (load-theme antares-color-theme t)))

;; Шрифт

;; Используемый шривт. Я выбрал моноширинный и /Dejavu Sans Mono/ потому что
;; он свободный и имеет хорошую поддержку Юникода, да и выглядит неплохо!

(set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 110)

;; Установим шрифт с хорошей поддержкой символов юникод, чтобы использовать его
;; в случае отсутствия определённых символов в выбранном шрифте
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

;; Улучшенные номера строк

;; Отображает номера строк в более привлекательном виде. Я не слишком часто их
;; использую так как это слишком медленная функция, но иногда она удобна.

;; Оказывается можно получить нумерацию строк как в Vim!
;; http://www.emacswiki.org/emacs/LineNumbers#toc6
(unless window-system
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local linum-format-fmt
                          (let ((w (length (number-to-string
                                            (count-lines (point-min) (point-max))))))
                            (concat "%" (number-to-string w) "d"))))))

(defun antares-linum-format-func (line)
   (concat
    (propertize (format linum-format-fmt line) 'face 'linum)
    (propertize " " 'face 'linum)))

(unless window-system
  (setq linum-format 'antares-linum-format-func))

;; Переключение отображения завершающих пробелов

;; Показать/скрыть завершающие пробелы в буфере.

;; Из http://stackoverflow.com/a/11701899/634816
(defun antares-toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; Завершение внутренних процессов с помощью буфера =list process=

;; Добавим функционал для завершения процессов непосредственно в буфере
;; =list process=

;; Взято с http://stackoverflow.com/a/18034042
(defun antares-delete-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))

(define-key process-menu-mode-map (kbd "C-c k") 'antares-delete-process-at-point)

;; Перемещение окон

;; Обеспечивает более интуитивные перемещения окон.

(defun antares-scroll-other-window()
  (interactive)
  (scroll-other-window 1))

(defun antares-scroll-other-window-down ()
  (interactive)
  (scroll-other-window-down 1))

(use-package windmove)
(use-package winner
  :config
  (winner-mode t))

;; Вспомогательные функции для управления буферами

;; Некоторые пользовательские функции для управления буферами.

(defun antares-alternate-buffers ()
  "Toggle between the last two buffers"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun antares-revert-buffer ()
  "Revert the buffer to the save disk file state"
  (interactive)
  (revert-buffer nil t))

(defun antares-kill-this-buffer ()
  "Kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(defun antares-diff-buffer-with-file ()
  "Compare the current modified buffer with the saved version."
  (interactive)
  (let ((diff-switches "-u"))
    (diff-buffer-with-file (current-buffer))))

;; Использовать шифрование

;; Используем шифрование для защиты конфиденциальных данных, например
;; конфигурации почтовых серверов (хранятся в =authinfi.gpg=) и другой важной
;; пользовательской информации

(use-package epa-file
  :config
  (progn
    (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))))

;; Проверка орфографии

;; Активация проверки орфографии по умолчанию. Также используется
;; [[http://hunspell.sourceforge.net/][hunspell]] вместо
;; [[http://www.gnu.org/software/ispell/ispell.html][ispell]] в качестве
;; коннектора.

(setq-default ispell-program-name    "hunspell"
              ispell-really-hunspell t
              ispell-check-comments  t
              ispell-extra-args      '("-i" "utf-8") ;; много шума, отключить?
              ispell-dictionary      "en_US")

;; Переключение между наиболее часто используемыми словарями
(defun antares-switch-dictionary ()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "en_US") "ru_RU" "en_US")))
    (ispell-change-dictionary change)
    (message "Переключен словарь с %s на %s" dic change)))

(defun antares-turn-on-spell-check ()
  (flyspell-mode 1))

;; Включить проверку орфографии в некоторых режимах
(add-hook 'markdown-mode-hook 'antares-turn-on-spell-check)
(add-hook 'text-mode-hook     'antares-turn-on-spell-check)
(add-hook 'org-mode-hook      'antares-turn-on-spell-check)
(add-hook 'prog-mode-hook     'flyspell-prog-mode)

;; Dired

;; Есть два способа, чтобы избежать использовать более одного буфера при
;; использовании Dired.

(use-package dired
   :init
   ;; человеко-читаемые размеры файлов
   (setq dired-listing-switches "-alh")
   ;; 'a' использовать заново текущий буфер, 'RET' открыть новый
   (put 'dired-find-alternate-file 'disabled nil)

   ;; '^' использовать текущий буфер
   (add-hook 'dired-mode-hook
             (lambda ()
               (define-key dired-mode-map (kbd "^")
                 (lambda ()
                   (interactive)
                   (find-alternate-file ".."))))))

;; Ido

;; Используем ido, чтобы работать с файлами и буферами более удобно.

(use-package ido
  :config
  (progn
    (setq ido-save-directory-list-file (concat antares-emacs-temporal-directory "ido.last")
          ido-enable-flex-matching t
          ido-use-virtual-buffers t)
    ;; (ido-mode t)
    (ido-everywhere t)))

;; ediff

;; Более правильная конфигурация ediff по умолчанию.

(use-package ediff
  :init
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;; eww

;; Настройки для Emacs Web Browser.

(use-package eww
  :init
  (setq eww-download-directory "~/temporal")
  :config
  (bind-keys :map eww-mode-map
             ("s" . eww-view-source)))

;; Настройки Org-mode

(use-package org
  :defer 1
  :config
  (progn
    ;; Модули используемые по умолчанию
    (setq org-modules '(
        org-bbdb
        org-bibtex
        org-docview
        org-mhe
        org-rmail
        org-crypt
        org-protocol
        org-gnus
        org-id
        org-info
        org-habit
        org-irc
        org-annotate-file
        org-eval
        org-expiry
        org-man
        org-panel
        org-toc))

    ;; Директории используемые по умолчанию
    (setq org-directory "~/org"
          org-default-notes-file (concat org-directory "/notes.org"))

    ;; Установка архива
    (setq org-archive-location "~/org/archive/%s_archive::datetree/** Archived")
    (setq org-agenda-custom-commands
          '(("Q" . "Custom queries") ;; установка метки для "Q"
            ("Qa" "Archive search" search ""
             ((org-agenda-files (file-expand-wildcards "~/org/archive/*.org_archive"))))
            ;; ...тут прочие команды
            ))

    ;; Синтаксис подсветки блоков кода
    (setq org-src-fontify-natively  t
          org-src-tab-acts-natively t)
    (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

    ;; Синтаксис подсветки блоков кода при экспорте в PDF
    ;; Включаем latex-exporter
    (use-package ox-latex)
    ;; Добавляем minted к дефолтным пакетам включаемым при экспорте.
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (add-to-list 'org-latex-packages-alist '("" "xunicode"))
    ;; Указываем latex экспорту использовать пакет minted для раскрашивания кода
    (setq org-latex-listings 'minted)
    ;; Позволим экспортеру использовать настройку -shell-escape, чтобы позволить
    ;; запускать внешние программы.
    ;; Это, очевидно, может быть опасно для активации!
    (setq org-latex-pdf-process
          '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

    ;; Управление задачами
    (setq org-log-done t)
    (setq org-clock-idle-time nil)

    ;; Повестка дня и дневник
    (setq org-agenda-include-diary t)
    (setq org-agenda-files '("~/org"))
    (setq org-agenda-inhibit-startup t)

    ;; Настройка для открытия файлов во внешних программах
   (setq org-file-apps
          '(("\\.pdf\\'" . "zathura %s")
            ("\\.gnumeric\\'" . "gnumeric %s")))

    ;; Защита скрытых деревьев от случайного удаления (не работает с evil)
    (setq-default org-catch-invisible-edits  'error
                  org-ctrl-k-protect-subtree 'error)

    ;; Показывать картинки в тексте
    ;; Работает только в GUI, но это неплохая фишка и её можно использовать
    (when (window-system)
      (setq org-startup-with-inline-images t))
    ;; ограничение ширины изображения
    (setq org-image-actual-width '(800))

    ;; :::::: Org-Babel ::::::

    ;; Поддержка языков
    (org-babel-do-load-languages
     (quote org-babel-load-languages)
     (quote (
             (calc . t)
             (clojure . t)
             (ditaa . t)
             (dot . t)
             (emacs-lisp . t)
             (gnuplot . t)
             (latex . t)
             (ledger . t)
             (octave . t)
             (org . t)
             (makefile . t)
             (plantuml . t)
             (python . t)
             (R . t)
             (ruby . t)
             (sh . t)
             (sqlite . t)
             (sql . nil))))
    (setq org-babel-python-command "python2")

    ;; Обновить картинки после выполнения
    (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

    ;; Не спрашивать подтверждения для запуска "безопасных" языков
    (defun antares-org-confirm-babel-evaluate (lang body)
      (and (not (string= lang "ditaa"))
         (not (string= lang "dot"))
         (not (string= lang "gnuplot"))
         (not (string= lang "ledger"))
         (not (string= lang "plantuml"))))

    (setq org-confirm-babel-evaluate 'antares-org-confirm-babel-evaluate)))

;; ag

;; [[./img/ag.png]]

;; [[https://github.com/Wilfred/ag.el][ag.el]] простой интерфейс для Emacs к ag,
;; ("the silver searcher" замена ack).

(use-package ag
  :ensure t
  :defer 1
  :config
  (progn
    (setq ag-reuse-buffers 't
          ag-highlight-search t
          ag-arguments (list "--color" "--smart-case" "--nogroup" "--column" "--all-types" "--"))))

;; async

;; [[https://github.com/jwiegley/emacs-async][async.el]] модуль для выполнения
;; асинхронной обработки в Emacs.

(use-package async
  :defer t
  :ensure t)

;; auto-complete

;; [[./img/auto_complete.png]]

;; [[https://github.com/auto-complete/auto-complete][Auto Complete Mode]]
;; (aka =auto-complete.el=, =auto-complete-mode=) расширение, автоматизирующее и
;; дополняющее вводимые команды.

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config
  (progn
    (global-auto-complete-mode)
    (add-to-list 'ac-sources 'ac-source-abbrev)
    (add-to-list 'ac-sources 'ac-source-dictionary)
    (add-to-list 'ac-sources 'ac-source-filename)
    (add-to-list 'ac-sources 'ac-source-imenu)
    (add-to-list 'ac-sources 'ac-source-semantic)
    (add-to-list 'ac-sources 'ac-source-words-in-buffer)
    (add-to-list 'ac-sources 'ac-source-yasnippet)
    (bind-keys :map ac-menu-map
               ("\C-n" . ac-next)
               ("\C-p" . ac-previous))
    (setq ac-use-menu-map t
          ac-ignore-case 'smart
          ac-auto-start 2)
    (ac-flyspell-workaround))

  ;; Файл в котором хранится история автодополнения.
  (setq ac-comphist-file (concat user-emacs-directory
                                 "temp/ac-comphist.dat"))

  ;; Грязный хак для использования AC везде
  (define-globalized-minor-mode real-global-auto-complete-mode
    auto-complete-mode (lambda ()
                         (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))
                         ))
  (real-global-auto-complete-mode t))

;; avy

;; [[./img/avy.png]]

;; [[https://github.com/abo-abo/avy][avy]] - GNU Emacs пакет для перехода к
;; видимому тексту, используя символьное дерево решений.

;; [[./img/ace_link.png]]

;; [[https://github.com/abo-abo/ace-link][ace-link]] - пакет Emacs для выбора
;; ссылки для перехода.
;; Работает в org-mode, info, help и eww.

;; | Binding | Call       | Do           |
;; |---------+------------+--------------|
;; | o       | ace-link-* | jump to link |
;; |---------+------------+--------------|


;; [[./img/ace_window.png]]

;; [[https://github.com/abo-abo/ace-window][ace-window]] - пакет для выбора окна
;; для переключения. Также может использоваться для перехода по словам, строкам,
;; символам, удаляет/перемещает/копирует строки и делает другие интересные вещи.

(use-package avy
      :ensure t
      :config
      (setq avy-keys       '(?a ?s ?d ?e ?f ?g ?r ?v ?h ?j ?k ?l ?n ?m ?u)
            avy-background t
            avy-all-windows t
            avy-style 'at-full
            avy-case-fold-search nil)
      (set-face-attribute 'avy-lead-face nil :foreground "gold" :weight 'bold :background nil)
      (set-face-attribute 'avy-lead-face-0 nil :foreground "deep sky blue" :weight 'bold :background nil)
      (use-package ace-link
        :ensure t
        :defer 1
        :config
        (ace-link-setup-default))
      (use-package ace-window
        :ensure t
        :defer 1
        :config
        (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 2.0)
        (set-face-attribute 'aw-mode-line-face nil :inherit 'mode-line-buffer-id :foreground "lawn green")
        (setq aw-keys   '(?a ?s ?d ?f ?j ?k ?l)
              aw-dispatch-always t
              aw-dispatch-alist
              '((?x aw-delete-window     "Ace - Delete Window")
                (?c aw-swap-window       "Ace - Swap Window")
                (?n aw-flip-window)
                (?h aw-split-window-vert "Ace - Split Vert Window")
                (?v aw-split-window-horz "Ace - Split Horz Window")
                (?m delete-other-windows "Ace - Maximize Window")
                (?g delete-other-windows)
                (?b balance-windows)
                (?u winner-undo)
                (?r winner-redo)))

        (when (package-installed-p 'hydra)
          (defhydra hydra-window-size (:color red)
            "Windows size"
            ("h" shrink-window-horizontally "shrink horizontal")
            ("j" shrink-window "shrink vertical")
            ("k" enlarge-window "enlarge vertical")
            ("l" enlarge-window-horizontally "enlarge horizontal"))
          (defhydra hydra-window-frame (:color red)
            "Frame"
            ("f" make-frame "new frame")
            ("x" delete-frame "delete frame"))
          (defhydra hydra-window-scroll (:color red)
            "Scroll other window"
            ("n" joe-scroll-other-window "scroll")
            ("p" joe-scroll-other-window-down "scroll down"))
          (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
          (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
          (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
        (ace-window-display-mode t)))

;; beacon

;; [[https://github.com/Malabarba/beacon][Beacon]] - вспомогательный режим,
;; помогающий проще искать курсор.

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-push-mark 35
        beacon-push-mark 35
        beacon-blink-when-focused t
        beacon-color "deep sky blue"))

;; boxquote

;; [[./img/boxquote.png]]

;; [[https://github.com/davep/boxquote.el/blob/master/boxquote.el][boxquote.el]]
;; Предоставляет набор функций для использования цитируемого текста, с
;; объединением показаным в левой части текстовой области. Такой стиль
;; маркировки может быть использован, чтобы показать включаемый внешний текст
;; или пример кода.

;; Так выглядит boxquote:
;; #+BEGIN_EXAMPLE
;; ╭────[ Lorem ipsum ]
;; │ Nullam eu ante vel est convallis dignissim.  Fusce suscipit, wisi nec facilisis
;; │ facilisis, est dui fermentum leo, quis tempor ligula erat quis odio.  Nunc porta
;; │ vulputate tellus.  Nunc rutrum turpis sed pede.  Sed bibendum.  Aliquam posuere.
;; │ Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis
;; │ varius mi purus non odio.  Pellentesque condimentum, magna ut suscipit
;; │ hendrerit, ipsum augue ornare nulla, non luctus diam neque sit amet urna.
;; ╰────
;; #+END_EXAMPLE

(use-package boxquote
  :ensure t
  :defer t
  :config
  (setq-default  boxquote-bottom-corner "╰"      ; U+2570
                 boxquote-side          "│ "     ; U+2572 + space
                 boxquote-top-and-tail  "────"   ; U+2500 (×4)
                 boxquote-top-corner    "╭")     ; U+256F
  (when (package-installed-p 'hydra)
    (defhydra hydra-boxquote (:color blue :hint nil)
       "
                                                                    ╭──────────┐
  Text           External           Apropos         Do              │ Boxquote │
╭───────────────────────────────────────────────────────────────────┴──────────╯
  [_r_] region        [_f_] file      [_K_] describe-key        [_t_] title
  [_p_] paragraph     [_b_] buffer    [_F_] describe-function   [_u_] unbox
  [_a_] buffer        [_s_] shell     [_V_] describe-variable   [_w_] fill-paragraph
  [_e_] text           ^ ^            [_W_] where-is            [_n_] narrow
  [_d_] defun         [_y_] yank       ^ ^                      [_c_] narrow to content
  [_q_] boxquote      [_Y_] yanked     ^ ^                      [_x_] kill
--------------------------------------------------------------------------------
       "
      ("<esc>" nil "quit")
      ("x" boxquote-kill)
      ("Y" boxquote-yank)
      ("e" boxquote-text)
      ("u" boxquote-unbox)
      ("d" boxquote-defun)
      ("t" boxquote-title)
      ("r" boxquote-region)
      ("a" boxquote-buffer)
      ("q" boxquote-boxquote)
      ("W" boxquote-where-is)
      ("p" boxquote-paragraph)
      ("f" boxquote-insert-file)
      ("K" boxquote-describe-key)
      ("s" boxquote-shell-command)
      ("b" boxquote-insert-buffer)
      ("y" boxquote-kill-ring-save)
      ("w" boxquote-fill-paragraph)
      ("F" boxquote-describe-function)
      ("V" boxquote-describe-variable)
      ("n" boxquote-narrow-to-boxquote)
      ("c" boxquote-narrow-to-boxquote-content))))

;; bug-hunter

;; [[./img/bug_hunter.png]]

;; [[https://github.com/Malabarba/elisp-bug-hunter][The Bug Hunter]]
;; Библиотека  Emacs, которая находит источники ошибок или неожиданного
;; поведения внутри конфигурационного файла elisp (обычно =init.el= или
;; =.emacs=).

(use-package bug-hunter
  :ensure t
  :commands (bug-hunter-file bug-hunter-init-file))

;; calfw

;; [[./img/cfw_calendar.png]]

;; [[https://github.com/kiwanami/emacs-calfw][Calfw]] - программа для вывода
;; календаря в буфер Emacs.

(use-package calfw
  :commands cfw:open-org-calendar
  :defer 0.5
  :ensure t
  :config
  (progn
    (use-package calfw-org)
    ;; Unicode characters
    (setq cfw:fchar-junction ?╋
          cfw:fchar-vertical-line ?┃
          cfw:fchar-horizontal-line ?━
          cfw:fchar-left-junction ?┣
          cfw:fchar-right-junction ?┫
          cfw:fchar-top-junction ?┯
          cfw:fchar-top-left-corner ?┏
          cfw:fchar-top-right-corner ?┓)))

;; charmap

;; [[./img/charmap.png]]

;; [[https://github.com/lateau/charmap][Charmap]] - просмотрщик Unicode таблицы
;; для Emacs. С помощью CharMap вы можете просмотреть таблицу Unicode основанную
;; на стандарте The Unicode Standard 6.2.

(use-package charmap
  :commands charmap
  :defer t
  :ensure t
  :config
  (setq charmap-text-scale-adjust 2))

;; TODO chess

;; [[./img/chess.png]]

;; [[https://github.com/jwiegley/emacs-chess][Chess.el]] клиент и библиотека
;; шахмат к Emacs, разработанные для написания шахматных программ или для игр в
;; шахматы против различных движков, в том числе интернет сервисов. Библиотека
;; может быть использована для анализа вариаций, используя просмотр исторических
;; игр, или множества других целей.

(use-package chess
  :ensure t
  :commands chess
  :config
  (setq chess-images-default-size 70
        chess-images-separate-frame nil))

;; cloc

;; [[./img/cloc.png]]

;; [[https://github.com/cosmicexplorer/cloc-emacs][cloc]] Количество строк кода
;; в буфере

(use-package cloc
  :ensure t
  :commands cloc)

;; csv-mode

;; [[https://github.com/emacsmirror/csv-mode][csv-mode]] - основной режим для
;; редактирования значений, разделённых запятой или символом.

;; | Binding | Call                    | Do                                                                       |
;; |---------+-------------------------+--------------------------------------------------------------------------|
;; | C-c C-v | csv-toggle-invisibility | Переключает невидимость разделителей полей при выравнивании              |
;; | C-c C-t | csv-transpose           | Переписать строки (которые могут иметь разную длину) в качестве столбцов |
;; | C-c C-c | csv-set-comment-start   | Set comment start for this CSV mode buffer to STRING                     |
;; | C-c C-u | csv-unalign-fields      | Undo soft alignment and optionally remove redundant white space          |
;; | C-c C-a | csv-align-fields        | Align all the fields in the region to form columns                       |
;; | C-c C-z | csv-yank-as-new-table   | Yank fields as a new table starting at point                             |
;; | C-c C-y | csv-yank-fields         | Yank fields as the ARGth field of each line in the region                |
;; | C-c C-k | csv-kill-fields         | Kill specified fields of each line in the region                         |
;; | C-c C-d | csv-toggle-descending   | Toggle csv descending sort ordering                                      |
;; | C-c C-r | csv-reverse-region      | Reverse the order of the lines in the region                             |
;; | C-c C-n | csv-sort-numeric-fields | Sort lines in region numerically by the ARGth field of each line         |
;; | C-c C-s | csv-sort-fields         | Sort lines in region lexicographically by the ARGth field of each line   |
;; |---------+-------------------------+--------------------------------------------------------------------------|

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

;; define-word

;; [[https://github.com/abo-abo/define-word][define-word]] - пакет GNU Emacs,
;; который позволяет увидеть определение слова или фразы в точке, без
;; необходимости переключаться в браузер.

(use-package define-word
  :ensure t)

;; diff-hl

;; [[https://github.com/dgutov/diff-hl][diff-hl]] highlights uncommitted changes on the left side of the window, allows
;; you to jump between and revert them selectively.

;; | Bind    | Call                   | Do                                                                  |
;; |---------+------------------------+---------------------------------------------------------------------|
;; | C-x v = | diff-hl-diff-goto-hunk | Run VC diff command and go to the line corresponding to the current |
;; | C-x v n | diff-hl-revert-hunk    | Revert the diff hunk with changes at or above the point             |
;; | C-x v [ | diff-hl-previous-hunk  | Go to the beginning of the previous hunk in the current buffer      |
;; | C-x v ] | diff-hl-next-hunk      | Go to the beginning of the next hunk in the current buffer          |
;; |---------+------------------------+---------------------------------------------------------------------|

(use-package diff-hl
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'dired-mode-hook  'diff-hl-dired-mode)
    (add-hook 'org-mode-hook    'turn-on-diff-hl-mode)
    (add-hook 'prog-mode-hook   'turn-on-diff-hl-mode)
    (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)))

