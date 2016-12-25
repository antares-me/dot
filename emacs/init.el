
;; Как это работает
   
;; Используется один из стандартнх init файлов Emacs, =init.el= для загрузки всей
;; конфигурации. Предпологается, что настройки хранятся в стандартном =~/.emacs.d=
;; каталоге и для установки конфигурации вам нужна символическая ссылка с этого
;; каталога =emacs= на него. Файл =~/.emacs.d/init.el= собирается из всех блоков
;; кода этого файла =~/emacs.d/readme.org=, экспортируемых в процессе, который
;; называется "tangling". Если блок помечен как =:tangle no=, он будет пропущен.
;; Tangling происходит автоматически каждый раз при изменении =readme.org=, с
;; помощью хука, чтобы быть уверенным в синхронизации файлов.

;; Это хук для создаёт новый =~/.emacs.d/init.el= каждый раз при изменении этого
;; файла.

;; Оригинал взят отсюда
;; https://github.com/larstvei/dot-emacs/blob/master/init.org
(defun antares-tangle-init ()
  "Если текущий буфер 'readme.org' блоки кода собираются и собранный файл компилируется."
  (when (or
         (equal (buffer-file-name)
                (expand-file-name (concat user-emacs-directory "readme.org")))
         (equal (buffer-file-name)
                (expand-file-name "~/dotfiles/emacs/readme.org")))
    (call-process-shell-command
     "emacs ~/.emacs.d/readme.org --batch --eval='(org-babel-tangle)' && notify-send -i 'emacs' 'Emacs' 'init-файл собран'" nil 0)))
    ;; (byte-compile-file (concat user-emacs-directory "init.el")))

(add-hook 'after-save-hook 'antares-tangle-init)

;; Репозитории

;; Репозитории ELPA, откуда берутся пакеты.

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; use-package и bind-key

;; Макрос [[https://github.com/jwiegley/use-package][use-package]] позволяет использовать изолированные конфигурации пакетов в
;; настройках emacs таким образом, это становится более производительно и, ну,
;; просто аккуратно. А это позволяет нам установить автоматически те пакеты,
;; которые еще не установлены (с использованием ключевого слова =:ensure t=) и
;; освобождая нас от использования пользовательского процесса начальной
;; загрузки.

;; Он поставляется также с модулем =bind-key=, который помогает нам
;; управлять привязками клавиш более простоым способом. С помощью этих двух
;; утилит, работающих совместно, мы можем установить пакеты атомарно, как острова,
;; будучи в состоянии добавить/отключить/удалить пакеты, не вмешиваясь в другие.

;; Во избежание проблем с файлами более новые, чем их байт скомпилированные
;; аналоги, лучше более медленный запуск, чем загрузка устаревших и, возможно,
;; сломанных пакетов.
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
;; Убираем панели и GUI меню
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Заменяем вопросы yes/no на y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; Показывать пустые строки в конце (внизу) буфера
(toggle-indicate-empty-lines)
;; Удалить предыдущий выбор если он перезаписывается новой вставкой
(delete-selection-mode)
;; Мигающий курсор раздражает. Отключим мигание.
(blink-cursor-mode -1)
;; Более тонкие границы окон
(fringe-mode '(1 . 1))

;; Использование ibuffer по умолчанию
(defalias 'list-buffers 'ibuffer)

 ;; Убедимся что UTF-8 используется везде.
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

;; Отключим автосохранение и бэкапы
;; Я предпочитаю использовать дерево отмен (undo-tree) с ветвлением вместо
;; автосохранения файлов. Так как я использую gpg для авторизации и подписи
;; файлов, более безопасно не использовать резервные копии этих файлов.
;; Используйте DCVS и регулярно бэкапьте файлы!      
(setq auto-save-default nil
      auto-save-list-file-prefix nil
      make-backup-files nil)

 ;; Всегда делать отступы пробелами
(setq-default indent-tabs-mode  nil
              default-tab-width 4
              c-basic-offset 4)

;; Подсвечивать парные скобки, когда курсор на одной из них.
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

;; Перенос текста на 80 символе по умолчанию (только текст)
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
(setq user-full-name    "Salnikov Anton"
      user-mail-address "antares@antares.me")

;; Временные файлы

;; Я люблю хранить все временные файлы и папки (cache, backups, ...) в уникальных
;; директориях. Так чище, меньше ошибок и проще управлять.

;; Сначала создадим переменную, в которую поместим путь к этой директории и если
;; она не существует создадим её.

(defvar antares-emacs-temporal-directory (concat user-emacs-directory "tmp/"))
(unless (file-exists-p antares-emacs-temporal-directory)
  (make-directory antares-emacs-temporal-directory))

;; Сохраним все временные файлы во временных каталогах вместотого, чтобы плодить их
;; в $HOME директории.

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
;; файлов. Этот список автоматически сохраняется во время сеанса Emacs. Вы можете
;; получить доступ к этому списку через меню.

(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat antares-emacs-temporal-directory "recentf")
          recentf-max-saved-items 100
          recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG"))
    (recentf-mode t)))

;; Сохранить сессию между запусками Emacs (Desktop)

;; Desktop Save Mode - функция сохранения состояния Emacs от одного сеанса к другому.

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

;; Цветовая схема

;; Тут я устанавливаю тему по-умолчанию, конечно субективное решение. Эта конфигурация
;; работает в терминальном и графическом режиме а также в клиент-серверном и автономном
;; буфере.

;; *Внимание: когда тестируете новую тему, сначала отключите эту
;; или используйте =helm-themes=.*

;; Этот код служит для предотвращения перезагрузки темы каждый раз, когда вы
;; открываете новый клиент в режиме сервера (из GUI или терминала)

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

;; Используемый шрифт. Мой выбор моноширинный /Dejavu Sans Mono/ потому что он
;; свободный и имеет отличную поддержку Юникода, да и выглядит неплохо!

(set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 110)

;; Set a font with great support for Unicode Symbols to fallback in
;; those case where certain Unicode glyphs are missing in the current
;; font.
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

;; Улучшенная нумерация строк

;; Показывает более удобные номера строк. Я не часто использую их потому, что это
;; очень медленная функция, но иногда она удобна.

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

;; Показ завершающих пробелов

;; Показать/скрыть завершающие пробелы в буфере

;; from http://stackoverflow.com/a/11701899/634816
(defun antares-toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; Убить внутренний процесс с помощью =list process= буфера

;; Добавляет функционал возможности завершения процесса прямо в =list process=
;; буфере

;; seen at http://stackoverflow.com/a/18034042
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

;; Предоставляет больше интерактивности в передвижении окон.

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

;; Несколько кастомных функций для управления буферами.

(defun antares-alternate-buffers ()
  "Переключение между последними двумя буферами"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun antares-revert-buffer ()
  "Откат буфера до состояния сохранённого на диске файла"
  (interactive)
  (revert-buffer nil t))

(defun antares-kill-this-buffer ()
  "Удалить текущий буфер"
  (interactive)
  (kill-buffer (current-buffer)))

(defun antares-diff-buffer-with-file ()
  "Сравнить текущий изменённый буфер с сохранённой версией"
  (interactive)
  (let ((diff-switches "-u"))
    (diff-buffer-with-file (current-buffer))))

;; Использование шифрования

;; Использование шифрования для защиты конфиденциальных данных. Таких как
;; конфигурации почтовых серверов (хранятся в =authinfo.gpg=) и пользовательских
;; данных.

(use-package epa-file
  :config
  (progn
    (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))))

;; Правописание

;; Включаем по-умолчанию проверку правописания. Также используем [[http://hunspell.sourceforge.net/][hunspell]] вместо
;; [[http://www.gnu.org/software/ispell/ispell.html][ispell]] для исправлений.

(setq-default ispell-program-name    "hunspell"
              ispell-really-hunspell t
              ispell-check-comments  t
              ispell-extra-args      '("-i" "utf-8") ;; производит много шума, отключить?
              ispell-dictionary      "en_US")

;; Переключение между двумя наиболее часто используемыми словарями
(defun antares-switch-dictionary ()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "en_US") "ru_RU" "en_US")))
    (ispell-change-dictionary change)
    (message "Словарь переключен с %s на %s" dic change)))

(defun antares-turn-on-spell-check ()
  (flyspell-mode 1))

;; enable spell-check in certain modes
(add-hook 'markdown-mode-hook 'antares-turn-on-spell-check)
(add-hook 'text-mode-hook     'antares-turn-on-spell-check)
(add-hook 'org-mode-hook      'antares-turn-on-spell-check)
(add-hook 'prog-mode-hook     'flyspell-prog-mode)

;; Dired

;; Есть два способа, чтобы избежать использование боле одного буфера при
;; использовании Dired.

(use-package dired
   :init
   ;; human-readable sizes
   (setq dired-listing-switches "-alh")
   ;; 'a' использовать текущий буфер, 'RET' открыть новый
   (put 'dired-find-alternate-file 'disabled nil)

   ;; '^' использовать текущий буфер
   (add-hook 'dired-mode-hook
             (lambda ()
               (define-key dired-mode-map (kbd "^")
                 (lambda ()
                   (interactive)
                   (find-alternate-file ".."))))))

;; Ido

;; Используем ido для работы с файлами и буферами удобным способом.

(use-package ido
  :config
  (progn
    (setq ido-save-directory-list-file (concat antares-emacs-temporal-directory "ido.last")
          ido-enable-flex-matching t
          ido-use-virtual-buffers t)
    ;; (ido-mode t)
    (ido-everywhere t)))

;; ediff

;; Более удобная конфигурация ediff по умолчанию.

(use-package ediff
  :init
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;; eww

;; Настройки Emacs Web Browser.

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
    ;; укажем модули, включённые по умолчанию
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

    ;; Настройка директории по умолчанию
    (setq org-directory "~/MEGA/org"
          org-default-notes-file (concat org-directory "/notes.org"))

    ;; Настройка архива
    (setq org-archive-location "~/MEGA/org/archive/%s_archive::datetree/** Archived")
    (setq org-agenda-custom-commands
          '(("Q" . "Custom queries") ;; gives label to "Q"
            ("Qa" "Archive search" search ""
             ((org-agenda-files (file-expand-wildcards "~/MEGA/org/archive/*.org_archive"))))
            ;; ...Тут прочие команды
            ))

    ;; Подсветка синтаксиса в блоках кода
    (setq org-src-fontify-natively  t
          org-src-tab-acts-natively t)
    (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

    ;; Подсветка синтаксиса в блоках кода при экспорте в PDF
    ;; Подключим latex-exporter
    (use-package ox-latex)
    ;; Add minted to the defaults packages to include when exporting.
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (add-to-list 'org-latex-packages-alist '("" "xunicode"))
    ;; Tell the latex export to use the minted package for source
    ;; code coloration.
    (setq org-latex-listings 'minted)
    ;; Let the exporter use the -shell-escape option to let latex
    ;; execute external programs.
    ;; This obviously and can be dangerous to activate!
    (setq org-latex-pdf-process
          '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

    ;; Управление задачами
    (setq org-log-done t)
    (setq org-clock-idle-time nil)

    ;; Планировщик и дневник
    (setq org-agenda-include-diary t)
    (setq org-agenda-files '("~/MEGA/org"))
    (setq org-agenda-inhibit-startup t)

    ;; конфигурация внешних приложений для открытия файлов
    (setq org-file-apps
          '(("\\.pdf\\'" . "zathura %s")
            ("\\.gnumeric\\'" . "gnumeric %s")))

    ;; protect hidden trees for being inadvertily edited (do not work with evil)
    (setq-default org-catch-invisible-edits  'error
                  org-ctrl-k-protect-subtree 'error)

    ;; Показ картинок в тексте
    ;; работает только в GUI, но это удобная функция
    (when (window-system)
      (setq org-startup-with-inline-images t))
    ;; Ограничение ширины картинок
    (setq org-image-actual-width '(800))

    ;; :::::: Org-Babel ::::::

    ;; Поддержка языков программирования
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

    ;; refresh images after execution
    (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

    ;; don't ask confirmation to execute "safe" languages
    (defun antares-org-confirm-babel-evaluate (lang body)
      (and (not (string= lang "ditaa"))
         (not (string= lang "dot"))
         (not (string= lang "gnuplot"))
         (not (string= lang "ledger"))
         (not (string= lang "plantuml"))))

    (setq org-confirm-babel-evaluate 'antares-org-confirm-babel-evaluate)))

;; ag

;; [[./img/ag.png]]

;; [[https://github.com/Wilfred/ag.el][ag.el]] простой фронтенд Emacs для ag, ("the silver searcher" замена ack).

(use-package ag
  :ensure t
  :defer 1
  :config
  (progn
    (setq ag-reuse-buffers 't
          ag-highlight-search t
          ag-arguments (list "--color" "--smart-case" "--nogroup" "--column" "--all-types" "--"))))

;; async

;; [[https://github.com/jwiegley/emacs-async][async.el]] модуль для создания
;; асинхронных процессов в Emacs.

(use-package async
  :defer t
  :ensure t)

;; автодополнение

;; [[./img/auto_complete.png]]

;; [[https://github.com/auto-complete/auto-complete][Auto Complete Mode]] (aka =auto-complete.el=, =auto-complete-mode=) расширение
;; которое автоматизирует и расширяет систему автодополнения.

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

  ;; файл в котором хранится история автодополнения.
  (setq ac-comphist-file (concat user-emacs-directory
                                 "temp/ac-comphist.dat"))

  ;; грязный фикс для работы AC везде
  (define-globalized-minor-mode real-global-auto-complete-mode
    auto-complete-mode (lambda ()
                         (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))
                         ))
  (real-global-auto-complete-mode t))

;; TODO aria2c
;; Основной режим для управления менеджером загрузок [[http://aria2.sourceforge.net/][aria2c]]
;; [[./img/aria2c.png]]

(use-package aria2
  :ensure t
  :config
  (setq aria2-add-evil-quirks t))

;; avy

;; [[./img/avy.png]]

;; [[https://github.com/abo-abo/avy][avy]] GNU Emacs пакет для пролистывания видимого текста
;; используя символьное дерево решений.

;; [[./img/ace_link.png]]

;; [[https://github.com/abo-abo/ace-link][ace-link]] Emacs пакет для выбора ссылки для перехода.
;; Работает в org-mode, info, help и eww.

;; | Привязка | Вызов      | Действие          |
;; |----------+------------+-------------------|
;; | o        | ace-link-* | Переход по ссылке |
;; |----------+------------+-------------------|


;; [[./img/ace_window.png]]

;; [[https://github.com/abo-abo/ace-window][ace-window]] пакет для выбора окна для переключения.
;; Также может быть использован для перехода по словам, строкам, символам,
;; подстрокам, перемещения/удаления/копирования строк и других удобных действий.

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
              '((?x aw-delete-window     "Ace - Удалить окно")
                (?c aw-swap-window       "Ace - Сменить окно")
                (?n aw-flip-window)
                (?h aw-split-window-vert "Ace - Разделить окно по вертикали")
                (?v aw-split-window-horz "Ace - Разделить окно по горизонтали")
                (?m delete-other-windows "Ace - Развернуть окно")
                (?g delete-other-windows)
                (?b balance-windows)
                (?u winner-undo)
                (?r winner-redo)))

        (when (package-installed-p 'hydra)
          (defhydra hydra-window-size (:color red)
            "Размер окна"
            ("h" shrink-window-horizontally "сократить по горизонтали")
            ("j" shrink-window "сократить по вертикали")
            ("k" enlarge-window "увеличить по вертикали")
            ("l" enlarge-window-horizontally "увеличить по горизонтали"))
          (defhydra hydra-window-frame (:color red)
            "Frame"
            ("f" make-frame "Новый фрейм")
            ("x" delete-frame "Удалить фрейм"))
          (defhydra hydra-window-scroll (:color red)
            "Scroll other window"
            ("n" antares-scroll-other-window "scroll")
            ("p" antares-scroll-other-window-down "scroll down"))
          (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
          (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
          (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
        (ace-window-display-mode t)))

;; beacon

;; [[https://github.com/Malabarba/beacon][Beacon]] дополнительный режим, который помогает в поиске курсора.

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

;; [[https://github.com/davep/boxquote.el/blob/master/boxquote.el][boxquote.el]] предоставляет набор функций для использования
;; текста в стиле цитат. Текст частично выделен в левой части. Такая разметка
;; текста может быть использована для показа включённого внешнего текста или пример
;; кода.

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
  Текст          External           Apropos         Do              │ Boxquote │
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

;; [[https://github.com/Malabarba/elisp-bug-hunter][The Bug Hunter]] библиотека Emacs, которая ищет источник ошибки или
;; неожиданного поведения внутри elisp файла конфигурации (обычно =init.el= или
;; =.emacs=).

(use-package bug-hunter
  :ensure t
  :commands (bug-hunter-file bug-hunter-init-file))

;; calfw

;; [[./img/cfw_calendar.png]]

;; [[https://github.com/kiwanami/emacs-calfw][Calfw]] программа отображает календарь в буфере Emacs.

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
          cfw:fchar-top-right-corner ?┓)

          ;; Месяцы
          (setq calendar-month-name-array
           ["Январь" "Февраль" "Март" "Апрель" "Май" "Июнь"
           "Июль" "Август" "Сентяврь" "Октябрь" "Ноябрь" "Декабрь"])

          ;; Дни недели
          (setq calendar-day-name-array
           ["Воскресенье" "Понедельник" "Вторник" "Среда" "Четверг"
           "Пятница" "Суббота"])

          ;; Первый день недели
          (setq calendar-week-start-day 1) ; 0:Воскресенье, 1:Понедельник
          )
          )

;; TODO CIDER
;;    [[https://github.com/clojure-emacs/cider][CIDER]] интерактивная среда разработки для Clojure

(use-package cider
  :defer t
  :ensure t
)

;; charmap

;; [[./img/charmap.png]]

;; [[https://github.com/lateau/charmap][Charmap]] просмотр таблицы Unicode для Emacs.
;; С помощью CharMap можно посмотреть таблицу Unicode основанную на стандарте Unicode 6.2.

(use-package charmap
  :commands charmap
  :defer t
  :ensure t
  :config
  (setq charmap-text-scale-adjust 2))

;; TODO chess

;; [[./img/chess.png]]

;; [[https://github.com/jwiegley/emacs-chess][Chess.el]] клиент и библиотека Emacs, предназначенные
;; для использования и записи шахматных программ или для игр в шахматы против
;; различных движков, в том числе интернет-серверов. Библиотека может быть
;; использована для анализа вариаций, просмотра исторических игр или других целей.

(use-package chess
  :ensure t
  :commands chess
  :config
  (setq chess-images-default-size 70
        chess-images-separate-frame nil))

;; cloc

;; [[./img/cloc.png]]

;; [[https://github.com/cosmicexplorer/cloc-emacs][cloc]] количество строк кода в буфере

(use-package cloc
  :ensure t
  :commands cloc)

;; TODO clojure-mode
;;    [[https://github.com/clojure-emacs/clojure-mode][Clojure-mode]] интеграция,
;;    навигация и рефакторинг для Clojure(Script)

(use-package clojure-mode
  :ensure t
  :commands cloc)

;; csv-mode

;; [[https://github.com/emacsmirror/csv-mode][csv-mode]] основной режим для редактирования значений, разделённых запятой/символом.

;; | Binding | Call                    | Do                                                                     |
;; |---------+-------------------------+------------------------------------------------------------------------|
;; | C-c C-v | csv-toggle-invisibility | Toggle invisibility of field separators when aligned                   |
;; | C-c C-t | csv-transpose           | Rewrite rows (which may have different lengths) as columns             |
;; | C-c C-c | csv-set-comment-start   | Set comment start for this CSV mode buffer to STRING                   |
;; | C-c C-u | csv-unalign-fields      | Undo soft alignment and optionally remove redundant white space        |
;; | C-c C-a | csv-align-fields        | Align all the fields in the region to form columns                     |
;; | C-c C-z | csv-yank-as-new-table   | Yank fields as a new table starting at point                           |
;; | C-c C-y | csv-yank-fields         | Yank fields as the ARGth field of each line in the region              |
;; | C-c C-k | csv-kill-fields         | Kill specified fields of each line in the region                       |
;; | C-c C-d | csv-toggle-descending   | Toggle csv descending sort ordering                                    |
;; | C-c C-r | csv-reverse-region      | Reverse the order of the lines in the region                           |
;; | C-c C-n | csv-sort-numeric-fields | Sort lines in region numerically by the ARGth field of each line       |
;; | C-c C-s | csv-sort-fields         | Sort lines in region lexicographically by the ARGth field of each line |
;; |---------+-------------------------+------------------------------------------------------------------------|

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

;; define-word

;; [[https://github.com/abo-abo/define-word][define-word]] пакет GNU Emacs, позволяющий увидеть определение слова или фразы
;; в точке, без необходимости переключаться в браузер.

(use-package define-word
  :ensure t)

;; diff-hl

;; [[https://github.com/dgutov/diff-hl][diff-hl]] подсвечивает незакоммиченные изменения с левой стороны окна,
;; позволяет перемещаться между ними и выборочно откатить их.

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

;; elfeed

;; [[https://github.com/skeeto/elfeed][Elfeed]] предоставляет расширяемую читалку лент новостей для Emacs с поддержкой Atom и RSS

;; *Режим поиска*

;; [[./img/elfeed.png]]

;; *Режим просмотра*

;; [[./img/elfeed_show.png]]

(use-package elfeed
  :ensure t
  :commands elfeed
  :config
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "4 weeks ago"
                                :remove 'unread))
  (setq elfeed-db-directory  (concat antares-emacs-temporal-directory "elfeed")
        elfeed-search-filter "@4-weeks-old +unread "
        elfeed-search-title-max-width 100)
  (setq elfeed-feeds
        '(
          ("http://endlessparentheses.com/atom.xml" emacs)
          ("http://planet.emacsen.org/atom.xml" emacs)
          ("https://www.reddit.com/r/emacs/.rss" emacs)
          ("https://www.reddit.com/r/orgmode/.rss" emacs)
          ("http://www.blackhats.es/wordpress/?p=670" emacs)
          ("http://www.howardism.org/index.xml" emacs)
          ("http://www.masteringemacs.org/feed/" emacs)))
  (bind-keys :map elfeed-search-mode-map
             ("a"   .  elfeed-search-update--force)
             ("A"   .  elfeed-update)
             ("d"   .  elfeed-unjam)
             ("o"   .  elfeed-search-browse-url)
             ("j"   .  next-line)
             ("k"   .  previous-line)
             ("g"   .  beginning-of-buffer)
             ("G"   .  end-of-buffer)
             ("v"   .  set-mark-command)
             ("<escape>" .  keyboard-quit))
  (bind-keys :map elfeed-show-mode-map
             ("j"     . elfeed-show-next)
             ("k"     . elfeed-show-prev)
             ("o"     . elfeed-show-visit)
             ("<escape>" .  keyboard-quit)
             ("SPC"   . scroll-up)
             ("S-SPC" . scroll-down)
             ("TAB"   . shr-next-link)
             ("S-TAB" . shr-previous-link))

  (when (package-installed-p 'hydra)
      (bind-keys :map elfeed-search-mode-map
             ("\\"   . hydra-elfeed-search/body))
      (bind-keys :map elfeed-show-mode-map
             ("\\"   . hydra-elfeed-show/body))
      (eval-and-compile
        (defhydra hydra-elfeed-common (:color blue)
          ("\\" hydra-master/body "back")
          ("<ESC>" nil "quit")))

      (defhydra hydra-elfeed-search (:hint nil :color blue :inherit (hydra-elfeed-common/heads))
        "
                                                                      ╭────────┐
  Move   Filter     Entries        Tags          Do                   │ Elfeed │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  _p_/_k_    [_s_] live   [_RET_] view     [_r_] read      [_a_] refresh
  ^ ^↑^ ^    [_S_] set    [_o_] browse     [_u_] unread    [_A_] fetch
  ^ ^ ^ ^     ^ ^         [_y_] yank url   [_+_] add       [_d_] unjam
  ^ ^↓^ ^     ^ ^         [_v_] mark       [_-_] remove    [_E_] edit feeds
  _n_/_j_     ^ ^          ^ ^              ^ ^            [_q_] exit
--------------------------------------------------------------------------------
        "
        ("q"    quit-window)
        ("a"    elfeed-search-update--force)
        ("A"    elfeed-update)
        ("d"    elfeed-unjam)
        ("s"    elfeed-search-live-filter)
        ("S"    elfeed-search-set-filter)
        ("RET"  elfeed-search-show-entry)
        ("o"    elfeed-search-browse-url)
        ("y"    elfeed-search-yank)
        ("v"    set-mark-command)
        ("n"    next-line :color red)
        ("j"    next-line :color red)
        ("p"    previous-line :color red)
        ("k"    previous-line :color red)
        ("r"    elfeed-search-untag-all-unread)
        ("u"    elfeed-search-tag-all-unread)
        ("E"    (lambda() (interactive)(find-file "~/.emacs.d/elfeed.el.gpg")))
        ("+"    elfeed-search-tag-all)
        ("-"    elfeed-search-untag-all))

    (defhydra hydra-elfeed-show (:hint nil :color blue)
        "
                                                                      ╭────────┐
  Scroll       Entries        Tags          Links                     │ Elfeed │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  _S-SPC_    _p_/_k_  [_g_] refresh   [_u_] unread    _S-TAB_
  ^  ↑  ^    ^ ^↑^ ^  [_o_] browse    [_+_] add       ^  ↑  ^
  ^     ^    ^ ^ ^ ^  [_y_] yank url  [_-_] remove    ^     ^
  ^  ↓  ^    ^ ^↓^ ^  [_q_] quit       ^ ^            ^  ↓  ^
   _SPC_     _n_/_j_  [_s_] quit & search^^            _TAB_
--------------------------------------------------------------------------------
        "
        ("q"     elfeed-kill-buffer)
        ("g"     elfeed-show-refresh)
        ("n"     elfeed-show-next :color red)
        ("j"     elfeed-show-next :color red)
        ("p"     elfeed-show-prev :color red)
        ("k"     elfeed-show-prev :color red)
        ("s"     elfeed-show-new-live-search)
        ("o"     elfeed-show-visit)
        ("y"     elfeed-show-yank)
        ("u"     (elfeed-show-tag 'unread))
        ("+"     elfeed-show-tag)
        ("-"     elfeed-show-untag)
        ("SPC"   scroll-up :color red)
        ("S-SPC" scroll-down :color red)
        ("TAB"   shr-next-link :color red)
        ("S-TAB" shr-previous-link :color red))))

;; emmet-mode

;; [[https://github.com/smihica/emmet-mode][emmet-mode]] второстепенный режим, предоставляющий поддержку [[http://emmet.io/][Emmet]], который создаёт HTML и CSS из CSS-подобных селекторо.

;; Вот пример работы, набрав
;;      : a#q.x>b#q.x*2
;; получим такой код HTML:
;; #+BEGIN_EXAMPLE
;; <a id="q" class="x" href="">
;;     <b id="q" class="x"></b>
;;     <b id="q" class="x"></b>
;; </a>
;; #+END_EXAMPLE

;; | Binding  | Call                   | Do                        |
;; |----------+------------------------+---------------------------|
;; | C-j      | emmet-expand-line      | expand the emmet snippet  |
;; | C-return | emmet-expand-line      | expand the emmet snippet  |
;; | C-n      | emmet-next-edit-point  | go to the next edit point |
;; | C-p      | emmet-prev-edit-point  | go to the next edit point |
;; | C-c w    | emmet-wrap-with-markup | Wrap region with markup   |
;; |----------+------------------------+---------------------------|

;; [[https://github.com/yasuyk/helm-emmet][helm-emmet]] предоставляет источники для helm фрагментов в Emmet-mode

;; [[https://github.com/yasuyk/ac-emmet][ac-emmet]] источники автозаполнения для снипетов emmet-mode

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (bind-keys :map emmet-mode-keymap
             ("C-n" . emmet-next-edit-point)
             ("C-p" . emmet-prev-edit-point))

  (use-package helm-emmet
    :ensure t
    :ensure helm
    :commands helm-emmet)

  (use-package ac-emmet
    :ensure t
    :ensure auto-complete
    :config
    (add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
    (add-hook 'css-mode-hook  'ac-emmet-css-setup)))

;; epresent

;; [[https://github.com/eschulte/epresent][epresent]] простой режим презентации для Emacs Org-mode

;; | Binding   | Call                        | Do                                         |
;; |-----------+-----------------------------+--------------------------------------------|
;; | j         | scroll-up                   | scroll up one "line" of the same "slide"   |
;; | ↓         | scroll-up                   | scroll up one "line" of the same "slide"   |
;; | k         | scroll-down                 | scroll down one "line" of the same "slide" |
;; | ↑         | scroll-down                 | scroll down one "line" of the same "slide" |
;; |-----------+-----------------------------+--------------------------------------------|
;; | 1         | epresent-top                | top level of the presentation              |
;; | t         | epresent-top                | top level of the presentation              |
;; | q         | epresent-quit               | quit                                       |
;; |-----------+-----------------------------+--------------------------------------------|
;; | SPC       | epresent-next-page          | next "slide"                               |
;; | n         | epresent-next-page          | next "slide"                               |
;; | f         | epresent-next-page          | next "slide"                               |
;; | →         | epresent-next-page          | next "slide"                               |
;; | BACKSPACE | epresent-previous-page      | previous "slide"                           |
;; | p         | epresent-previous-page      | previous "slide"                           |
;; | b         | epresent-previous-page      | previous "slide"                           |
;; | ←         | epresent-previous-page      | previous "slide"                           |
;; |-----------+-----------------------------+--------------------------------------------|
;; | c         | epresent-next-src-block     | move to the next code block                |
;; | C         | epresent-previous-src-block | move to the previous code block            |
;; | e         | org-edit-src-code           | edit the source block                      |
;; | x         | org-babel-execute-src-block | execute the source block                   |
;; | r         | epresent-refresh            | refresh the page to show the results       |
;; | g         | epresent-refresh            | refresh the page to show the results       |
;; | C-c C-c   |                             | refresh the page to show the results       |
;; |-----------+-----------------------------+--------------------------------------------|

(use-package epresent
  :ensure t
  :defer t)

;; TODO esup

;; [[https://github.com/jschaf/esup][Esup]] пакет для тестирования времени запуска Emacs даже без выхода из Emacs.

(use-package esup
  :ensure t
  :commands esup)

;; evil

;; [[https://gitorious.org/evil/pages/Home][Evil]] расширяемый vi слой для Emacs. Он эмулирует основные особенности Vim,
;; а также предоставляет средства для написания пользовательских расширений.

;; | Binding | Call                        | Do                                      |
;; |---------+-----------------------------+-----------------------------------------|
;; | C-z     | evil-emacs-state            | Toggle evil-mode                        |
;; | \       | evil-execute-in-emacs-state | Execute the next command in emacs state |


;; [[https://github.com/Dewdrops/evil-exchange][Evil-exchange]] простой оператор изменения текста для Evil.
;; Это порт [[https://github.com/tommcdo/vim-exchange][vim-exchange]] от Tom McDonald.

;; | Binding | Call                 | Do                                                    |
;; |---------+----------------------+-------------------------------------------------------|
;; | gx      | evil-exchange        | Define (and highlight) the first {motion} to exchange |
;; | gX      | evil-exchange-cancel | Clear any {motion} pending for exchange.              |

;; [[https://github.com/cofi/evil-indent-textobject][evil-indent-textobject]] текстовый объект evil на основе отступа.

;; | textobject | Do                                                                     |
;; |------------+------------------------------------------------------------------------|
;; | ii         | Inner Indentation: the surrounding textblock with the same indentation |
;; | ai         | Above & Indentation: ii + the line above with a different indentation  |
;; | aI         | Above & Indentation+: ai + the line below with a different indentation |

;; Используем пакет [[https://github.com/redguardtoo/evil-matchit][Matchit]], эквивалентный Vim.

;; | Binding | Call              | Do                        |
;; |---------+-------------------+---------------------------|
;; | %       | evilmi-jump-items | jumps between item/tag(s) |
;; |---------+-------------------+---------------------------|

;; [[https://github.com/redguardtoo/evil-nerd-commenter][evil-nerd-commenter]] комментирует/раскомментируетстроки эффективно. Как Nerd Commenter в Vim

;; Используем пакет [[https://github.com/timcharper/evil-surround][evil-surround]], эквивалентный Vim.

;; | Binding | Do                                  |
;; |---------+-------------------------------------|
;; | ys      | create surround ('your surround')   |
;; | cs      | change surround                     |
;; | ds      | delete surround                     |
;; | S       | for create surrounds in visual mode |

;; [[https://github.com/victorhge/iedit][iedit]] позволяет редактировать одно вхождение какого-нибудь текста в буфере
;; или области и одновременно редактировать другие вхождения таким же образом, с
;; визуальной обратной связью по мере ввода.
;; [[https://github.com/magnars/expand-region.el][Expand region]] увеличивает выделенную область с помощью смысловых едениц.
;; Просто продолжайте нажимать клавишу, пока он не выберет то, что вы хотите.
;; [[https://github.com/syl20bnr/evil-iedit-state][evil-iedit-state]] выделение в Evil для iedit и расширенной области.

(use-package evil
  :ensure t
  :config
  (progn
    (defcustom antares-evil-state-modes
    '(fundamental-mode
      text-mode
      prog-mode
      term-mode
      conf-mode
      web-mode)
    "Список режимов, которые должны запускаться в статусе Evil."
    :type '(symbol))

    (defcustom antares-emacs-state-modes
    '(debugger-mode
      process-menu-mode
      pdf-view-mode
      doc-view-mode
      eww-mode
      epresent-mode
      elfeed-show-mode
      elfeed-search-mode
      sx-question-mode
      sx-question-list-mode
      paradox-menu-mode
      package-menu-mode
      archive-mode
      irfc-mode
      chess-mode
      git-commit-mode
      git-rebase-mode)
    "Список режимов, которые должны запускаться в Evil Emacs статусе."
    :type '(symbol))

    ;; better indentation
    (define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)

    ;; esc quits almost everywhere, Gotten from ;;
    ;; http://stackoverflow.com/questions/8483182/emacs-evil-mode-best-practice,;;
    ;; trying to emulate the Vim behaviour
    ;; (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

    ;; change cursor color depending on mode
    (setq evil-emacs-state-cursor    '("red" box)
          evil-normal-state-cursor   '("lawn green" box)
          evil-visual-state-cursor   '("orange" box)
          evil-insert-state-cursor   '("deep sky blue" bar)
          evil-replace-state-cursor  '("red" bar)
          evil-operator-state-cursor '("red" hollow))

    (defun antares-major-mode-evil-state-adjust ()
      (cond ((member major-mode antares-evil-state-modes) (turn-on-evil-mode))
            ((member major-mode antares-emacs-state-modes) (turn-off-evil-mode))
            ((apply 'derived-mode-p antares-evil-state-modes) (turn-on-evil-mode))
            ((apply 'derived-mode-p antares-emacs-state-modes) (turn-off-evil-mode))))

    (add-hook 'after-change-major-mode-hook #'antares-major-mode-evil-state-adjust)

    ;; defining new text objects
    ;; seen at http://stackoverflow.com/a/22418983/634816
    (defmacro antares-define-and-bind-text-object (key start-regex end-regex)
      (let ((inner-name (make-symbol "inner-name"))
            (outer-name (make-symbol "outer-name")))
        `(progn
           (evil-define-text-object ,inner-name (count &optional beg end type)
             (evil-select-paren ,start-regex ,end-regex beg end type count nil))
           (evil-define-text-object ,outer-name (count &optional beg end type)
             (evil-select-paren ,start-regex ,end-regex beg end type count t))
           (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
           (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

    ;; between underscores:
    (antares-define-and-bind-text-object "_" "_" "_")
    ;; an entire line:
    (antares-define-and-bind-text-object "l" "^" "$")
    ;; between dollars sign:
    (antares-define-and-bind-text-object "$" "\\$" "\\$")
    ;; between pipe characters:
    (antares-define-and-bind-text-object "|" "|" "|")

    ;; custom bindings for /Org-mode/.
    (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
    (evil-define-key 'normal org-mode-map (kbd "H") 'org-metaleft)
    (evil-define-key 'normal org-mode-map (kbd "L") 'org-metaright)
    (evil-define-key 'normal org-mode-map (kbd "K") 'org-metaup)
    (evil-define-key 'normal org-mode-map (kbd "J") 'org-metadown)
    (evil-define-key 'normal org-mode-map (kbd "U") 'org-shiftmetaleft)
    (evil-define-key 'normal org-mode-map (kbd "I") 'org-shiftmetaright)
    (evil-define-key 'normal org-mode-map (kbd "O") 'org-shiftmetaup)
    (evil-define-key 'normal org-mode-map (kbd "P") 'org-shiftmetadown)
    (evil-define-key 'normal org-mode-map (kbd "t")   'org-todo)
    (evil-define-key 'normal org-mode-map (kbd "-")   'org-cycle-list-bullet)

    (evil-define-key 'insert org-mode-map (kbd "C-c .")
      '(lambda () (interactive) (org-time-stamp-inactive t))))

    ;; bindings to use with hydra package
    (when (package-installed-p 'hydra)
      (define-key evil-motion-state-map "\\" 'hydra-master/body)
      (define-key evil-normal-state-map ","  'hydra-leader/body)
      (define-key evil-visual-state-map ","  'hydra-leader/body))

    (use-package evil-exchange
      :ensure t
      :config
      (evil-exchange-install))

    (use-package evil-indent-textobject
      :ensure t)

    (use-package evil-matchit
      :ensure t
      :config
      (global-evil-matchit-mode t))

    (use-package evil-nerd-commenter
      :ensure t
      :init
      (setq evilnc-hotkey-comment-operator ""))

    (use-package evil-iedit-state
      :ensure t
      :ensure expand-region
      :config
      (add-hook 'iedit-mode-hook 'evil-iedit-state)
      (when (package-installed-p 'hydra)
        (bind-keys :map evil-iedit-state-map
                   ("\\" . hydra-iedit/body))
        (bind-keys :map evil-iedit-insert-state-map
                   ("\\" . hydra-iedit-insert/body))
        (defhydra hydra-iedit (:color blue :hint nil)
          "
                                                                         ╭───────┐
    Occurrences                            Scope                         │ iedit │
  ╭──────────────────────────────────────────────────────────────────────┴───────╯
     ^ ^  _gg_        [_tab_]^ toggle                         _J_
     ^ ^  ^ ↑ ^       [_\#_]   number all                     ^↑^
     ^ ^   _N_        [_D_]  ^ delete all                 _L_ine|_F_unction
     ^ ^  ^ ↑ ^       [_S_]  ^ substitute all                 ^↓^
     _0_ ←^   ^→ $    [_I_]  ^ insert at beginning            _K_
     ^ ^  ^ ↓ ^       [_A_]  ^ append at the end
     ^ ^   _n_        [_p_]  ^ replace with yank
     ^ ^  ^ ↓ ^       [_U_]  ^ up-case all
     ^ ^   _G_        [_C-U_]^ down-case all
     ^ ^   ^ ^        [_V_]  ^ toggle lines
  --------------------------------------------------------------------------------
          "
          ("<esc>" nil "quit")
          ( "#"         iedit-number-occurrences)
          ( "\$"         evil-iedit-state/evil-end-of-line)
          ( "0"         evil-iedit-state/evil-beginning-of-line)
          ( "a"         evil-iedit-state/evil-append)
          ( "A"         evil-iedit-state/evil-append-line)
          ( "c"         evil-iedit-state/evil-change)
          ( "D"         iedit-delete-occurrences)
          ( "F"         iedit-restrict-function)
          ( "gg"        iedit-goto-first-occurrence)
          ( "G"         iedit-goto-last-occurrence)
          ( "i"         evil-iedit-insert-state)
          ( "I"         evil-iedit-state/evil-insert-line)
          ( "J"         iedit-expand-down-a-line)
          ( "K"         iedit-expand-up-a-line)
          ( "L"         iedit-restrict-current-line)
          ( "n"         iedit-next-occurrence)
          ( "N"         iedit-prev-occurrence)
          ( "o"         evil-iedit-state/evil-open-below)
          ( "O"         evil-iedit-state/evil-open-above)
          ( "p"         evil-iedit-state/paste-replace)
          ( "s"         evil-iedit-state/evil-substitute)
          ( "S"         evil-iedit-state/substitute)
          ( "V"         iedit-toggle-unmatched-lines-visible)
          ( "U"         iedit-upcase-occurrences)
          ( "C-U"       iedit-downcase-occurrences)
          ( "C-g"       evil-iedit-state/quit-iedit-mode)
          ( "tab"       iedit-toggle-selection)
          ( "backspace" iedit-blank-occurrences)
          ( "escape"    evil-iedit-state/quit-iedit-mode))

        (defhydra hydra-iedit-insert (:color blue :hint nil)
          "
                                                                         ╭───────┐
                                                                         │ iedit │
  ╭──────────────────────────────────────────────────────────────────────┴───────╯
  --------------------------------------------------------------------------------
          "
          ("<esc>" nil "quit"))))

    (use-package evil-surround
      :ensure t
      :config
      (global-evil-surround-mode 1)))

;; fill-column-indicator

;; [[https://github.com/alpaker/Fill-Column-Indicator][fill-column-indicator]] переключает вертикальный столбец, указывающий на заполнение строки.

(use-package fill-column-indicator
  :ensure t
  :commands fci-mode
  :config
  (fci-mode)
  (setq fci-rule-column 79))

;; fixmee

;; [[https://github.com/rolandwalker/fixmee][fixmee]] для быстрой навигации к FIXME и TODO меткам в Emacs.

;; | Binding | Call                             | Do                                       |
;; |---------+----------------------------------+------------------------------------------|
;; | C-c f   | fixmee-goto-nextmost-urgent      | Go to the next TODO/FIXME                |
;; | C-c F   | fixmee-goto-prevmost-urgent      | Go to the previous TODO/FIXME            |
;; | C-c v   | fixmee-view-listing              | View the list of TODOs                   |
;; | M-n     | fixmee-goto-next-by-position     | Go to the next TODO/FIXME (above a TODO) |
;; | M-p     | fixmee-goto-previous-by-position | Go to the next TODO/FIXME (above a TODO) |

(use-package fixmee
  :ensure t
  :diminish fixmee-mode
  :commands (fixmee-mode fixmee-view-listing)
  :init
  (add-hook 'prog-mode-hook 'fixmee-mode))

(use-package button-lock
  :diminish button-lock-mode)

;; flatland-theme

;; [[https://github.com/gchp/flatland-emacs][Flatland]] для Emacs- порт популярной темы Flatland для Sublime
;; Text, разработанной Pixel Lab.

(use-package flatland-theme
  :ensure t
  :defer t)

;; TODO flycheck

;; [[https://github.com/yasuyk/helm-flycheck][helm-flycheck]] показ ошибок flycheck с помощью helm.

(use-package flycheck
  :ensure t
  :defer t
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'sgml-mode 'flycheck-mode)
  (use-package helm-flycheck
    :ensure t
    :ensure helm
    :commands helm-flycheck))

;; TODO geben
   
;;    [[https://github.com/pokehanai/geben-on-emacs][geben]] плагин для дебага через протокол DBGp.

(use-package geben
  :ensure t
  :defer t
  :config
)

;; git-modes

;; [[https://github.com/magit/git-modes][Git modes]] GNU Emacs режимы для файлов связанных с Git.
;; Доступен в репозитории на GitHub, но также доступен как независимый пакет в Melpa.

(use-package gitconfig-mode
  :ensure t
  :defer t)
(use-package gitignore-mode
  :ensure t
  :defer t)
(use-package gitattributes-mode
  :ensure t
  :defer t)

;; git-timemachine

;; Use [[https://github.com/pidu/git-timemachine][git-timemachine]] для просмотра истории версий файла.
;; =p= (предыдущая) и =n= (следующая).

(use-package git-timemachine
  :ensure t
  :commands git-timemachine
  :config
  (defadvice git-timemachine-mode (after toggle-evil activate)
    "Отключаем `evil-local-mode' если включен `git-timemachine-mode',
    и включаем его обратно при выключении `git-timemachine-mode'."
    (evil-local-mode (if git-timemachine-mode -1 1))))

;; google-maps

;; [[https://julien.danjou.info/projects/emacs-packages#google-maps][google-maps]] предоставляет поддержку Google Maps в Emacs.
;; Работает в качестве независимой команды, а также интегрирована в org-mode.

;; | Binding | Call                               | Do                                                    |
;; |---------+------------------------------------+-------------------------------------------------------|
;; | C-c M-c | org-coordinates-google-geocode-set | Set Coordinates Properties from a Location (org-mode) |
;; | C-c M-L | org-address-google-geocode-set     | Set Address Properties from a Location (org-mode)     |
;; | C-c M-A | org-address-google-geocode-set     | Set Address Properties from a Location (org-mode)     |
;; | C-c M-l | org-location-google-maps           | Open Map from Address Properties (org-mode)           |
;; |---------+------------------------------------+-------------------------------------------------------|

(use-package google-maps
  :ensure t
  :defer 5
  :config
  (bind-keys :map google-maps-static-mode-map
             ("H" . google-maps-static-add-home-marker)
             ("k" . google-maps-static-move-north)
             ("j" . google-maps-static-move-south)
             ("h" . google-maps-static-move-west)
             ("l" . google-maps-static-move-east)
             ("y" . google-maps-static-copy-url)
             ("q" . quit-window))

  (when (package-installed-p 'hydra)
    (bind-keys :map google-maps-static-mode-map
               ("\\" . hydra-gmaps/body))
    (defhydra hydra-gmaps (:hint nil :color blue)
        "
                                                                   ╭─────────────┐
    Move       Zoom        Do                                      │ Google maps │
  ╭────────────────────────────────────────────────────────────────┴─────────────╯
   ^ ^   ^ _k_ ^    ^ ^   _<_/_+_/_._    [_t_] map type
   ^ ^   ^ ^↑^ ^    ^ ^   ^ ^ ^↑^ ^ ^    [_g_] refresh
   _h_ ← _c_|_C_ → _l_    ^ _z_|_Z_ ^    [_y_] yank url
   ^ ^   ^ ^↓^ ^    ^ ^   ^ ^ ^↓^ ^ ^    [_q_] quit
   ^ ^   ^ _j_ ^    ^ ^   _>_/_-_/_,_
  --------------------------------------------------------------------------------
        "
        ("\\" hydra-master/body "back")
        ("<ESC>" nil "quit")
        ("q"       google-maps-static-quit)
        ("+"       google-maps-static-zoom-in)
        (">"       google-maps-static-zoom-in)
        ("."       google-maps-static-zoom-in)
        ("-"       google-maps-static-zoom-out)
        ("<"       google-maps-static-zoom-out)
        (","       google-maps-static-zoom-out)
        ("z"       google-maps-static-zoom)
        ("Z"       google-maps-static-zoom-remove)
        ("y"       google-maps-static-copy-url)
        ("c"       google-maps-static-center)
        ("C"       google-maps-static-center-remove)
        ("t"       google-maps-static-set-maptype)
        ("g"       google-maps-static-refresh)
        ("k"       google-maps-static-move-north)
        ("j"       google-maps-static-move-south)
        ("h"       google-maps-static-move-west)
        ("l"       google-maps-static-move-east)))

  (use-package org-location-google-maps))

;; google-this

;; [[https://github.com/Bruce-Connor/emacs-google-this][google-this]] пакет, предоставляющий набор функций и привязок клавиш для запуска поиска Google внутри Emacs.

(use-package google-this
  :ensure t
  :defer t)

;; google-translate

;; [[./img/google_translate.png]]

;; [[https://github.com/atykhonov/google-translate][google-translate]] пакет, позволяющий перевести строку с помощью сервиса Google Translate
;; прямо из GNU Emacs.

(use-package google-translate
  :ensure t
  :commands google-translate-smooth-translate
  :init
  (setq-default google-translate-translation-directions-alist
                '(("ru" . "en") ("en" . "ru"))
                google-translate-show-phonetic t))

;; graphviz-dot-mode

;; [[https://github.com/ppareit/graphviz-dot-mode][graphviz-dot-mode]] режим для языка DOT, с использованием graphviz.

(use-package graphviz-dot-mode
  :ensure t
  :defer t)

;; haskell-mode

;; [[https://github.com/haskell/haskell-mode][haskell-mode]] режим Haskell для Emacs.

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

;; TODO helm

;; [[https://github.com/emacs-helm/helm][Helm]] инкрементальное завершение и сужение поиска для Emacs.

;; [[https://github.com/emacs-helm/helm-descbinds][Helm descbinds]] предоставляет интерфейс для =describe-bindings= Emacs, создающие сочетания клавиш
;; в интерактивном режиме для активного в настоящий момент режима с помощью helm.

;; | Binding | Call              | Do                  |
;; |---------+-------------------+---------------------|
;; | C-h b   | describe-bindings | Show helm-descbinds |
;; | C-x C-h | describe-bindings | Show heml-descbinds |
;; |---------+-------------------+---------------------|

;; [[https://github.com/ShingoFukuyama/helm-swoop][helm-swoop]] строит список строк в другом буфере, который может быть сужен любыми вводимыми словами.
;; В то же время курсор оригинального буфера перемещается от строки к строке в
;; соответствии с перемещением вверх и вниз по списку строк.

;; [[https://github.com/syohex/emacs-helm-themes][helm-themes]] позволяет выбор темы с Helm.

;; [[https://github.com/areina/helm-dash][helm-dash]] использует docsets [[https://kapeli.com/dash][Dash]] для просмотра документации.
;; Не требует установки Dash или Zeal.

(use-package helm
  :ensure t
  :config
  (progn
  (setq helm-surfraw-duckduckgo-url "https://duckduckgo.com/lite/?q=!%s&kp=1"
        helm-idle-delay 0.0
        helm-input-idle-delay 0.01
        helm-quick-update t
        helm-M-x-requires-pattern nil
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-ff-skip-boring-files t
        helm-autoresize-max-height 50
        helm-autoresize-min-height 50)
  (when (package-installed-p 'hydra)
      (define-key helm-map (kbd "\\") 'hydra-helm/body)
      (defhydra hydra-helm (:hint nil :color pink)
        "
                                                                          ╭──────┐
   Navigation   Other  Sources     Mark             Do             Help   │ Helm │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
        ^_k_^         _K_       _p_   [_m_] mark         [_v_] view         [_H_] helm help
        ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
    _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
        ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
        ^_j_^         _J_       _n_    ^ ^               [_w_] toggle windows
  --------------------------------------------------------------------------------
        "
        ("<tab>" helm-keyboard-quit "back" :exit t)
        ("<escape>" nil "quit")
        ("\\" (insert "\\") "\\" :color blue)
        ("h" helm-beginning-of-buffer)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-end-of-buffer)
        ("g" helm-beginning-of-buffer)
        ("G" helm-end-of-buffer)
        ("n" helm-next-source)
        ("p" helm-previous-source)
        ("K" helm-scroll-other-window-down)
        ("J" helm-scroll-other-window)
        ("c" helm-recenter-top-bottom-other-window)
        ("m" helm-toggle-visible-mark)
        ("t" helm-toggle-all-marks)
        ("u" helm-unmark-all)
        ("H" helm-help)
        ("s" helm-buffer-help)
        ("v" helm-execute-persistent-action)
        ("d" helm-persistent-delete-marked)
        ("y" helm-yank-selection)
        ("w" helm-toggle-resplit-and-swap-windows)
        ("f" helm-follow-mode)))
  (helm-autoresize-mode 1))
  (use-package helm-descbinds
    :ensure t
    :config
    (helm-descbinds-mode t)
    (setq helm-descbinds-window-sytle 'split-window))
  (use-package helm-swoop
    :ensure t
    :commands (helm-swoop helm-multi-swoop))
  (use-package helm-themes
    :ensure t
    :commands helm-themes)
  (use-package helm-dash
    :ensure t
    :config
    (setq helm-dash-docsets-path "~/gitRepos/dotfiles/emacs/docsets")
    ;; each time that emacs starts load all the docsets already downloaded
    (setq helm-dash-common-docsets
          (sort
           (let (value) 
             (dolist (element
                      (directory-files "~/gitRepos/dotfiles/emacs/docsets" nil "\\.docset$" 1) 
                      value)
               (setq value (cons (file-name-sans-extension element) value))))
           'string-lessp))))

;; hydra

;; [[https://github.com/abo-abo/hydra][Hydra]] пакет GNU Emacs, который может использоваться для привязки команд в семейство горячих клавиш
;; с общим префиксом - Hydra.

;; Я использую его в качестве общего интерфейса для наиболее часто используемых
;; команд в моём рабочем процессе. Он основан на предыдущей идее, которую я
;; реализовывал в Vim с Unite, чтобы генерировать меню, в котором наиболее полезные
;; команды отображались с помощью привязки клавиш для их активации. В то же время
;; Unite работал в качестве интерфейса для нескольких из этих команд.

;; В Emacs способ добиться такого же поведения иной потому, что благодаря многим
;; разработчикам мы имеем две роли, которые Unite предпочитал разделять в моей
;; конфигурации Vim на два отдельных способа:

;; + Командный интерфейс:
;;   Я использую наиболее подходящий для этого пакет - Helm. Это эквивалент Unite в
;;   Vim. Он работает в качестве основы для автодополнения и выбора для многих
;;   команд и задач Emacs. Пока я не использую его на полную мощь, но думаю, что
;;   буду применять в большом количестве задач.

;; + Меню:
;;   Вначале, имитируя проект [[https://github.com/syl20bnr/spacemacs][Spacemacs]], я использовал комбинации =evil-leader= и =guide-key=
;;   для создания меню. Но при этом всплывали некоторые глюки и я не хочу
;;   использовать активный Evil во всех буферах. После появилась Hydra и с первого
;;   момента я понял, что она решает практически любую проблему, которая была в
;;   предыдущей настройке. Она может использоваться по всему Emacs и она более
;;   настраиваемая и более ориентированная к моей первоначальной цели.

;; Я использую Hydra двумя способами:

;; + Активация через "\", для вызова всех основных и пакетных меню. Используя его,
;;   а иногда команду =helm-descbinds= (C-h b), я могу видеть и запоминать все
;;   наиболее полезные команды и привязки клавиш, которые есть в моём распоряжении
;;   и это очень удобно.Больше не тратится времени на попытки вспомнить комбинации.

;; + Активация через "," для работы в качестве Evil leader key (только когда активен
;;   Evil) для доступа к меню общих задач, которые мне нужны когда я редактирую
;;   текст (например комментирую область).

;; Я всё ещё предпочитаю "язык" Evil, поэтому многие команды и пакеты
;; сконфигурированы таким образом.

(use-package hydra
  :ensure t
  :defer 0.1
  :init
  (bind-key "\\" 'hydra-master/body)
  :config
  (setq lv-use-separator t)
  (set-face-attribute 'hydra-face-blue nil :foreground "deep sky blue" :weight 'bold)

  (eval-and-compile
    (defhydra hydra-common (:color blue)
      ("<ESC>" nil "quit")))

  (defhydra hydra-master (:color blue :idle 0.4)
    "
                                                                       ╭───────┐
                                                                       │ Index │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_a_] bookmarks    [^h^]               [_o_] organization  [_v_] games
  [_b_] buffers      [_i_] internet      [_p_] project       [_w_] window
  [_c_] flycheck     [_j_] jump          [_q_] exit          [_x_] shell
  [_d_] development  [_k_] spell         [_r_] register      [^y^]
  [_e_] emacs        [_l_] lisp          [_s_] search        [^z^]
  [_f_] file         [_m_] media         [_t_] text
  [_g_] git          [_n_] narrow        [^u^]
--------------------------------------------------------------------------------
    "
    ("<SPC>" antares-alternate-buffers "alternate buffers")
    ("<ESC>" nil "quit")
    ("\\" (insert "\\") "\\")
    ("a"     hydra-bookmarks/body nil)
    ("b"     hydra-buffers/body nil)
    ("c"     hydra-flycheck/body nil)
    ("d"     hydra-development/body nil)
    ("e"     hydra-emacs/body nil)
    ("f"     hydra-file/body nil)
    ("g"     hydra-git/body nil)
    ("i"     hydra-internet/body nil)
    ("j"     hydra-jump/body nil)
    ("k"     hydra-spell/body nil)
    ("l"     hydra-lisp/body nil)
    ("m"     hydra-media/body nil)
    ("n"     hydra-narrow/body nil)
    ("o"     hydra-organization/body nil)
    ("p"     hydra-project/body nil)
    ("q"     hydra-exit/body nil)
    ("r"     hydra-register/body nil)
    ("s"     hydra-search/body nil)
    ("t"     hydra-text/body nil)
    ("v"     hydra-games/body nil)
    ("w"     ace-window nil)
    ("x"     hydra-system/body nil))

  (defhydra hydra-bookmarks (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                   ╭───────────┐
       List                          Do                            │ Bookmarks │
╭──────────────────────────────────────────────────────────────────┴───────────╯
  [_h_] list bookmarks (helm)     [_j_] jump to a bookmark
  [_l_] list bookmarks            [_m_] set bookmark at point
  ^ ^                             [_s_] save bookmarks
--------------------------------------------------------------------------------
    "
    ("h" helm-bookmarks)
    ("j" bookmark-jump)
    ("l" list-bookmarks)
    ("m" bookmark-set)
    ("s" bookmark-save))

  (defhydra hydra-buffers (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                     ╭─────────┐
  Switch                 Do                                          │ Buffers │
╭────────────────────────────────────────────────────────────────────┴─────────╯
  [_b_] switch (ido)       [_d_] kill the buffer
  [_i_] ibuffer            [_r_] toggle read-only mode
  [_a_] alternate          [_u_] revert buffer changes
  [_s_] switch (helm)      [_w_] save buffer
--------------------------------------------------------------------------------
    "
    ("a" antares-alternate-buffers)
    ("b" ivy-switch-buffer)
    ("d" antares-kill-this-buffer)
    ("i" ibuffer)
    ("m" ace-swap-window)
    ("r" read-only-mode)
    ("s" helm-buffers-list)
    ("u" antares-revert-buffer)
    ("w" save-buffer))

    (defhydra hydra-flycheck (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                    ╭──────────┐
   Navigate          Show Errors                  Do                │ Flycheck │
╭───────────────────────────────────────────────────────────────────┴──────────╯
   ^_p_^revious     [_l_] list errors           [_t_] toggle Flycheck
      ^^↑^^         [_e_] list errors (helm)    [_c_] select checker
    ^_f_^irst       [_d_] clear all errors      [_r_] run via compile
      ^^↓^^          ^ ^                        [_h_] describe checker
    ^_n_^ext
--------------------------------------------------------------------------------
      "
      ("c" flycheck-select-checker)
      ("h" flycheck-describe-checker)
      ("d" flycheck-clear)
      ("e" helm-flycheck)
      ("f" flycheck-first-error)
      ("l" flycheck-list-errors)
      ("n" flycheck-next-error :color red)
      ("p" flycheck-previous-error :color red)
      ("r" flycheck-compile)
      ("t" flycheck-mode))

    (defhydra hydra-development (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                 ╭─────────────┐
     Dash                   Web                 Quickrun         │ Development │
╭────────────────────────────────────────────────────────────────┴─────────────╯
  [_d_] search docs (at point) [_c_] Web Colors          [_q_] buffer
  [_D_] search docs            [_h_] HTTP header         [_v_] region
  [_i_] get docset             [_m_] HTTP method         [_x_] shell
  [_u_] get user docset        [_r_] HTTP relation       [_p_] with arg
  [_a_] activate docset        [_s_] HTTP status code    [_k_] buffer (helm)
   ^ ^                         [_g_] RESTclient          [_o_] only compile
   ^ ^                         [_f_] RFC doc             [_R_] replace
  [_l_] lines of code          [_F_] RFC index           [_e_] eval/print
--------------------------------------------------------------------------------
      "
      ("d" helm-dash-at-point)
      ("D" helm-dash)
      ("i" helm-dash-install-docset)
      ("u" helm-dash-install-user-docset)
      ("a" helm-dash-activate-docset)
      ("c" helm-colors)
      ("g" restclient-mode)
      ("f" irfc-visit)
      ("F" irfc-index)
      ("q" quickrun)
      ("v" quickrun-region)
      ("x" quickrun-shell)
      ("p" quickrun-with-arg)
      ("o" quickrun-compile-only)
      ("R" quickrun-replace-region)
      ("e" quickrun-eval-print)
      ("k" helm-quickrun)
      ("h" http-header)
      ("m" http-method)
      ("r" http-relation)
      ("s" http-status-code)
      ("l" cloc))

  (defhydra hydra-emacs (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                       ╭───────┐
   Execute       Packages         Help                     Misc        │ Emacs │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_s_] smex       [_p_] list      [_a_] apropos (helm)    [_t_] change theme (helm)
  [_m_] smex mode  [_i_] install   [_f_] info manual       [_l_] list emacs process
  [_h_] helm M-x   [_u_] upgrade   [_k_] bindings (helm)   [_c_] init time
  [_x_] counsel M-x ^ ^            [_b_] personal bindings [_o_] unbound commands
--------------------------------------------------------------------------------
      "
      ("C-h b" helm-descbinds "bindings")
      ("a" helm-apropos)
      ("b" describe-personal-keybindings)
      ("c" emacs-init-time)
      ("i" package-install)
      ("k" helm-descbinds)
      ("l" list-processes)
      ("f" info-display-manual)
      ("p" paradox-list-packages)
      ("t" helm-themes)
      ("u" paradox-upgrade-packages)
      ("m" smex-major-mode-commands)
      ("s" smex)
      ("h" helm-M-x)
      ("x" counsel-M-x)
      ("o" smex-show-unbound-commands))

  (defhydra hydra-file (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                        ╭──────┐
     Ido               Helm                 Dired        Ztree          │ File │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_o_] open file   [_f_] find file      [_d_] dired    [_z_] diff dirs
   ^ ^              [_m_] mini           [_r_] ranger
--------------------------------------------------------------------------------
      "
      ("o" find-file)
      ("f" helm-find-files)
      ("m" helm-mini)
      ("z" ztree-diff)
      ("d" dired)
      ("r" ranger))


  (defhydra hydra-text (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                        ╭──────┐
 Size  Toggle              Unicode                        Do            │ Text │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  _k_  [_f_] fill column     [_d_] unicode character           [_a_] align with regex
  ^↑^  [_h_] hidden chars    [_e_] evil digraphs table         [_w_] remove trailing ' '
  ^ ^  [_l_] line numbers    [_s_] specific code block         [_n_] count words
  ^↓^  [_t_] trailing ' '    [_u_] unicode character (helm)    [_i_] lorem ipsum
  _j_  [_v_] font space      [_p_] character code              [_x_] comment box
  ^ ^  [_c_] comment          ^ ^                              [_q_] boxquote
  ^ ^  [_b_] multibyte chars  ^ ^                              [_m_] iedit (multiple)
  ^ ^   ^ ^                   ^ ^                              [_r_] expand region
  ^ ^   ^ ^                   ^ ^                              [_U_] tabs to spaces
--------------------------------------------------------------------------------
      "
      ("a" align-regexp)
      ("b" toggle-enable-multibyte-characters)
      ("c" evilnc-comment-or-uncomment-lines)
      ("d" insert-char)
      ("e" evil-ex-show-digraphs)
      ("f" fci-mode)
      ("h" whitespace-mode)
      ("i" lorem-ipsum-insert-paragraphs)
      ("k" text-scale-increase :color red)
      ("j" text-scale-decrease :color red)
      ("l" linum-mode)
      ("n" count-words)
      ("m" iedit)
      ("p" describe-char)
      ("r" er/expand-region)
      ("s" charmap)
      ("t" antares-toggle-show-trailing-whitespace)
      ("u" helm-ucs)
      ("v" variable-pitch-mode)
      ("w" whitespace-cleanup)
      ("U" untabify)
      ("q" hydra-boxquote/body)
      ("x" comment-box))

  (defhydra hydra-git (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                         ╭─────┐
   Magit                          VC                    Timemachine      │ Git │
╭────────────────────────────────────────────────────────────────────────┴─────╯
  [_s_] status              [_d_] diffs between revisions  [_t_] timemachine
  [_B_] blame mode          [_b_] edition history
  [_l_] file log
--------------------------------------------------------------------------------
      "
      ("B" magit-blame-mode)
      ("b" vc-annotate)
      ("d" vc-diff)
      ("l" magit-file-log)
      ("s" magit-status)
      ("t" git-timemachine))

  (defhydra hydra-internet (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                    ╭──────────┐
    Browse       Search             Social               Post       │ Internet │
╭───────────────────────────────────────────────────────────────────┴──────────╯
  [_w_] eww      [_g_] google          [_f_] elfeed            [_i_] imgur
  [_u_] url      [_m_] google maps     [_x_] stack overflow
   ^ ^           [_s_] surfraw
   ^ ^           [_d_] wordnik
--------------------------------------------------------------------------------
      "
      ("f" elfeed)
      ("g" google-this)
      ("i" imgur-post)
      ("m" google-maps)
      ("d" define-word-at-point)
      ("s" helm-surfraw)
      ("w" eww)
      ("u" browse-url-at-point)
      ("x" sx-tab-newest))

  (defhydra hydra-jump (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                        ╭──────┐
  Window          Word/Char        Line         iSearch                 │ Jump │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_w_] jump        [_j_] word         [_l_] jump     [_i_] jump
  [_d_] close       [_p_] all words    [_y_] copy
  [_z_] maximize    [_b_] subword      [_m_] move
  [_s_] swap        [_c_] char         [_v_] copy region
   ^ ^              [_a_] two chars
--------------------------------------------------------------------------------
      "
      ("w" ace-window)
      ("d" ace-delete-window)
      ("z" ace-maximize-window)
      ("s" ace-swap-window)
      ("j" avy-goto-word-1)
      ("p" avy-goto-word-0)
      ("b" avy-goto-subword-0)
      ("c" avy-goto-char)
      ("a" avy-goto-char-2)
      ("l" avy-goto-line)
      ("y" avy-copy-line)
      ("m" avy-move-line)
      ("v" avy-copy-region)
      ("i" avy-isearch))

  (defhydra hydra-spell (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                       ╭───────┐
    Flyspell               Ispell                      Gtranslate      │ Spell │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_k_] correct word       [_w_] check word            [_g_] en ⇆ es
  [_n_] next error         [_t_] toggle dictionary     [_G_] any lang
  [_f_] toggle flyspell    [_d_] change dictionary
  [_p_] toggle prog mode
--------------------------------------------------------------------------------
      "
      ("w" ispell-word)
      ("d" ispell-change-dictionary)
      ("t" antares-switch-dictionary)
      ("g" google-translate-smooth-translate)
      ("G" google-translate-query-translate)
      ("f" flyspell-mode)
      ("p" flyspell-prog-mode)
      ("k" flyspell-auto-correct-word)
      ("n" flyspell-goto-next-error))

  (defhydra hydra-lisp (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                        ╭──────┐
    Elisp              Bug hunter                                       │ Lisp │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_r_] eval region    [_f_] file
  [_s_] eval sexp      [_i_] init-file
--------------------------------------------------------------------------------
      "
      ("f" bug-hunter-file)
      ("i" bug-hunter-init-file)
      ("r" eval-region)
      ("s" eval-last-sexp))

  (defhydra hydra-narrow (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                      ╭────────┐
    Narrow                                                            │ Narrow │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_f_] narrow to defun
  [_p_] narrow to page
  [_r_] narrow to region
  [_w_] widen
--------------------------------------------------------------------------------
      "
      ("f" narrow-to-defun)
      ("p" narrow-to-page)
      ("r" narrow-to-region)
      ("w" widen))

  (defhydra hydra-project (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                  ╭────────────┐
  Files             Search          Buffer             Do         │ Projectile │
╭─────────────────────────────────────────────────────────────────┴────────────╯
  [_f_] file          [_a_] ag          [_b_] switch         [_g_] magit
  [_l_] file dwim     [_A_] grep        [_v_] show all       [_p_] commander
  [_r_] recent file   [_s_] occur       [_V_] ibuffer        [_i_] info
  [_d_] dir           [_S_] replace     [_K_] kill all
  [_o_] other         [_t_] find tag
  [_u_] test file     [_T_] make tags
  [_h_] root
                                                                      ╭────────┐
  Other Window      Run             Cache              Do             │ Fixmee │
╭──────────────────────────────────────────────────╯ ╭────────────────┴────────╯
  [_F_] file          [_U_] test        [_kc_] clear         [_x_] TODO & FIXME
  [_L_] dwim          [_m_] compile     [_kk_] add current   [_X_] toggle
  [_D_] dir           [_c_] shell       [_ks_] cleanup
  [_O_] other         [_C_] command     [_kd_] remove
  [_B_] buffer
--------------------------------------------------------------------------------
      "
      ("a"   projectile-ag)
      ("A"   projectile-grep)
      ("b"   projectile-switch-to-buffer)
      ("B"   projectile-switch-to-buffer-other-window)
      ("c"   projectile-run-async-shell-command-in-root)
      ("C"   projectile-run-command-in-root)
      ("d"   projectile-find-dir)
      ("D"   projectile-find-dir-other-window)
      ("f"   projectile-find-file)
      ("F"   projectile-find-file-other-window)
      ("g"   projectile-vc)
      ("h"   projectile-dired)
      ("i"   projectile-project-info)
      ("kc"  projectile-invalidate-cache)
      ("kd"  projectile-remove-known-project)
      ("kk"  projectile-cache-current-file)
      ("K"   projectile-kill-buffers)
      ("ks"  projectile-cleanup-known-projects)
      ("l"   projectile-find-file-dwim)
      ("L"   projectile-find-file-dwim-other-window)
      ("m"   projectile-compile-project)
      ("o"   projectile-find-other-file)
      ("O"   projectile-find-other-file-other-window)
      ("p"   projectile-commander)
      ("r"   projectile-recentf)
      ("s"   projectile-multi-occur)
      ("S"   projectile-replace)
      ("t"   projectile-find-tag)
      ("T"   projectile-regenerate-tags)
      ("u"   projectile-find-test-file)
      ("U"   projectile-test-project)
      ("v"   projectile-display-buffer)
      ("V"   projectile-ibuffer)
      ("X"   fixmee-mode)
      ("x"   fixmee-view-listing))

  (defhydra hydra-exit (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                        ╭──────┐
   Quit                                                                 │ Exit │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_c_] exit emacs (standalone or client)
  [_s_] shutdown the emacs daemon
--------------------------------------------------------------------------------
      "
      ("c" save-buffers-kill-terminal)
      ("s" save-buffers-kill-emacs))

  (defhydra hydra-register (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                    ╭──────────┐
   Logs                        Registers                Undo        │ Register │
╭───────────────────────────────────────────────────────────────────┴──────────╯
  [_c_] commands history       [_e_] emacs registers    [_u_] undo tree
  [_o_] echo-area messages     [_r_] evil registers
  [_b_] minibuffer             [_m_] evil marks
  [_l_] messages               [_k_] kill ring
  [_d_] diff buffer with file
--------------------------------------------------------------------------------
      "
      ("c" helm-complex-command-history)
      ("d" antares-diff-buffer-with-file)
      ("e" helm-register)
      ("k" helm-show-kill-ring)
      ("a" helm-all-mark-rings)
      ("l" popwin:messages)
      ("m" evil-show-marks)
      ("o" view-echo-area-messages)
      ("r" evil-show-registers)
      ("b" helm-minibuffer-history)
      ("u" undo-tree-visualize))

  (defhydra hydra-search (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                      ╭────────┐
   Files                             Buffer                           │ Search │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_a_] regex search (Ag)           [_b_] by word
  [_A_] regex by filetype (Ag)      [_o_] by word (occur)
  [_h_] regex search (grep & helm)  [_w_] by word (multi)
  [_g_] regex search (grep)         [_t_] tags & titles
  [_f_] find
  [_l_] locate
--------------------------------------------------------------------------------
      "
      ("A" ag-files)
      ("a" ag)
      ("b" helm-swoop)
      ("f" helm-find)
      ("g" rgrep)
      ("h" helm-do-grep)
      ("l" helm-locate)
      ("o" helm-occur)
      ("t" helm-semantic-or-imenu)
      ("w" helm-multi-swoop))

  (defhydra hydra-games (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                       ╭───────┐
   Game                                                                │ Games │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_b_] bubbles       [_c_] chess (computer)
  [_t_] tetris        [_a_] chess (internet)
  [_g_] gomoku
--------------------------------------------------------------------------------
      "
      ("b" bubbles-set-game-hard)
      ("c" chess)
      ("a" chess-ics)
      ("g" gomoku)
      ("t" tetris))

  (defhydra hydra-system (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                      ╭────────┐
   Terminals                     System                               │ System │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_s_] new multi-term           [_c_] shell command
  [_n_] next multi-term          [_a_] aync shell command
  [_p_] previous multi-term      [_m_] man page
  [_d_] dedicated multi-term     [_l_] list system process
  [_e_] eshell                   [_t_] top command
--------------------------------------------------------------------------------
      "
      ("a" async-shell-command)
      ("c" shell-command)
      ("e" eshell)
      ("m" helm-man-woman)
      ("l" proced)
      ("s" multi-term)
      ("n" multi-term-next)
      ("p" multi-term-previous)
      ("d" multi-term-dedicated-toggle)
      ("t" helm-top))

  (defhydra hydra-media (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                       ╭───────┐
   Mingus              Mpd                     Volume                  │ Media │
╭──────────────────────────────────────────────────────────────────────┴───────╯
 [_m_] mingus         [_n_] next song          [_-_] volume down
 [_f_] search         [_p_] previous song      [_+_] volume up
 [_l_] playlist       [_c_] clear playlist
 [_a_] All            [_t_] pause
  ^ ^                 [_s_] stop
  ^ ^                 [_d_] start daemon
--------------------------------------------------------------------------------
      "
      ("m" mingus)
      ("f" mingus-search)
      ("c" mingus-clear)
      ("n" mingus-next)
      ("p" mingus-prev)
      ("t" mingus-toggle)
      ("s" mingus-stop)
      ("d" mingus-start-daemon)
      ("l" mingus-load-playlist)
      ("a" mingus-load-all)
      ("-" mingus-vol-down)
      ("\+" mingus-vol-up))

  (defhydra hydra-organization (:color blue :hint nil :idle 0.4 :inherit (hydra-common/heads))
      "
                                                                ╭──────────────┐
     Tasks            Org mode               Comms      Others  │ Organization │
╭───────────────────────────────────────────────────────────────┴──────────────╯
  [_a_] agenda      [_c_] capture             [_m_] mail      [_x_] speed type
  [_l_] agenda list [_p_] pomodoro            [_t_] contacts
  [_d_] calendar    [_s_] search headings     [_h_] add location
   ^ ^              [_g_] open location gmaps
   ^ ^              [_f_] archive subtree
--------------------------------------------------------------------------------
      "
      ("a" org-agenda)
      ("c" org-capture)
      ("d" cfw:open-org-calendar)
      ("g" org-location-google-maps)
      ("h" org-address-google-geocode-set)
      ("l" org-agenda-list)
      ("f" org-archive-subtree)
      ("m" mu4e)
      ("p" org-pomodoro)
      ("s" helm-org-agenda-files-headings)
      ("t" org-contacts)
      ("x" speed-type-text))

   (defhydra hydra-leader ( :color blue :hint nil :idle 0.4)
       "
                                                                      ╭────────┐
   Toggle                        Do                                   │ Leader │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_c_] comment                  [_a_] align with regex
  [_f_] fill column              [_p_] show character code
  [_h_] hidden chars             [_i_] insert unicode character (helm)
  [_e_] trailing whitespace      [_<SPC>_] remove trailing whitespaces
  [_v_] font space               [_u_] undo tree
   ^ ^                           [_j_] jump word
   ^ ^                           [_x_] comment box
   ^ ^                           [_r_] expand region
   ^ ^                           [_m_] iedit (multiple edit)
   ^ ^                           [_g_] google translate
   ^ ^                           [_s_] swiper
   ^ ^                           [_t_] helm-semantic-or-imenu
--------------------------------------------------------------------------------
      "
      ("<escape>" nil "quit")
      ("a" align-regexp)
      ("c" evilnc-comment-or-uncomment-lines)
      ("r" er/expand-region)
      ("f" fci-mode)
      ("g" google-translate-smooth-translate)
      ("h" whitespace-mode)
      ("i" helm-ucs)
      ("j" avy-goto-word-1)
      ("m" iedit-mode)
      ("n" count-words)
      ("p" describe-char)
      ("e" antares-toggle-show-trailing-whitespace)
      ("u" undo-tree-visualize)
      ("v" variable-pitch-mode)
      ("<SPC>" whitespace-cleanup)
      ("s" antares-swiper)
      ("t" helm-semantic-or-imenu)
      ("x" comment-box)))

;; ibuffer-vc

;; [[https://github.com/purcell/ibuffer-vc][ibuffer-vc]] поакзывает буферы сгруппированные по системам контроля версий.

(use-package ibuffer-vc
  :ensure t
  :commands ibuffer
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  :config
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process))))

;; ido-ubiquitous

;; Дайне мне ido... Везде!

;; [[https://github.com/DarwinAwardWinner/ido-ubiquitous][ido-ubiquitous]] делает именно то, на что вы надеялись устанавливая
;; =(setq ido-everywhere t)=. Заменяет стандартное автодополнение emacs на
;; автодополнение ido везде, где это возможно сделать не нарушая работы.

(use-package ido-ubiquitous
  :ensure t
  :disabled t
  :ensure ido
  :config
  (ido-ubiquitous-mode t)
  (setq ido-ubiquitous-max-items 50000))

;; ido-vertical-mode

;; [[https://github.com/gempesaw/ido-vertical-mode.el][ido-vertical-mode]] позволяет =ido-mode= показать вертикально.

(use-package ido-vertical-mode
  :ensure t
  :ensure ido
  :config
  (ido-vertical-mode t))

;; TODO impatient-mode

;; Благодара [[https://github.com/skeeto/impatient-mode][impatient-mode]] вы можете видеть результат редактирования HTML во время редактирования.

(use-package impatient-mode
  :ensure t)

;; imgur

;; [[https://github.com/myuhe/imgur.el][imgur]] клиент imgur для Emacs

(use-package imgur
  :ensure t
  :commands imgur-post)

;; irfc

;; [[./img/irfc.png]]

;; [[http://www.emacswiki.org/emacs/irfc.el][irfc]] интерфейс для IETF RFC документов.

(use-package irfc
  :ensure t
  :init
  (setq-default irfc-directory (concat antares-emacs-temporal-directory "RFC")
                irfc-assoc-mode t)
  (defun irfc-index ()
    (interactive)
    (defvar antares-rfc-index-file (concat irfc-directory "/rfc0000.txt" ))
    (defvar antares-rfc-index-url "https://www.ietf.org/download/rfc-index.txt")
    (unless (file-exists-p antares-rfc-index-file)
      (url-copy-file antares-rfc-index-url joe-rfc-index-file))
    (find-file antares-rfc-index-file))
  :config
  (bind-keys :map irfc-mode-map
             ("SPC" . scroll-up)
             ("S-SPC" . scroll-down)
             ("j" . next-line)
             ("k" . previous-line)
             ("h" . backward-char)
             ("l" . forward-char)
             ("J" . irfc-scroll-up-one-line)
             ("K" . irfc-scroll-down-one-line)
             ("G" . end-of-buffer)
             ("g" . beginning-of-buffer)
             ("T" . irfc-render-toggle)
             ("q" . irfc-quit)
             ("o" . irfc-follow)
             ("v" . irfc-visit)
             ("i" . irfc-index)
             ("r" . irfc-reference-goto)
             ("f" . irfc-head-goto)
             ("F" . irfc-head-number-goto)
             ("e" . irfc-page-goto)
             ("n" . irfc-page-next)
             ("p" . irfc-page-prev)
             (">" . irfc-page-last)
             ("<" . irfc-page-first)
             ("t" . irfc-page-table)
             ("H" . irfc-head-next)
             ("L" . irfc-head-prev)
             ("RET" . irfc-table-jump)
             ("<tab>" . irfc-rfc-link-next)
             ("<backtab>" . irfc-rfc-link-prev))
  (when (package-installed-p 'hydra)
    (bind-keys :map irfc-mode-map
             ("\\" . hydra-irfc/body))
    (defhydra hydra-irfc (:hint nil :color blue)
          "
                                                                            ╭──────┐
      Move     Scroll   Page  Heads    Links      TOC           Do          │ iRFC │
    ╭───────────────────────────────────────────────────────────────────────┴──────╯
          ^_g_^     _S-SPC_    _<_     ^ ^ ^ ^        ^ ^       [_t_] TOC       [_v_] visit RFC
          ^^↑^^       ^↑^      ^↑^     ^ ^ ^ ^        ^ ^       [_RET_] node    [_i_] index
          ^_k_^       _K_      _p_     ^ _L_ ^    _<backtab>_    ^ ^            [_r_] reference
          ^^↑^^       ^↑^      ^↑^     ^ ^↑^ ^        ^↑^        ^ ^            [_T_] toggle
      _h_ ←   → _l_   ^ ^      _e_     _f_/_F_        _o_        ^ ^            [_q_] quit
          ^^↓^^       ^↓^      ^↓^     ^ ^↓^ ^        ^↓^
          ^_j_^       _J_      _n_     ^ _H_ ^      _<tab>_
          ^^↓^^       ^↓^      ^↓^     ^ ^ ^ ^        ^ ^
          ^_G_^      _SPC_     _>_     ^ ^ ^ ^        ^ ^
    --------------------------------------------------------------------------------
          "
          ("\\" hydra-master/body "back")
          ("<escape>" nil "quit")
             ("SPC" scroll-up)
             ("S-SPC" scroll-down)
             ("j" next-line)
             ("k" previous-line)
             ("h" backward-char)
             ("l" forward-char)
             ("J" irfc-scroll-up-one-line)
             ("K" irfc-scroll-down-one-line)
             ("G" end-of-buffer)
             ("g" beginning-of-buffer)
             ("T" irfc-render-toggle)
             ("q" irfc-quit)
             ("o" irfc-follow)
             ("v" irfc-visit)
             ("i" irfc-index)
             ("r" irfc-reference-goto)
             ("f" irfc-head-goto)
             ("F" irfc-head-number-goto)
             ("e" irfc-page-goto)
             ("n" irfc-page-next)
             ("p" irfc-page-prev)
             (">" irfc-page-last)
             ("<" irfc-page-first)
             ("t" irfc-page-table)
             ("H" irfc-head-next)
             ("L" irfc-head-prev)
             ("RET" irfc-table-jump)
             ("<tab>" irfc-rfc-link-next)
             ("<backtab>" irfc-rfc-link-prev))))

;; TODO jedi

;; [[https://github.com/tkf/emacs-jedi][Jedi]] предоставляет очень хорошее автодополнение для python-mode.

(use-package jedi
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  :config
  (setq jedi:complete-on-dot t))

;; know-your-http-well

;; Пакет [[https://github.com/for-GET/know-your-http-well][package]] предоставляет HTTP заголовки, media-типы,
;; методы, отношения и коды состояния. Все они сгруппированы и имеют ссылки на
;; спецификации.

(use-package know-your-http-well
  :ensure t
  :commands (http-header http-method http-relation http-status-code))

;; lorem-ipsum

;; [[https://github.com/jschaf/emacs-lorem-ipsum][lorem-ipsum]] добавляет текст-рыбу lorem ipsum для Emacs.

(use-package lorem-ipsum
  :ensure t
  :commands lorem-ipsum-insert-paragraphs)

;; lua-mode

;; [[https://github.com/immerrr/lua-mode][lua-mode]] основной режим для редактирования исходников Lua в Emacs.

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode))

;; TODO magit

;; С помощью [[https://github.com/magit/magit][Magit]], вы можете просматривать и редактировать свои репозитории Git с помошью Emacs.
;; Вы , например, можете просмотреть и закоммитить изменения, внесённые в
;; отслеживаемые файлы, а также просмотреть историю последних изменений. Есть
;; поддержка cherry picking, откатов, мерджей, ребэйзов и прочих общих операций Git.

(use-package magit
  :ensure t
  :pin melpa-stable
  :commands magit-status)

;; TODO markdown-mode

;; [[http://jblevins.org/projects/markdown-mode/][markdown-mode]] основной режим для редактирования Markdown-форматированных текстовых файлов в
;; GNU Emacs.

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; mingus

(use-package mingus
  :ensure t
  :config
  (use-package mingus-stays-home)
  )

;; TODO moe-theme
;; [[https://github.com/kuanyui/moe-theme.el][Moe-theme]] понравившаяся мне цветовая тема.

(use-package moe-theme
  :ensure t
  :config
  (load-theme 'moe-dark t))

;; Enable mu4e

(use-package mu4e
  :commands mu4e)

;; First load the user's sensitive information

;; This load the =mail.el.gpg= file where I store the email information
;; that I prefer to keep private (since I share this config in GitHub) to
;; inquisitive eyes.

(add-hook 'mu4e-main-mode-hook (lambda ()
    (load-library (concat user-emacs-directory "mail.el.gpg"))))

;; The rest of the SMTP configuration

;; This is the config needed to choose the right smtp configuration for
;; the proper account in each moment (for new messages, replies, forwards
;; & drafts editing).

;; set a stronger TLS configuration than the default to avoid nasty
;; warnings and for a little more secure configuration
(setq gnutls-min-prime-bits 2048)

;; the multiple functions that provide the multiple accounts selection functionality
(defun antares-mu4e-choose-account ()
    (completing-read (format "Compose with account: (%s) "
      (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
          (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                              nil t nil nil (caar my-mu4e-account-alist)))

(defun antares-mu4e-get-field (a)
    (let ((field (cdar (mu4e-message-field mu4e-compose-parent-message a))))
        (string-match "@\\(.*\\)\\..*" field)
        (match-string 1 field)))


(defun antares-mu4e-is-not-draft ()
    (let ((maildir (mu4e-message-field (mu4e-message-at-point) :maildir)))
       (if (string-match "drafts*" maildir)
              nil
              t)))

(defun antares-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
            (let ((field (if (antares-mu4e-is-not-draft)
                            (antares-mu4e-get-field :to)
                            (antares-mu4e-get-field :from))))
                (if (assoc field my-mu4e-account-alist)
                    field
                    (antares-mu4e-choose-account)))
            (antares-mu4e-choose-account)))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'antares-mu4e-set-account)

;; Queuing emails

;; When offline or when you want do delay a message, you can go to the
;; queuing mode and send them all turning it off.

;; Allow queuing mails
(setq smtpmail-queue-mail nil ;; start in non-queuing mode
    smtpmail-queue-dir "~/org/mail/mails/Queue")

;; Signature

;; Add the signature by default when a new email is composed.

(setq mu4e-compose-signature-auto-include t)
(setq
        message-signature t
        mu4e-compose-signature t)

;; Sending emails asynchronous

;; This is useful to send emails with attachments and do not block emacs
;; until end the transmission.

(use-package smtpmail-async
  :config
  (setq
   send-mail-function 'async-smtpmail-send-it
   message-send-mail-function 'async-smtpmail-send-it))

;; Setup maildir & folders

;; The default Maildir path and subfolders.

(setq
    mu4e-maildir       "~/org/mail"        ;; top-level Maildir
    mu4e-sent-folder   "/mails/Sent"       ;; folder for sent messages
    mu4e-drafts-folder "/mails/Drafts"     ;; unfinished messages
    mu4e-trash-folder  "/mails/Trash"      ;; trashed messages
    mu4e-refile-folder "/mails/Archive")   ;; saved messages

;; where store the saved attachments
(setq mu4e-attachment-dir  "~/temporal")

;; General Options

;; mu4e's general options.

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; do not ask for confirmation on exit
(setq mu4e-confirm-quit  nil)

;; set mu4e as the default emacs email client
(setq mail-user-agent 'mu4e-user-agent)

;; decorate mu main view
(defun antares-mu4e-main-mode-font-lock-rules ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([a-zA-Z]\\{1,2\\}\\)\\]" nil t)
      (add-text-properties (match-beginning 1) (match-end 1)
      '(face font-lock-variable-name-face)))))
(add-hook 'mu4e-main-mode-hook 'antares-mu4e-main-mode-font-lock-rules)

;; attempt to automatically retrieve public keys when needed
(setq mu4e-auto-retrieve-keys t)

;; don't reply to myself
(setq mu4e-compose-dont-reply-to-self t)

;; only personal messages get in the address book
(setq mu4e-compose-complete-only-personal t)

;; org-mode integration

;; Integrate with org-mode

(use-package org-mu4e
  :config
  (progn
    (setq org-mu4e-convert-to-html t)
    (defalias 'org-mail 'org-mu4e-compose-org-mode)))

;; Updating the email

;; Update the index every 2 minutes but don't retrieve the email via
;; Emacs.

(setq
  mu4e-get-mail-command "true" ;; or fetchmail, or ...
  mu4e-update-interval 120)    ;; update every 2 minutes

;; Header's view config

;; The headers view configuration.

;; more cool and practical than the default
(setq mu4e-headers-from-or-to-prefix '("" . "➜ "))
;; to list a lot of mails, more than the default 500
;; is reasonable fast, so why not?
(setq mu4e-headers-results-limit 750)
;; columns to show
(setq mu4e-headers-fields
    '(
      (:human-date . 9)
      (:flags . 6)
      (:mailing-list . 10)
      (:size . 6)
      (:from-or-to . 22)
      (:subject)))

;; Message view config

;; Config for view mode.

;; visible fields
(setq mu4e-view-fields
    '(
        :from
        :to
        :cc
        :bcc
        :subject
        :flags
        :date
        :maildir
        :mailing-list
        :tags
        :attachments
        :signature
))

;; program to convert to pdf
(setq mu4e-msg2pdf "/usr/bin/msg2pdf")

;; view email addresses not only the name
(setq mu4e-view-show-addresses t)

;; attempt to show images when viewing messages
(setq
   mu4e-view-show-images t
   mu4e-view-image-max-width 800)

;; use imagemagick if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; Editor view config

;; The editor view configuration.

;; kill the buffer when is no needed any more
(setq message-kill-buffer-on-exit t)

;; set the text width and activate the spell checking
(add-hook 'mu4e-compose-mode-hook (lambda ()
                                    (set-fill-column 80)
                                    (flyspell-mode)))

;; Message view actions

;; Define actions for message view.

;; add the action to open an HTML message in the browser
(add-to-list 'mu4e-view-actions
  '("browse mail" . mu4e-action-view-in-browser) t)

;; add the action to retag messages
(add-to-list 'mu4e-view-actions
  '("retag mail" . mu4e-action-retag-message) t)

;;Search for messages sent by the sender of the message at point
(defun antares-search-for-sender (msg)
    (mu4e-headers-search
        (concat "from:" (cdar (mu4e-message-field msg :from)))))

;; define 'x' as the shortcut
(add-to-list 'mu4e-view-actions
    '("xsearch for sender" . antares-search-for-sender) t)

;; integration with org-contacts
(setq mu4e-org-contacts-file "~/org/contacts.org")

(add-to-list 'mu4e-headers-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)
(add-to-list 'mu4e-view-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)


;; get a pgp key from a message
;; from  http://hugoduncan.org/post/snarf-pgp-keys-in-emacs-mu4e/
(defun antares-mu4e-view-snarf-pgp-key (&optional msg)
  "get the pgp key for the specified message."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
          (path (mu4e-message-field msg :path))
          (cmd (format "%s verify --verbose %s"
                 mu4e-mu-binary
                 (shell-quote-argument path)))
          (output (shell-command-to-string cmd)))
    (let ((case-fold-search nil))
      (when (string-match "key:\\([A-F0-9]+\\)" output)
        (let* ((cmd (format "%s --recv %s"
                            epg-gpg-program (match-string 1 output)))
               (output (shell-command-to-string cmd)))
          (message output))))))

(add-to-list 'mu4e-view-actions
             '("get PGP keys" . antares-mu4e-view-snarf-pgp-key) t)

;; Deal with HTML messages

;; Try to visualize as best as possible the HTML messages in text mode.

(setq mu4e-view-prefer-html t)
(setq mu4e-html2text-command "w3m -dump -cols 110 -T text/html")

;; autosmiley.el

;; [[http://www.emacswiki.org/emacs/autosmiley.el][autosmiley.el]] by Damyan Pepper

(use-package smiley
    :config
    (progn
      (defun autosmiley-overlay-p (overlay)
        "Return whether OVERLAY is an overlay of autosmiley mode."
        (memq (overlay-get overlay 'category)
              '(autosmiley)))

      (defun autosmiley-remove-smileys (beg end)
        (dolist (o (overlays-in beg end))
          (when (autosmiley-overlay-p o)
            (delete-overlay o))))

      (defvar *autosmiley-counter* 0
        "Each smiley needs to have a unique display string otherwise
        adjacent smileys will be merged into a single image.  So we put
        a counter on each one to make them unique")

      (defun autosmiley-add-smiley (beg end image)
        (let ((overlay (make-overlay beg end)))
          (overlay-put overlay 'category 'autosmiley)
          (overlay-put overlay 'display (append image (list :counter (incf *autosmiley-counter*))))))

      (defun autosmiley-add-smileys (beg end)
        (save-excursion
          (dolist (entry smiley-cached-regexp-alist)
            (let ((regexp (car entry))
                  (group (nth 1 entry))
                  (image (nth 2 entry)))
              (when image
                (goto-char beg)
                (while (re-search-forward regexp end t)
                  (autosmiley-add-smiley (match-beginning group) (match-end group) image)))))))

      (defun autosmiley-change (beg end &optional old-len)
        (let ((beg-line (save-excursion (goto-char beg) (line-beginning-position)))
              (end-line (save-excursion (goto-char end) (line-end-position))))
          (autosmiley-remove-smileys beg-line end-line)
          (autosmiley-add-smileys beg-line end-line)))

      ;;;###autoload
      (define-minor-mode autosmiley-mode
        "Minor mode for automatically replacing smileys in text with
        cute little graphical smileys."
        :group 'autosmiley :lighter " :)"
        (save-excursion
          (save-restriction
            (widen)
            (autosmiley-remove-smileys (point-min) (point-max))
            (if autosmiley-mode
                (progn
                  (unless smiley-cached-regexp-alist
                    (smiley-update-cache))
                  (jit-lock-register 'autosmiley-change))
              (jit-lock-unregister 'autosmiley-change))))))

;;**** Use gnome emoticons

;;Seen [[https://github.com/ahilsend/dotfiles/blob/3b9756a4f544403b7013bff80245df1b37feecec/.emacs.d/rc/rc-smiley.el][here]]

  (setq
      smiley-data-directory "/usr/share/icons/gnome/22x22/emotes/"
      smiley-regexp-alist '(("\\(:-?)\\)\\W" 1 "face-smile")
                            ("\\(;-?)\\)\\W" 1 "face-wink")
                            ("\\(:-|\\)\\W" 1 "face-plain")
                            ("\\(:-?/\\)[^/]\\W" 1 "face-uncertain")
                            ("\\(;-?/\\)\\W" 1 "face-smirk")
                            ("\\(:-?(\\)\\W" 1 "face-sad")
                            ("\\(:,-?(\\)\\W" 1 "face-crying")
                            ("\\(:-?D\\)\\W" 1 "face-laugh")
                            ("\\(:-?P\\)\\W" 1 "face-raspberry")
                            ("\\(8-)\\)\\W" 1 "face-cool")
                            ("\\(:-?\\$\\)\\W" 1 "face-embarrassed")
                            ("\\(:-?O\\)\\W" 1 "face-surprise")))
  (add-to-list 'gnus-smiley-file-types "png"))

;; View emoticons in mu4e

;; Show Smileys
(add-hook 'mu4e-view-mode-hook 'autosmiley-mode)
;; Test smileys:  :-] :-o :-) ;-) :-\ :-| :-d :-P 8-| :-(

;; Bookmarks

;; Мои закладки

(add-to-list 'mu4e-bookmarks
             '("flag:flagged" "Помеченные" ?f))
(add-to-list 'mu4e-bookmarks
             '("date:48h..now" "Последние 2 дня" ?l))
(add-to-list 'mu4e-bookmarks
             '("date:1h..now" "Последний час" ?h))
(add-to-list 'mu4e-bookmarks
             '("flag:attach" "С вложениями" ?a) t)
(add-to-list 'mu4e-bookmarks
             '("mime:application/pdf" "С документами PDF" ?d) t)
(add-to-list 'mu4e-bookmarks
             '("size:3M..500M" "Большие сообщения" ?b) t)

;; Shortcuts

;; My defined shortcuts

;; Folder shortcuts
(setq mu4e-maildir-shortcuts
  '(
    ("/mails/Archive" . ?a)
    ("/mails/business" . ?b)
    ("/mails/Drafts" . ?d)
    ("/mails/education" . ?e)
    ("/mails/Inbox" . ?i)
    ("/mails/joedicastro" . ?j)
    ("/mails/lists" . ?l)
    ("/mails/Local" . ?h)
    ("/mails/motley" . ?m)
    ("/mails/publicity" . ?p)
    ("/mails/Sent" . ?s)
    ("/mails/Spam" . ?x)
    ("/mails/Trash" . ?t)
    ("/mails/work" . ?w)))

;; Dired integration

;; Integration with Dired, so we can attach a file to a new email
;; directly from Dired.

;; | Binding     | Call              | Do                           |
;; |-------------+-------------------+------------------------------|
;; | C-c RET C-a | gnus-dired-attach | Attach a file to a new email |

(use-package gnus-dired
  :config
  (progn
    ;; make the `gnus-dired-mail-buffers' function also work on
    ;; message-mode derived modes, such as mu4e-compose-mode
    (defun gnus-dired-mail-buffers ()
      "Return a list of active message buffers."
      (let (buffers)
        (save-current-buffer
          (dolist (buffer (buffer-list t))
            (set-buffer buffer)
            (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
              (push (buffer-name buffer) buffers))))
        (nreverse buffers)))

    (setq gnus-dired-mail-mode 'mu4e-user-agent)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)))

;; Encrypt/Decrypt

;; Config for encrypt/decrypt emails

(setq mu4e-decryption-policy t)

;; ; Sign the messages by default
;; (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign)
;; ;rename to signature.asc
;; (defadvice mml2015-sign (after mml2015-sign-rename (cont) act)
;;   (save-excursion
;;     (search-backward "Content-Type: application/pgp-signature")
;;     (goto-char (point-at-eol))
;;     (insert "; name=\"signature.asc\"; description=\"Digital signature\"")))

;; Attachment reminder

;; To avoid memory faults, as forget to attach a file after mention it
;; in the message's body.

;; simple regexp used to check the message. Tweak to your own need.
(defvar antares-message-attachment-regexp "\\(adjunto\\|attach\\)")
;; the function that checks the message
(defun antares-message-check-attachment nil
  "Check if there is an attachment in the message if I claim it."
  (save-excursion
    (message-goto-body)
    (when (search-forward-regexp antares-message-attachment-regexp nil t nil)
      (message-goto-body)
      (unless (or (search-forward "<#part" nil t nil)
        (message-y-or-n-p
   "No attachment. Send the message ?" nil nil))
  (error "No message sent")))))
  ;; check is done just before sending the message
  (add-hook 'message-send-hook 'antares-message-check-attachment)

;; Open a mu4e search in a new frame

;; This is useful when you are composing a new email and need to do a
;; search in your emails to get a little context in the conversation.

(defun antares-mu4e-headers-search-in-new-frame
    (&optional expr prompt edit ignore-history)
        "Execute `mu4e-headers-search' in a new frame."
        (interactive)
        (select-frame (make-frame))
        (mu4e-headers-search expr prompt edit ignore-history))

;; mu4e-maildirs-extension

;; [[https://github.com/agpchil/mu4e-maildirs-extension][Mu4e maildirs extension]] добавляет почтовые папки в mu4e-main-view.

(use-package mu4e-maildirs-extension
  :ensure t
  :defer 0.8
  :config
  (progn
    (mu4e-maildirs-extension)
    (setq mu4e-maildirs-extension-maildir-separator    "*"
          mu4e-maildirs-extension-submaildir-separator "✉"
          mu4e-maildirs-extension-action-text          nil)))

;; multi-term

;; [[http://www.emacswiki.org/emacs/multi-term.el][multi-term]] для создания и управления множеством терминальных буферов в Emacs.

(use-package multi-term
  :ensure t
  :commands (multi-term multi-term-next)
  :config
  (setq multi-term-program "/bin/zsh"))

;; org-bullets

;; [[https://github.com/sabof/org-bullets][org-bullets]] показать org-mode маркеры как UTF-8 символы.

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿" "☼" "⚬"))
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; TODO org-plus-contrib

(use-package org-plus-contrib
  :ensure t)

;; org-protocol intercepts calls from emacsclient to trigger
;; custom actions without external dependencies. Only one protocol
;; has to be configured with your external applications or the
;; operating system, to trigger an arbitrary number of custom
;; actions.
;; to use it to capture web urls and notes from Firefox, install
;; this Firefox plugin, http://chadok.info/firefox-org-capture/
(use-package org-protocol
  :config
  (progn
  (setq org-protocol-default-template-key "w")
  (setq org-capture-templates
        (quote
         (("w" "Web captures" entry (file+headline "~/org/notes.org" "Web")
           "* %^{Title}    %^G\n\n  Source: %u, %c\n\n  %i"
           :empty-lines 1))))))

;; the org-contacts Emacs extension allows to manage your contacts
;; using Org-mode.

(use-package org-contacts
  :config
  (progn
    (setq org-contacts-file (concat org-directory "/contacts.org"))
    (setq org-contacts-matcher "EMAIL<>\"\"|ALIAS<>\"\"|PHONE<>\"\"|ADDRESS<>\"\"|BIRTHDAY")

    (add-to-list 'org-capture-templates
      '("p" "Contacts" entry (file org-contacts-file)
         "** %(org-contacts-template-name)
         :PROPERTIES:%(org-contacts-template-email)
         :END:"))))

;; org-capture
(add-to-list 'org-capture-templates
    '("t" "TODO" entry (file+headline "~/org/tasks.org" "Tasks")
       "* TODO %^{Task}  %^G\n   %?\n  %a"))
(add-to-list 'org-capture-templates
    '("n" "Notes" entry (file+headline "~/org/notes.org" "Notes")
       "* %^{Header}  %^G\n  %u\n\n  %?"))

;; org-pomodoro

;; [[https://github.com/lolownia/org-pomodoro][org-pomodoro]] добавляет очень простую поддержку техники Pomodoro в org-mode Emacs'а.

(use-package org-pomodoro
  :ensure t
  :commands org-pomodoro
  :pin melpa-stable)

;; ox-pandoc

;; [[https://github.com/kawabata/ox-pandoc][ox-pandoc]] переводит файлы org-mode в различные форматы, используя Pandoc.

(use-package ox-pandoc
  :defer 1
  :ensure t)

;; page-break-lines

;; [[./img/page-break-lines.png]]

;; [[https://github.com/purcell/page-break-lines][page-break-lines]] предоставляет глобальный режим, в котором уродливые символы перевода
;; строки =^L= отображаются как аккуратные горизонтальные черты.

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode t))

;; paradox

;; [[https://github.com/Bruce-Connor/paradox][Paradox]] проект модернизации Emacs'овского меню пакетов. С оценками пакетов,
;; статистикой использования, настраиваемостью и многим другим.

(use-package paradox
  :ensure t
  :commands paradox-list-packages
  :config
  (setq paradox-github-token t
        paradox-automatically-star nil
        paradox-execute-asynchronously t))

;; TODO parinfer
;;    [./img/parinfer.gif]
;;    [[https://github.com/DogLooksGood/parinfer-mode][parinfer]] умное расставление скобок

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil           ; If you use Evil.
            lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)))

;; TODO password-store

;; Поддержка [[http://www.zx2c4.com/projects/password-store/][Password store (pass)]] в Emacs.

;; - [ ] добавить собственную модификацию

(use-package password-store
  :ensure t
  :defer t)

;; pcache

;; [[https://github.com/sigma/pcache][pcache]] персистентное кэширование для Emacs. Необходимо для других пакетов таких как =fixmee=.

(use-package pcache
  :ensure t
  :init
  (setq pcache-directory (concat antares-emacs-temporal-directory "pcache" )))

;; pdf-tools

;; [[./img/pdf_tools.png]]

;; [[https://github.com/politza/pdf-tools][PDF Tools]] помимо всего прочего является заменой DocView для PDF файлов.
;; Ключевым отличием является то, что страницы не генерируются предварительно,
;; например тем же ghostscript, и хранятся в файловой системе, а создаются налету
;; по требованию и сохраняются в памяти.

;; [[https://github.com/markus1189/org-pdfview][org-pdfview]] добавляет поддержку ссылок org режима из pdfview буферов как docview.

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page
                pdf-view-use-imagemagick t
                pdf-view-midnight-colors '("white smoke" . "gray5"))
  (bind-keys :map pdf-view-mode-map
      ("\\" . hydra-pdftools/body)
      ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
      ("g"  . pdf-view-first-page)
      ("G"  . pdf-view-last-page)
      ("l"  . image-forward-hscroll)
      ("h"  . image-backward-hscroll)
      ("j"  . pdf-view-next-line-or-next-page)
      ("k"  . pdf-view-previous-line-or-previous-page)
      ("e"  . pdf-view-goto-page)
      ("t"  . pdf-view-goto-label)
      ("u"  . pdf-view-revert-buffer)
      ("al" . pdf-annot-list-annotations)
      ("ad" . pdf-annot-delete)
      ("aa" . pdf-annot-attachment-dired)
      ("am" . pdf-annot-add-markup-annotation)
      ("at" . pdf-annot-add-text-annotation)
      ("y"  . pdf-view-kill-ring-save)
      ("i"  . pdf-misc-display-metadata)
      ("s"  . pdf-occur)
      ("b"  . pdf-view-set-slice-from-bounding-box)
      ("r"  . pdf-view-reset-slice))

  (when (package-installed-p 'hydra)
    (bind-keys :map pdf-view-mode-map
               ("\\" . hydra-pdftools/body))
    (defhydra hydra-pdftools (:color blue :hint nil)
        "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
      ^^^_g_^^^       _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search      [_u_] revert buffer
      ^^^^↑^^^^       ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline     [_i_] info
      ^^^_p_^^^       ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link        [_d_] midgnight mode
      ^^^^↑^^^^       ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link [_D_] print mode
 _h_ ← _e_/_t_ → _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
      ^^^^↓^^^^       ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
      ^^^_n_^^^       ^ ^  _r_eset slice box
      ^^^^↓^^^^
      ^^^_G_^^^
   --------------------------------------------------------------------------------
        "
        ("\\" hydra-master/body "back")
        ("<ESC>" nil "quit")
        ("al" pdf-annot-list-annotations)
        ("ad" pdf-annot-delete)
        ("aa" pdf-annot-attachment-dired)
        ("am" pdf-annot-add-markup-annotation)
        ("at" pdf-annot-add-text-annotation)
        ("y"  pdf-view-kill-ring-save)
        ("+" pdf-view-enlarge :color red)
        ("-" pdf-view-shrink :color red)
        ("0" pdf-view-scale-reset)
        ("H" pdf-view-fit-height-to-window)
        ("W" pdf-view-fit-width-to-window)
        ("P" pdf-view-fit-page-to-window)
        ("n" pdf-view-next-page-command :color red)
        ("p" pdf-view-previous-page-command :color red)
        ("d" pdf-view-midnight-minor-mode)
        ("D" pdf-view-printer-minor-mode)
        ("b" pdf-view-set-slice-from-bounding-box)
        ("r" pdf-view-reset-slice)
        ("g" pdf-view-first-page)
        ("G" pdf-view-last-page)
        ("e" pdf-view-goto-page)
        ("t" pdf-view-goto-label)
        ("o" pdf-outline)
        ("s" pdf-occur)
        ("i" pdf-misc-display-metadata)
        ("u" pdf-view-revert-buffer)
        ("F" pdf-links-action-perfom)
        ("f" pdf-links-isearch-link)
        ("B" pdf-history-backward :color red)
        ("N" pdf-history-forward :color red)
        ("l" image-forward-hscroll :color red)
        ("h" image-backward-hscroll :color red)))

   (use-package org-pdfview
     :ensure t))

;; php
;; Режим [[https://github.com/ejmr/php-mode][PHP]] является основной режим для редактирования исходного кода PHP.
;; Это расширение режима C. Таким образом, он наследует все функциональные
;; возможности навигации C режима, но подсветка соответствует грамматике PHP и
;; углублена в соответствии с рекомендациями кодирования PEAR. Она также включает в
;; себя пару удобных функций IDE-типа, такие как поиск документации и просмотрщик
;; исходников и классов.

(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode)
  :interpreter ("php" . php-mode))

;; popwin

;; [[https://github.com/m2ym/popwin-el][Popwin]] это менеджер всплывающих окон Emacs, который освобождает вас от ада раздражающих буферов,
;; таких как *Help*, *Completions*, *compilation*, и так далее.

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (setq popwin:popup-window-height 35
        popwin:special-display-config
        '(("*Miniedit Help*" :noselect t)
          (help-mode :noselect nil)
          (completion-list-mode :noselect t)
          (compilation-mode :noselect nil)
          (grep-mode :noselect t)
          (occur-mode :noselect t)
          ("*Pp Macroexpand Output*" :noselect t)
          ("*Shell Command Output*")
          ("*Async Shell Command*")
          ("*vc-diff*")
          ("*vc-change-log*")
          (" *undo-tree*" :width 60 :position right)
          ("^\\*anything.*\\*$" :regexp t)
          ("*slime-apropos*")
          ("*slime-macroexpansion*")
          ("*slime-description*")
          ("*slime-compilation*" :noselect t)
          ("*slime-xref*")
          ("*Flycheck errors*")
          ("*Warnings*")
          ("*Process List*")
          ("*Smex: Unbound Commands*")
          ("*Paradox Report*" :noselect nil)
          ("*Diff*" :noselect nil)
          ("*Messages*" :noselect nil)
          ("*Google Maps*" :noselect nil)
          ("*ag search*" :noselect nil)
          ("*PDF-Occur*" :noselect nil)
          ("*PDF-Metadata*" :noselect nil)
          ("^\\*Outline .*\\.pdf\\*$" :regexp t :noselect nil)
          ("*MULTI-TERM-DEDICATED*" :noselect nil :stick t)
          (sldb-mode :stick t)
          (slime-repl-mode)
          (slime-connection-list-mode)))

  (add-hook 'popwin:after-popup-hook 'turn-off-evil-mode)
  (bind-keys :map popwin:window-map
             ((kbd "<escape>") . popwin:close-popup-window)))

;; TODO powerline

;; [[https://github.com/milkypostman/powerline][Powerline]]
;; Предлагаемая версия 2.0 оригинального Emacs Powerline, которая является форком
;; Powerline для Vim.
;; Emacs версия Vim powerline.
;; В эту версию включена поддержка UTF-8. UTF-8 разделители будут отображаться
;; юникодными символами правильно, например под mintty, так долго, сколько у вас
;; будут установлены пропатченные шрифты.
;; По умолчанию все терминальные режимы Emacs используют разделители UTF-8.

(use-package powerline
  :ensure t
  :defer t
  )

;; TODO NEOTree
;; [[./img/neotree.png]]
;; [[https://github.com/jaypei/emacs-neotree][Neotree]] это аналог NerdTree (менеджера файлов) в Vim.

(use-package neotree
  :ensure t)
(global-set-key [f8] 'neotree-toggle)

;; pretty-mode

;; [[https://github.com/akatov/pretty-mode][pretty-mode]] позволяет использовать математические /символы/ *Unicode* /symbols/ вместо выражений
;; или ключевых слов в некоторых языках программирования

(use-package pretty-mode
  :ensure t
  :defer t
  :config
  (add-hook 'prog-mode-hook 'turn-on-pretty-mode))

;; projectile

;; [[https://github.com/bbatsov/projectile][Projectile]] библиотека проектного взаимодействия для Emacs.
;; Цель библиотеки- обеспечение набора функция, работающих на уровне проекта без
;; внешних зависимостей (по фозможности). Например - поиск файлов проекта имеет
;; портативную реализацию , написанную на чистом Emacs Lisp, без использования GUI
;; поиска (но ради производительности используется внешний механизм индексирования
;; с помощью внешних команд).

(use-package projectile
  :ensure projectile
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-cache-file (concat antares-emacs-temporal-directory "projectile.cache"))
    (setq projectile-known-projects-file (concat antares-emacs-temporal-directory "projectile-bookmarks.eld"))
    (setq projectile-enable-caching t)
    (projectile-global-mode)))

;; TODO quickrun

;; [[https://github.com/syohex/emacs-quickrun][quickrun.el]] пакет, предназначенный для выполнения редактируемого буфера.
;; quickrun.el похож на интерпретатор, но quickrun.el предоставляет более удобные
;; команды. quickrun.el выполняет не только языки сценариев (Perl, Ruby, Python),
;; но и компилируемые языки(C, C++, Go, Java etc) и языки разметки.

(use-package quickrun
   :ensure t
   :defer t)

;; ranger

;;  [[https://github.com/ralesi/ranger.el][Ranger]] второстепенный режим, включаемый в Dired.
;;  Эмулирует многие из особенностей [[https://github.com/ralesi/ranger.el][ranger]].
;;  Этот второстепенный режим показывает стек родительских каталогов и обновляет
;;  родительские буферы во время навигации по файловой системе.

(use-package ranger
  :ensure t
  :config
  (setq ranger-cleanup-on-disable t
        ranger-show-dotfiles nil
        ranger-show-literal nil))

;; TODO web-mode
   
;;    [[./img/web-mode.png]]
;;    [[http://web-mode.org/][Web-mode]] основной режим для редактирования web-шаблонов
;;    [[https://github.com/fxbois/web-mode][Github]]

(use-package web-mode
  :ensure t
  )
