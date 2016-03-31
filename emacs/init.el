
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
