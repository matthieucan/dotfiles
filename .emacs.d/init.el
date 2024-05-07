;; emacs configuration

;; ====================
;; package management - straight.el / use-package.el
;; ====================

;; https://jeffkreeftmeijer.com/emacs-straight-use-package/
;; https://jeffkreeftmeijer.com/emacs-straight-use-package/
;; https://systemcrafters.net/advanced-package-management/using-straight-el/

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
             :custom (straight-use-package-by-default t))

;; Prevent Emacs-provided org from being loaded
(straight-register-package 'org)

;; ====================
;; emacs main config
;; ====================

;; See https://github.com/nasseralkmim/.emacs.d/blob/master/init.el
(use-package emacs
  :bind
  (("C-x C-b" . ibuffer)
   ("M-n" . "\C-u1\C-v\C-n")  ; scroll one line down
   ("M-p" . "\C-u1\M-v\C-p")  ; scroll one line up
   ("M-j" . replace-regexp)   ; search&replace (regex)
   ("M-k" . query-replace)    ; search&replace (string)
   ("C-c y" . duplicate-line)
   ("C-c c" . duplicate-line-and-comment)
   ("M-;" . my/comment-uncomment)
   ;; ("C-M-n" . forward-list)   ; go to end of sexp
   ;; ("C-M-p" . backward-list)  ; go to start of sexp
   )

  :custom-face
  ;; reproduce terminal colors for graphical emacs
  (default ((((type graphic) (min-colors 256)) (:foreground "#d8dee9" :background "#2e3440"))))
  (completion-common-part ((t (:foreground "blue"))))
  (completions-common-part ((t (:foreground "blue"))))
  (custom-state ((t (:foreground "green"))))
  (custom-variable-tag ((t (:foreground "blue"))))
  (diff-added ((t (:background "green" :foreground "black"))))
  (diff-removed ((t (:background "red" :foreground "black"))))
  (font-lock-comment-face ((t (:foreground "blue"))))
  (font-lock-function-name-face ((t (:foreground "cyan"))))
  (font-lock-keyword-face ((t (:foreground "yellow"))))
  (font-lock-string-face ((t (:foreground "brightmagenta"))))
  (font-lock-type-face ((t (:foreground "green"))))
  (font-lock-variable-name-face ((t (:foreground "cyan"))))
  (match ((t (:foreground "black" :background "yellow" :weight bold))))
  (minibuffer-prompt ((t (:foreground "yellow"))))
  (region ((t (:background "yellow" :foreground "black")))) ; region (selection)
  (highlight ((t (:background "red" :foreground "black" :weight bold))))
  (hl-line ((t (:background "color-233")))) ; highlighted line around cursor
  (isearch ((t (:foreground "black" :background "yellow"))))
  (isearch-fail ((t (:foreground "black" :background "brightred"))))
  (lazy-highlight ((t (:foreground "black" :background "blue"))))
  (show-paren-match ((t (:background "cyan" :foreground "black")))) ; matching parentheses
  (line-number ((t (:foreground "grey50")))) ; line numbers in fringe
  (line-number-current-line ((t (:foreground "white"))))
  (secondary-selection ((t (:foreground "black" :background "yellow"))))
  (vertical-border ((t (:foreground "blue"))))

  :hook
  (prog-mode . display-line-numbers-mode) ; line numbers on the left margin
  (conf-mode . display-line-numbers-mode)
  ;; (prog-mode . hl-line-mode) ; highlight current line

  :init
  (column-number-mode t) ; show line & column numbers in mode line
  (setq inhibit-startup-screen t) ; start emacs with scratch
  (tool-bar-mode -1) ; disable tool bar
  (menu-bar-mode -1) ; disable menu bar
  (when (display-graphic-p)
    (scroll-bar-mode -1)) ; disable scroll bar

  (setq use-short-answers t) ; answer quesions with y/n
  (setq confirm-kill-processes nil)

  ;; TODO smart parens https://github.com/Fuco1/smartparens
  (show-paren-mode 1) ; matching parentheses
  (setq show-paren-delay 0)

  ;; (setq lazy-highlight-cleanup nil) ; keep search matches highlighted
  ;; (setq lazy-highlight-max-at-a-time nil)
  ;; (setq lazy-highlight-initial-delay 0)

  (setq-default indent-tabs-mode nil) ; no tabs

  (setq require-final-newline t) ; add newline at the end of files

  ;; (setq display-line-numbers-type 'relative) ; line numbers relative to current line

  ;; line duplication, with or without commenting the first one
  (defun duplicate-line (&optional commentfirst)
    "duplicate line at point; if commentfirst is non-nil, comment the original" 
    (interactive)
    (beginning-of-line)
    (push-mark)
    (end-of-line)
    (let ((str (buffer-substring (region-beginning) (region-end))))
      (when commentfirst
        (comment-region (region-beginning) (region-end)))
      (insert
       (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
      (forward-line -1)))

  (defun duplicate-line-and-comment ()
    "duplicate line at point and comment the original"
    (interactive)
    (duplicate-line t)
    )

  ;; comment/uncomment either region, or line at position
  (defun my/comment-uncomment ()
    (interactive)
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (when (or (not transient-mark-mode) (region-active-p))
        (setq start (save-excursion
                      (goto-char (region-beginning))
                      (beginning-of-line)
                      (point))
              end (save-excursion
                    (goto-char (region-end))
                    ;; (end-of-line)
                    (point))))
      (comment-or-uncomment-region start end)))

  (save-place-mode 1) ; remember cursor position in files
  (setq save-place-forget-unreadable-files nil) ; speed-up exit

  (global-auto-revert-mode 1) ; always reload files from disk when changed

  (setq vc-follow-symlinks t) ; always follow symlinks when opening a file

  ;; save backup files (foo~) in separate directory
  (setq backup-directory-alist `(("." . "~/.saves")))

  ;; arrange completion list sorting vertically
  (setq completions-format 'vertical)

  ;; prettier vertical window divider, without space between |s
  (unless standard-display-table
    (setq standard-display-table (make-display-table))) ; ensure display-table is set
  (defun my-change-window-divider ()
    (let ((display-table (or buffer-display-table standard-display-table)))
      (set-display-table-slot display-table 5 ?│)
      (set-window-display-table (selected-window) display-table)))
  (add-hook 'window-configuration-change-hook 'my-change-window-divider)

  )

;; ====================
;; org
;; ====================

(use-package org
  :mode (("\\.org$" . org-mode))

  :hook
  (org-mode . visual-line-mode)
  (org-mode . (lambda () (set-fill-column 90)))


  ;; agenda: tag placement
  ;; place tags close to the right-hand side of the window
  (org-agenda-mode . (lambda () (setq org-agenda-tags-column (- (window-width) 13))))

  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c p" . org-capture)
	 ("C-c u" . my/org-narrow-parent))

  :custom-face
   (org-agenda-calendar-event ((t (:foreground "brightwhite" :background nil))))
   (org-agenda-calendar-sexp ((t (:foreground "brightwhite" :background nil :slant normal))))
   (org-agenda-date ((t (:foreground "yellow" :background nil :underline nil))))
   (org-agenda-date-today ((t (:foreground "yellow" :background nil :weight bold :slant normal))))
   (org-agenda-date-weekend ((t (:inherit 'org-agenda-date))))
   (org-agenda-date-weekend-today ((t (:inherit 'org-agenda-date-today))))
   ;; (org-agenda-dimmed-todo-face ((t (:foreground))))
   (org-agenda-done ((t (:foreground "green"))))
   (org-agenda-structure ((t (:foreground "blue" :background nil))))
   (org-time-grid ((t (:foreground "yellow"))))
   (org-block ((t (:foreground "brightcyan"))))
   (org-code ((t (:foreground "white"))))
   (org-column ((t (:background "brightblack"))))
   (org-column-title ((t (:inherit org-column :weight bold :underline t))))
   (org-date ((t (:foreground "blue" :underline t))))
   (org-document-info ((t (:foreground "cyan"))))
   (org-document-info-keyword ((t (:foreground "white"))))
   (org-document-title ((t (:weight bold :foreground "yellow"))))
   (org-done ((t (:foreground "green"))))
   (org-drawer ((t (:foreground "blue"))))
   (org-ellipsis ((t (:foreground "blue"))))
   (org-footnote ((t (:foreground "white"))))
   (org-formula ((t (:foreground "yellow"))))
   (org-headline-done ((t (:foreground "green" :bold nil :strike-through t))))
   ;; (org-hide ((t (:foreground  :background ))))
   (org-level-1 ((t (:weight bold :foreground "blue"))))
   (org-level-2 ((t (:weight bold :foreground "magenta"))))
   (org-level-3 ((t (:weight bold :foreground "cyan"))))
   (org-level-4 ((t (:weight bold :foreground "brightgreen"))))
   (org-level-5 ((t (:weight bold :foreground "yellow"))))
   (org-level-6 ((t (:weight bold :foreground "brightblue"))))
   (org-level-7 ((t (:weight bold :foreground "brightmagenta"))))
   (org-level-8 ((t (:weight bold :foreground "brightcyan"))))
   (org-link ((t (:foreground "yellow" :underline t))))
   (org-meta-line ((t (:foreground "yellow" :background))))
   (org-priority ((t (:foreground "red" :weight normal))))
   (org-scheduled ((t (:foreground "blue"))))
   (org-scheduled-previously ((t (:foreground "blue"))))
   (org-scheduled-today ((t (:foreground "blue"))))
   (org-sexp-date ((t (:foreground "white" ))))
   ;; (org-special-keyword ((t (:foreground ))))
   (org-table ((t (:foreground "blue"))))
   (org-tag ((t (:foreground "yellow" :background "color-234" :weight bold))))
   (org-todo ((t (:foreground "red" :weight bold))))
   (org-upcoming-deadline ((t (:foreground "red" :weight bold))))
   (org-deadline-announce ((t (:foreground "red" :weight bold))))
   (org-warning ((t (:foreground "red" :weight bold))))

  :config
  ;; ;; color code blocks in org-mode
  ;; (setq org-src-fontify-natively t)

  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2018-04/msg00455.html
  (defun my-skip-entries-below-scheduled-entry ()
    "Exclude sub-items from todo list in agenda if their parent is scheduled."
    (when (save-excursion
            (org-up-heading-safe)
            (org-get-scheduled-time (point)))
      (org-end-of-subtree t t)))

  (defun my-skip-entries ()
    "Skip todo entries."
    (or (org-agenda-skip-entry-if 'scheduled 'deadline) (my-skip-entries-below-scheduled-entry)))

  ;; all content is folded when starting up
  (setq org-startup-folded t)

  ;; hide some tags in agenda views
  (setq org-agenda-hide-tags-regexp "picnic\\|ongoing\\|anothertag")

  ;; TODO
  (setq message-log-max 10000)

  ;; org-mode my agenda view
  (setq org-agenda-custom-commands
        '(
          ("h" "Home"
           (
            ;; Main agenda
            (agenda "")
            ;; Other TODOs
            (
             tags-todo
             "-goodidea"
             (
              (org-agenda-skip-function '(my-skip-entries))
              (org-tags-match-list-sublevels nil)
              (org-agenda-todo-list-sublevels nil)
              (org-agenda-overriding-header "General TODO list")
              (org-agenda-sorting-strategy '(priority-down todo-state-down))
              ))

            ;; Ideas
            (
             tags
             "goodidea"
             (
              (org-agenda-overriding-header "Ideas worth thinking about")
              (org-tags-match-list-sublevels nil)
              )
             )
	    )
	   (
            ;; (org-agenda-tag-filter-preset '("-picnic"))
            (org-agenda-files (remove "~/org/picnic.org" org-agenda-files))
            ;; (org-agenda-files '("~/org/test.org"))
            )
	   )
	  ("p" "Picnic"
	   (
	    (agenda ""
		    (
		     (org-agenda-files '("~/org/picnic.org" "~/org/contacts.org"))
		     ;; (org-agenda-sorting-strategy '(todo-state-up priority-down))
                     (org-agenda-use-time-grid nil)
		     )
		    )
	    (tags "ongoing-upcoming"
		  (
		   (org-agenda-overriding-header "Ongoing projects")
		   (org-agenda-sorting-strategy '(priority-down todo-state-up))
		   )
		  )
	    (tags "upcoming"
		  (
		   (org-agenda-overriding-header "Upcoming projects")
		   (org-agenda-sorting-strategy '(priority-down todo-state-up))
		   )
		  )
	    (tags "task" ((org-agenda-overriding-header "Clockable tasks")))
	    (todo "TODO|NEXT|CURR|BLOC|DELG"
		  (
		   (org-agenda-files '("~/org/picnic.org"))
		   (org-agenda-overriding-header "TODO list")
		   (org-agenda-sorting-strategy '(priority-down todo-state-down))
		   )
		  )
	    )
	   )
	  )
	)

  ;; (setq org-use-tag-inheritance nil)

  ;; org-agenda time grid settings
  (setq org-agenda-time-grid
        (quote
         ((today require-timed remove-match)
          (800 1000 1200 1400 1600 1800 2000)
          "....."
          "----------------")))

  ;; priorities
  (setq org-priority-faces
        '((?A . (:foreground "darkred" :background "color-246" :weight bold))
          (?B . (:foreground "yellow"))
          (?C . (:foreground "green"))))

  ;; todo items
  (setq org-todo-keywords
        '((sequence "TODO" "BLOC" "NEXT" "CURR" "|" "NOPE" "DONE")))

  (setq org-todo-keyword-faces
        '(
         ("TODO" . (:foreground "white" :background "red" :weight bold))
         ("NEXT" . (:foreground "black" :background "yellow" :weight bold))
         ("CURR" . (:foreground "black" :background "yellow" :weight bold))
         ("REVW" . (:foreground "black" :background "darkgreen" :weight bold))
         ("OPEN" . (:foreground "black" :background "white" :weight bold))
         ("BLOC" . (:foreground "red" :background "brightblack" :weight bold))
         ("NOPE" . (:foreground "black" :background "blue" :weight bold))
         ("DONE" . (:foreground "white" :background "green" :weight bold))))

  ;; org-agenda, when jumping to entry, narrow buffer
  (advice-add 'org-agenda-goto :after
              (lambda (&rest args)
                (org-narrow-to-subtree)))

  ;; org-agenda files
  (setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
  ;; (setq org-agenda-files '("~/org/test.org"))

  ;; follow hyperlinks with Enter
  (setq org-return-follows-link 1)

  ;; open file links in same frame
  (setq org-link-frame-setup (quote ((file . find-file))))

  ;; hide todo and tags subitems in global list
  (setq org-agenda-todo-list-sublevels nil)
  (setq org-tags-match-list-sublevels nil)

  ;; set default deadline reminder to 3 days before
  (setq org-deadline-warning-days 3)

  ;; geographical location, for sunset & sunrise times
  (setq calendar-latitude 52.4)
  (setq calendar-longitude 4.9)

  ;; org-agenda: don't display items DONE today
  (setq org-agenda-skip-scheduled-if-done 1)
  (setq org-agenda-skip-deadline-if-done 1)
  (setq org-agenda-skip-timestamp-if-done 1)

  ;; org-agenda: don't display scheduled/deadlined/timestamped items
  (setq org-agenda-todo-ignore-with-date 1)
  (setq org-agenda-tags-todo-honor-ignore-options 1)

  ;; allow org files to customize todo faces
  (put 'org-todo-keyword-faces 'safe-local-variable #'list)

  ;; capture
  (setq org-default-notes-file "~/org/log.org")
  (setq org-capture-templates
        '(("t" "TODO" entry (file+headline "~/org/gtd.org" "Tasks")
           "* TODO %?\n  %i\n  Added on %U\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/log.org")
           "* %?\n  Entered on %U\n%i\n  %a")
          ("J" "Journal (displayed in agenda)" entry (file+datetree "~/org/log.org")
           "* %?\n  Entered on %T\n%i\n  %a")
          ("p" "Picnic note" entry (file+olp+datetree "~/org/picnic.org" "Notes" "Captured")
           "* Note: %?                                                          :note:\n%T\n"
           :empty-lines 1 :empty-lines-after 2 :clock-keep t :tree-type month)
          ))

  ;; log time when task is done
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; duration format in clock tables
  ;; either "1d 2:34" when duration>1day, or "2:34" otherwise
  (setq org-duration-format '(("d" . nil) (special . h:mm)))

  ;; set duration units: 1 day = 8 hours, 1 week = 5 days
  (setq org-duration-units
        `(("min" . 1)
          ("h" . 60)
          ("d" . ,(* 60 8))
          ("w" . ,(* 60 8 5))))

  ;; necessary call after setting org-duration variables
  (org-duration-set-regexps)

  ;; move narrowed view 1 level up
  (defun my/org-narrow-parent ()
    "Select parent of current subtree and narrow to it."
    (interactive)
    (widen)
   (outline-up-heading 1 t)
   (org-reveal)
   (org-narrow-to-subtree)
   (outline-hide-subtree) ; hide and cycle renders better wrt empty lines between subtrees
   (org-cycle))

   ;; org agenda block separator
   (setq org-agenda-block-separator ?─)

  ;; other org modules
  ;; (load-file "~/.emacs.d/org-clock-csv.el")
  ;; (add-to-list 'org-modules 'org-habit t)
  )

;; ====================
;; markdown
;; ====================

(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode))

  :hook
  (markdown-mode . (lambda () (set-fill-column 88)))
  (markdown-mode . auto-fill-mode)
  )

;; ====================
;; Dockerfile
;; ====================

(use-package dockerfile-mode
  :mode (("Dockerfile" . dockerfile-mode))
)

;; ====================
;; dumb-jump
;; ====================

(use-package dumb-jump
  :bind (("C-M-j" . dumb-jump-go)   ; jump to definition
         ("C-M-k" . dumb-jump-back) ; come back
	 )
  )

;; ====================
;; modeline
;; ====================

;; control minor-mode indication in the mode-line
(use-package diminish
  :demand
  )

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (setq sml/mule-info nil) ; hide mule info
  (setq sml/replacer-regexp-list '((".*" ""))) ; only show file name, not path
  (setq sml/modified-char "*")

  :custom-face
  (mode-line ((t (:foreground "black" :background "color-233" :underline nil))))
  (mode-line-inactive ((t (:foreground "white":background nil :underline nil))))
  (sml/col-number ((t (:foreground "yellow"))))
  (sml/line-number ((t (:inherit 'sml/col-number))))
  (sml/modified ((t (:background "darkred" :foreground "white" :weight bold))))

  :config
  (sml/setup)
  )

;; ====================
;; anzu
;; ====================

; search stats in modeline
; https://github.com/emacsorphanage/anzu

(use-package anzu
  :diminish anzu-mode

  :custom-face
   (anzu-mode-line ((t (:foreground "yellow"))))
   (anzu-mode-line-no-match ((t (:foreground "red"))))

  :config
  (global-anzu-mode 1)
  )

;; ====================
;; company
;; ====================

(use-package company
  :diminish
  :bind
  (("M-s" . company-complete)
   )
  :config
  (global-company-mode 1)
  )

;; ====================
;; ivy/swiper/counsel
;; ====================

(use-package ivy
  :diminish
  :bind
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-h f" . counsel-describe-function)
   ("C-x b" . ivy-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-h v" . counsel-describe-variable)
   ("C-c i" . ivy-resume)
   ("C-c j" . counsel-git-grep)
   ("C-s" . swiper)
   ("C-r" . swiper-backward)
   (:map ivy-minibuffer-map
         ("<tab>" . ivy-alt-done)
         ("<return>" . ivy-done)
         ("C-o" . ivy-immediate-done)))

  :init
  (use-package counsel
    :diminish
    :config
    (counsel-mode))

  (use-package swiper
    :diminish
    )

  ;; columns in ivy-switch-buffer
  (use-package ivy-rich
    :config
    (ivy-rich-mode)
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    )

  :custom-face
   (ivy-highlight-face ((t (:foreground "black" :background "yellow"))))
   (ivy-minibuffer-match-face-1 ((t (:inherit 'ivy-highlight-face))))
   (ivy-minibuffer-match-face-2 ((t (:inherit 'ivy-highlight-face))))
   (ivy-minibuffer-match-face-3 ((t (:inherit 'ivy-highlight-face))))
   (ivy-minibuffer-match-face-4 ((t (:inherit 'ivy-highlight-face))))
   (ivy-minibuffer-match-highlight ((t (:inherit 'ivy-highlight-face))))

  :config
  (setq
   ivy-use-virtual-buffers t
   ivy-count-format "%d/%d "
   ivy-wrap t)

  (ivy-mode)
)

;; TODO https://codeberg.org/akib/emacs-corfu-terminal

;; https://github.com/company-mode/company-quickhelp
;; https://github.com/jcs-legacy/company-quickhelp-terminal
;; TODO doesn't work
;; (use-package company-quickhelp-terminal
;;   :config
;;   (company-quickhelp-terminal-mode)
;;   (setq company-quickhelp-use-propertized-text nil)
;;   )

;; No terminal support
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package popup
  :custom-face
  (popup-tip-face ((t (:foreground "black" :background "yellow"))))

  )

(use-package eldoc
  :config
  ;; (defun my-eldoc-display-message (format-string &rest args)
  ;;   "Display eldoc message near point."
  ;;   (when format-string
  ;;     (popup-tip (apply 'format format-string args))))
  ;; (setq eldoc-message-function #'my-eldoc-display-message)
  )

;; ====================
;; python
;; ====================

(use-package python
  :straight (:type built-in)

  :hook
  (python-mode . (lambda () (set-fill-column 88)))

  ;; LSP for Python, through eglot
  ;; https://github.com/python-lsp/python-lsp-server
  ;; python3 -m pip install 'python-lsp-server[all]' python-lsp-black pylsp-mypy
  (python-mode . eglot-ensure)

  :bind
  (:map python-mode-map
        ("M-q" . python-fill-paragraph)
   )

  :config
  (setq python-fill-docstring-style 'pep-257-nn)
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)

  (setq eglot-workspace-configuration
        '(:pylsp
          (:plugins
           (:pycodestyle
            (:enabled :json-false)
            :mccabe
            (:enabled :json-false)
            :pyflakes
            (:enabled :json-false)
            :flake8
            (:enabled t)
            )
           :configurationSources ["flake8"])))
  )

;; activate virtual environment with pyvenv-activate
(use-package pyvenv
  :config
  (pyvenv-mode t)
  )

;; ====================
;; flymake
;; ====================

(use-package flymake
  :straight (:type built-in)
  :custom-face
  (flymake-error ((t (:inherit 'flymake-warning))))
  )

;; ====================
;; eglot
;; ====================

(use-package eglot
  :hook
  (after-save . (lambda () (when (and (boundp 'eglot--managed-mode) eglot--managed-mode)
                             (eglot-format))))

  :bind
  (:map eglot-mode-map
	("C-M-j" . xref-find-definitions)
	("C-M-k" . xref-go-back)
	("M-r" . eglot-rename)
        ("M-t" . xref-find-references)
	("M-i" . flymake-goto-next-error)
	("M-I" . flymake-goto-prev-error))
  )

;; ====================
;; yaml
;; ====================

(use-package yaml-mode
  :mode (("\\.yaml" . yaml-mode)
         ("\\.yml" . yaml-mode)
         ("\\.gotmpl" . yaml-mode))
  )

;; ====================
;; git
;; ====================

;; TODO magit

;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :hook
  (prog-mode . git-gutter-mode)
  (conf-mode . git-gutter-mode)

  :diminish git-gutter-mode

  :custom-face
  (git-gutter:modified ((t (:foreground "black" :background "blue" :weight bold))))
  (git-gutter:added ((t (:foreground "black" :background "green" :weight bold))))
  (git-gutter:deleted ((t (:foreground "black" :background "red" :weight bold))))
  (git-gutter:unchanged ((t (:background nil))))

  :config
  (setq git-gutter:update-interval 1)
  (setq git-gutter:modified-sign "~")
  (setq git-gutter:added-sign "+")
  (setq git-gutter:deleted-sign "-")
  ;; prevents fringe from appearing and disappearing when file status changes
  (setq git-gutter:unchanged-sign " ")
  (setq git-gutter:ask-p nil) ; don't ask confirmation to stage hunk
  (setq git-gutter:hide-gutter t) ; hide gutter when there are no changes
  )

;; ====================
;; rainbow-mode
;; ====================

;; add color background to hexadecimal entries
(use-package rainbow-mode)


;; ====================
;; which-key
;; ====================

;; https://github.com/justbur/emacs-which-key
;; show list of possible commands after prefix
(use-package which-key
  :diminish which-key-mode

  :config
  (which-key-mode)
  )

;; ====================
;; consult
;; ====================

;; https://github.com/minad/consult

;; (use-package consult
;;   )

;; ====================
;; projectile
;; ====================

;; https://github.com/bbatsov/projectile
;; project-level commands

(use-package projectile
  :init
  (projectile-mode +1)

  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map))

  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy)
  ;; customize mode line
  (setq projectile-mode-line-function
        '(lambda () (format " Proj[%s]" (projectile-project-name))))
  )

(use-package counsel-projectile
  :after projectile

  :config
  (counsel-projectile-mode)
  )

;; ====================
;; deadgrep
;; ====================

;; https://github.com/Wilfred/deadgrep
;; ripgrep frontend

;; Within search results:
;; - enter: visit
;; - o: visit in another window
;; - n/p: move between results
;; - N/P: move between hits
;; - M-n/M-p: move between files
;; - S: change search term
;; - ^: rerun search in parent directory
;; - g: rerun search
;; - deadgrep-edit-mode: edit results (e.g. replace)

(use-package deadgrep
  :bind (("C-c r" . deadgrep) ; search recursively
	 )

  :config
  ;; add parameters to rg
  ;; https://github.com/Wilfred/deadgrep/issues/24#issuecomment-942290197
  (defun deadgrep--include-args (rg-args)
    (push "--hidden" rg-args) ; consider hidden folders/files
    ;; (push "--follow" rg-args) ; follow symlink
    )
  (advice-add 'deadgrep--arguments :filter-return #'deadgrep--include-args)
  )

;; ====================
;; Lisp
;; ====================

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-quicklisp slime-asdf))
  )


;; ====================
;; Hide/Show
;; ====================

;; allow folding parts of a buffer
(use-package hideshow
  :straight (:type built-in)

  :bind (("<backtab>" . hs-toggle-hiding) ; shift + tab
	 )

  :diminish hs-minor-mode

  :hook
  (prog-mode . hs-minor-mode)

  :config
  ;; add overlay to indicate folded lines
  (defun display-folded-overlay (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let ((overlay-string (format "...%d lines"
                                    (count-lines (overlay-start ov)
                                                 (overlay-end ov)))))
        (defface overlay-string-face
          '((t (:background "green" :foreground "black" :box t)))
          "Face to hightlight the ... area of hidden regions"
          :group 'hideshow)
        (put-text-property 0 (length overlay-string)
                           'face 'overlay-string-face overlay-string)
        (overlay-put ov 'display overlay-string)
        )))
  (setq hs-set-up-overlay #'display-folded-overlay)
  )

;; ====================
;; Writeroom mode
;; ====================

(use-package writeroom-mode
  )

;; ====================
;; prettier minor mode
;; ====================

(use-package prettier-js
  :hook
  (markdown-mode . prettier-js-mode)
  (js-mode . prettier-js-mode)

  :init
  (setq prettier-js-args '(
  "--print-width" "88"
  "--prose-wrap" "always"
  ))
  )

;; TODO
;; tramp-mode, editing remote files
;; helm https://github.com/emacs-helm/helm
;; neotree https://github.com/jaypei/emacs-neotree
;; projectile https://github.com/bbatsov/projectile

;; Tips

;; Emacs is slow to quit

;; C-h v emacs-kill-hook
;; ido is a good candidate

(use-package face-explorer
  :defer t
  )
