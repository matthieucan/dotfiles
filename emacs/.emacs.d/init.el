;; emacs configuration

;; ====================
;; package management - straight.el / use-package.el
;; ====================

;; https://jeffkreeftmeijer.com/emacs-straight-use-package/
;; https://jeffkreeftmeijer.com/emacs-straight-use-package/

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
  :bind (("C-x C-b" . ibuffer)
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

  :hook
  (prog-mode . display-line-numbers-mode) ; line numbers on the left margin
  (prog-mode . display-fill-column-indicator-mode) ; fill column on the right

  :init
  ;; (global-hl-line-mode t) ; highlight current line
  (column-number-mode t) ; show line & column numbers in mode line
  (setq inhibit-startup-screen t) ; start emacs with scratch
  (tool-bar-mode -1) ; disable tool bar
  (menu-bar-mode -1) ; disable menu bar
  (scroll-bar-mode -1) ; disable scroll bar

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

  (ido-mode 1) ; interactively do things
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)

  (global-auto-revert-mode 1) ; always reload files from disk when changed

  (setq vc-follow-symlinks t) ; always follow symlinks when opening a file

  ;; save backup files (foo~) in separate directory
  (setq backup-directory-alist `(("." . "~/.saves")))

  ;; arrange completion list sorting vertically
  (setq completions-format 'vertical)

  )

;; ====================
;; theme
;; ====================

;; M-x list-faces, M-x list-color-displays
;; M-x describe-face, M-x customize-themes
;; M-x describe-text-properties

(use-package solarized-theme
  :init
  (load-theme 'solarized-wombat-dark t)
  (custom-theme-set-faces
   'solarized-wombat-dark

   `(default ((t (:background nil))))

   `(font-lock-comment-face ((t (:foreground "blue"))))

   ;; region (selection)
   `(region ((t (:background "yellow" :foreground "black"))))

   ;; highlighted line around cursor
   `(hl-line ((t (:background "color-233"))))

   ;; matching parenthese
   `(show-paren-match ((t (:background "yellow" :foreground "black"))))

   ;; line numbers
   `(line-number-current-line ((t (:foreground "yellow"))))

   ;; column indicator
   `(fill-column-indicator ((t (:foreground "color-233"))))
   )
  )

;; (use-package nord-theme
;;   :init
;;   (load-theme 'nord t)
;;   )

;; (use-package rebecca-theme
;;   :init
;;   (load-theme 'rebecca t)
;;   (custom-theme-set-faces
;;    'rebecca
;;   (enable-theme 'rebecca)))

;; ====================
;; org
;; ====================

(use-package org
  :load-path "~/git/org-mode/lisp"
  :mode (("\\.org$" . org-mode))

  :hook
  (org-mode . visual-line-mode)

  ;; agenda: tag placement
  ;; place tags close to the right-hand side of the window
  (org-agenda-mode . (lambda () (setq org-agenda-tags-column (- (window-width) 12))))

  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c p" . org-capture)
	 ("C-c u" . my/org-narrow-parent))

  :custom-face
  ;; some faces need to be inverted with :inverse-video
  ;; something somewhere is messing up with faces
  ;; and this is not reflected in describe-face

   (org-agenda-calendar-event ((t (:foreground "brightwhite"))))
   (org-agenda-date ((t (:foreground "yellow" :background nil :underline nil))))
   (org-agenda-date-today ((t (:foreground "yellow" :background nil :weight bold :underline nil))))
   (org-agenda-date-weekend ((t (:foreground "yellow" :weight medium :underline nil))))
   (org-agenda-date-weekend-today ((t (:foreground "yellow" :background nil :weight bold :underline nil :inverse-video nil))))
   ;; (org-agenda-dimmed-todo-face ((t (:foreground))))
   (org-agenda-done ((t (:foreground "green" :background "black" :inverse-video t))))
   (org-agenda-structure ((t (:foreground "blue"))))
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
   (org-ellipsis ((t (:foreground "blue"))))
   (org-footnote ((t (:foreground "white"))))
   (org-formula ((t (:foreground "yellow"))))
   (org-headline-done ((t (:foreground "green" :bold nil :strike-through t))))
   ;; (org-hide ((t (:foreground  :background ))))
   (org-level-1 ((t (:weight bold :foreground "blue"))))
   (org-level-2 ((t (:weight bold :foreground "magenta"))))
   (org-level-3 ((t (:weight bold :foreground "cyan"))))
   (org-level-4 ((t (:weight bold :foreground "brightgreen"))))
   (org-level-5 ((t (:weight bold :foreground "brightyellow"))))
   (org-level-6 ((t (:weight bold :foreground "brightblue"))))
   (org-level-7 ((t (:weight bold :foreground "brightmagenta"))))
   (org-level-8 ((t (:weight bold :foreground "brightcyan"))))
   (org-link ((t (:foreground "yellow" :underline t))))
   (org-meta-line ((t (:foreground "orange" :background "black" :inverse-video t))))
   (org-priority ((t (:foreground "red"))))
   (org-scheduled ((t (:foreground "blue"))))
   (org-scheduled-previously ((t (:foreground "blue"))))
   (org-scheduled-today ((t (:foreground "blue"))))
   ;; (org-sexp-date ((t (:foreground ))))
   ;; (org-special-keyword ((t (:foreground ))))
   ;; (org-table ((t (:foreground ))))
   (org-tag ((t (:foreground "yellow" :background "color-234" :weight bold))))
   (org-todo ((t (:foreground "red" :weight bold))))
   ;; (org-upcoming-deadline ((t (:foreground "red" :weight bold))))
   ;; (org-deadline-announce ((t (:foreground "red" :weight bold))))
   (org-warning ((t (:weight bold :foreground "darkred"))))

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
		     (org-agenda-files '("~/org/picnic.org"))
		     ;; (org-agenda-sorting-strategy '(priority-down todo-state-down))
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
	    (tags-todo "TODO=\"DELG\"-ongoing"
		  (
		   (org-agenda-files '("~/org/picnic.org"))
		   (org-agenda-overriding-header "Delegated")
		   (org-agenda-sorting-strategy '(priority-down todo-state-down))
		   )
		  )
	    (todo "TODO|NEXT|CURR|BLOC"
		  (
		   (org-agenda-files '("~/org/picnic.org"))
		   (org-agenda-overriding-header "TODO list")
		   (org-agenda-sorting-strategy '(priority-down todo-state-down))
		   )
		  )
	    (todo "OPEN|BLOC|CURR|REVW|TODO"
		  (
		   (org-agenda-files '("~/org/jira/MUR.org"))
		   (org-agenda-overriding-header "JIRA/MUR")
		   (org-agenda-sorting-strategy '(priority-down todo-state-down))
		   )
		  )
	    (todo "OPEN|BLOC|CURR|REVW|TODO"
		  (
		   (org-agenda-files '("~/org/jira/DATA.org"))
		   (org-agenda-overriding-header "JIRA/DATA")
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
        '((?A . (:foreground "darkred" :background "white" :weight bold))
          (?B . (:foreground "yellow"))
          (?C . (:foreground "green"))))

  ;; todo items
  (setq org-todo-keywords
        '((sequence "TODO" "BLOC" "NEXT" "CURR" "|" "NOPE" "DONE")))

  (setq org-todo-keyword-faces
        '(
         ("TODO" . (:foreground "white" :background "darkred" :weight bold))
         ("NEXT" . (:foreground "black" :background "yellow" :weight bold))
         ("CURR" . (:foreground "black" :background "yellow" :weight bold))
         ("REVW" . (:foreground "black" :background "darkgreen" :weight bold))
         ("OPEN" . (:foreground "black" :background "white" :weight bold))
         ("BLOC" . (:foreground "darkred" :background "white" :weight bold))
         ("NOPE" . (:foreground "black" :background "blue" :weight bold))
         ("DONE" . (:foreground "darkgreen" :weight bold))))

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

  ;; other org modules
  ;; (load-file "~/.emacs.d/org-clock-csv.el")
  ;; (add-to-list 'org-modules 'org-habit t)

  ;; ; org-jira
  ;; (setq org-jira-working-dir "~/org/jira")
  ;; (setq jiralib-url "https://picnic.atlassian.net")
  ;; (setq org-jira-jira-status-to-org-keyword-alist
  ;;       '(("In Progress" . "CURR")
  ;;         ("In Review" . "REVW")
  ;;         ("To Do" . "TODO")
  ;;         ("Blocked" . "BLOC")
  ;;         ("Open" . "OPEN")
  ;;         ("Draft" . "OPEN")
  ;;         ("Backlog" . "OPEN")
  ;;         ("To Deploy" . "DONE")))

  ;; ; set custom JQL
  ;; (setq org-jira-custom-jqls
  ;;   '(
  ;;     (:jql " project = 'DATA' AND assignee = currentUser() AND status IN ('Blocked','In Progress','In Review','Open','To Do','To Deploy') ORDER BY created DESC "
  ;;           :limit 100
  ;;           :filename "DATA")
  ;;     (:jql " project = 'PY' AND assignee = currentUser() AND status IN ('Blocked','Draft','In Progress','In Review','Open','To Do') ORDER BY created DESC "
  ;;           :limit 100
  ;;           :filename "PY")
  ;;     (:jql " project = 'MUR' AND assignee = currentUser() AND status IN ('Backlog','Blocked','In Progress','In Review','To Do') ORDER BY created DESC "
  ;;           :limit 100
  ;;           :filename "MUR")
  ;;    ))

  ;; ; do not create deadline entries, I don't use them and it makes issues disappear from agenda
  ;; (setq org-jira-deadline-duedate-sync-p nil)

  ;; ; bind sortcut to refresh JIRA issues
  ;; (global-set-key "\C-cj" 'org-jira-get-issues-from-custom-jql)
  )

;; ====================
;; markdown
;; ====================

(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode))

  :hook
  (markdown-mode . (lambda () (set-fill-column 80)))
  (markdown-mode . display-fill-column-indicator-mode)
  )

;; ====================
;; Dockerfile
;; ====================

(use-package dockerfile-mode
  :mode (("Dockerfile" . dockerfile-mode))
)

;; ====================
;; ido-vertical-mode
;; ====================

(use-package ido-vertical-mode
  :config
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (ido-vertical-mode 1)
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
  (mode-line ((t (:foreground "black" :background "color-236"))))
  (mode-line-inactive ((t (:background nil))))
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
   (anzu-mode-line ((t (:foreground "brightyellow" :background "black"))))
   (anzu-mode-line-no-match ((t (:foreground "red" :background "black"))))

  :config
  (global-anzu-mode 1)
  )

;; ====================
;; company
;; ====================

(use-package company
  :config
  (global-company-mode 1)
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


;; ====================
;; python
;; ====================

;; TODO configure LSP
;; https://ddavis.io/posts/emacs-python-lsp/

;; M-x elpy-config

(use-package elpy
  :mode (("\\.py$" . elpy))

  :init
  (elpy-enable)

  :config
  (setq elpy-rpc-python-command "python3")
  ;; disable indent guides
  (setq elpy-modules (remove 'elpy-module-highlight-indentation elpy-modules))
)

(use-package company-jedi
  :after company

  :config
  (add-to-list 'company-backends 'company-jedi)
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

  :diminish git-gutter-mode

  :custom-face
  (git-gutter:modified ((t (:foreground "blue" :background "black" :weight bold))))
  (git-gutter:added ((t (:foreground "green" :background "black" :weight bold))))
  (git-gutter:deleted ((t (:foreground "red" :background "black" :weight bold))))

  :config
  (setq git-gutter:update-interval 1)
  (setq git-gutter:modified-sign "~")
  (setq git-gutter:added-sign "+")
  (setq git-gutter:deleted-sign "-")
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
;; (use-package projectile
;;   :config
;;   (projectile-mode 1)
;;   )

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

;; TODO
;; tramp-mode, editing remote files
;; helm https://github.com/emacs-helm/helm
;; neotree https://github.com/jaypei/emacs-neotree
;; projectile https://github.com/bbatsov/projectile

;; Tips

;; Emacs is slow to quit

;; C-h v emacs-kill-hook
;; ido is a good candidate