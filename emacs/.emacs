; packages: python-rope, python-ropemacs, pymacs, solidity-mode

(add-to-list 'load-path "~/.emacs.d/elpa/org-20170821/")
(require 'org)
(require 'ox-latex)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

; hide useless warnings
;(setq warning-minimum-level :error)

;; colors

(load-file "~/.emacs.d/rebecca-theme.el")
(load-theme 'rebecca t t)
(enable-theme 'rebecca)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((TeX-master . "main")))))

;; (cond
;;  (window-system
;;   (custom-set-faces
;;    ;; custom-set-faces was added by Custom.
;;    ;; If you edit it by hand, you could mess it up, so be careful.
;;    ;; Your init file should contain only one such instance.
;;    ;; If there is more than one, they won't work right.
;;    '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 123 :width normal :foundry "unknown" :family "monofur"))))
;;    '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "#9F9FFF"))))
;;    '(font-lock-constant-face ((((class color) (min-colors 88) (background dark)) (:foreground "#7F3F00"))))
;;    '(font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#afff7f" :slant oblique))))
;;    '(font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "#00afaf" :weight bold))))
;;    '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen3"))))
;;    '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#ff9696"))))
;;    '(font-lock-warning-face ((((class color) (min-colors 88) (background dark)) (:foreground "orange" :weight bold))))
;;    '(my-lisp-fold-face ((default (:foreground "#7F7FFF" :slant italic)) (nil nil))))
;;   )
;;  )

;; region (highlighted selection)
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

;; highlighted isearch
(set-face-attribute 'isearch nil :background "#00f" :foreground "#000")
(set-face-attribute 'lazy-highlight nil :background "#fff" :foreground "#000")

(fset 'λ 'lambda)

(global-set-key (kbd "C-x C-b")
                (λ (&optional arg) 
                  (interactive "P") 
                  (set-window-buffer nil (get-buffer-create "*Buffer List*")) 
                  (list-buffers (not arg))
                  (beginning-of-buffer)))

(cond
 (window-system
  (set-face-font 'default '"-*-monospace-medium-r-normal-*-13-120-*-iso8859-15")
  )
 )

;;disable tool bar
(tool-bar-mode -1)

;;parenthèses correspondantes :
(setq show-paren-mode t)
; surlignage du "matching" de parenthèses
(setq show-paren-delay 0)
(show-paren-mode 1)
(require 'paren)
(set-face-background 'show-paren-match "yellow")
(set-face-foreground 'show-paren-match "black")
(set-face-attribute 'show-paren-match nil :weight 'bold)

;;surlignage TODO FIXME BUG
(add-hook 'c-mode-common-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t)))))

;;mode retour à la ligne automatique
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; automatically get the correct mode for R and S
auto-mode-alist (append (list '("\\.c$" . c-mode)
			      '("\\.tex$" . latex-mode)
			      '("\\.S$" . S-mode)
			      '("\\.s$" . S-mode)
			      '("\\.R$" . R-mode)
			      '("\\.r$" . R-mode)
			      '("\\.html$" . html-mode)
                              '("\\.emacs" . emacs-lisp-mode)
	                )
		      auto-mode-alist)
; comment out the following if you are not using R/S-Plus on ACPUB
(setq-default inferior-S+6-program-name "Splus")
(setq-default inferior-R-program-name "R")

; coffee mode
(load-file "~/.emacs.d/coffee-mode.el")

; markdown mode
(load-file "~/.emacs.d/markdown-mode.el")
(autoload 'markdown-mode "markdown-mode.el" 
	"Major mode for editing Markdown files" t) 
	(setq auto-mode-alist 
		(cons '("\\.md" . markdown-mode) auto-mode-alist)
	)

; won't automagickally add newline at the end of saved files
;(setq-default require-final-newline nil)

;; don't let next-line add new lines at end of file
(setq next-line-add-newlines nil)

;; make edited files end with a carriage return
(setq require-final-newline nil)
(setq mode-require-final-newline nil)

; Python-mode
(setq py-install-directory "~/.emacs.d/python-mode.el-6.1.3")
(add-to-list 'load-path "~/.emacs.d/python-mode.el-6.1.3")
(require 'python-mode)

;(add-hook 'python-mode-hook
;          (lambda ()
;            (pymacs-load "ropemacs" "rope-")))

;(add-hook 'python-mode-hook 'flymake-mode)
;(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;(setq flymake-python-pyflakes-executable "flake8")
;(require 'flymake-cursor)

(setq-default indent-tabs-mode nil)

; disable scroll bar
(scroll-bar-mode -1)

; disable menu bar
(menu-bar-mode -1)

; consider _ as a punctuation character even in Python mode
(modify-syntax-entry ?_ "_" python-mode-syntax-table)

; web mode for Django templates & family
(load-file "~/.emacs.d/web-mode.el")
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(load-file "~/.emacs.d/dockerfile-mode.el")
(autoload 'dockerfile-mode "dockerfile-mode.el" 
  "Major mode for editing Dockerfiles" t) 
(setq auto-mode-alist 
      (cons '("Dockerfile\\'" . dockerfile-mode) auto-mode-alist)
      )

; scrollers
;(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   1)) )
;(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 1)) )
(global-set-key "\M-n" "\C-u1\C-v\C-n")
(global-set-key "\M-p" "\C-u1\M-v\C-p")

; makes emacs flash instead of beep
(setq visible-bell 1)

; go
(add-hook 'go-mode-hook 
  (lambda ()
    (setq-default) 
    (setq tab-width 2) 
    (setq standard-indent 2) 
    (setq indent-tabs-mode nil)))

; javascript
(setq js-indent-level 2)

; scala
;(require 'ensime)
;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))

; mail mode with mutt
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

; mail colors
(add-hook 'mail-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("^[ \t]*>[ \t]*>[ \t]*>.*$"
         (0 'font-lock-comment-face))
        ("^[ \t]*>[ \t]*>.*$"
         (0 'font-lock-constant-face))))))

; color code blocks in org-mode
(setq org-src-fontify-natively t)

; org-mode bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; org-mode my agenda view
(setq org-agenda-custom-commands
      '(("h" "My agenda view"
         (
          (agenda "")
          (tags-todo "-future" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline)) (org-agenda-todo-list-sublevels nil)))
          (tags "goodidea")
          (tags "future")
          ))))

; org-mode todo items
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "|" "CANCELED" "DONE")))

(setq org-todo-keyword-faces
      '(
       ("TODO" . (:background "darkred" :foreground "white" :weight bold))
       ("STARTED" . (:foreground "black" :background "yellow" :weight bold))
       ("CANCELED" . (:foreground "black" :background "blue" :weight normal))
       ("DONE" . (:foreground "darkgreen" :weight bold))))

;org-mode agenda files
(load-library "find-lisp")
(setq org-agenda-files
      (append (find-lisp-find-files "~/org" "\.org$")
            (find-lisp-find-files "~/git/phd-thesis/" "\.org$")))
;; (setq org-agenda-files (append
;;       '("~/org/"
;;         "~/git/phd-thesis/")))

; org-mode follow hyperlinks with Enter
(setq org-return-follows-link 1)

; org-mode open file links in same frame
(setq org-link-frame-setup (quote ((file . find-file))))

; org-mode hide todo and tags subitems in global list
(setq org-agenda-todo-list-sublevels nil)
(setq org-tags-match-list-sublevels nil)

; org-mode set deadline reminder to 3 days before
(setq org-deadline-warning-days 3)

; french holidays
;(require 'french-holidays)
;(setq calendar-holidays holiday-french-holidays)

; reload org files automatically
(add-hook 'org-mode-hook 'auto-revert-mode)

; agenda: don't display DONE items
(setq org-agenda-skip-scheduled-if-done 1)
(setq org-agenda-skip-deadline-if-done 1)

; todo list: don't display scheduled/deadlined/timestamped items
(setq org-agenda-todo-ignore-with-date 1)

;; commented out because it makes emacs slow as hell to start up, about 1/3 of the time
;; FIXME
;; active Babel languages
(require 'ob-python)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (python . t)
   (sh . t)
   (shell . t)
   (scala . t)))
;; add additional languages with '((language . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "python")))  ; don't ask for python
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

; mobile-org

(setq org-mobile-directory "~/ownCloud/MobileOrg/")

; org plot
(global-set-key "\M-\C-g" 'org-plot/gnuplot)

; hightlight current line
;; (load-file "~/.emacs.d/hl-spotlight.el")
;; (setq hl-spotlight-height 0)
;; (set-face-attribute 'hl-spotlight nil :background "#ccc" :foreground "#000")
;; (global-hl-spotlight-mode)

; line numbers
(setq linum-format "%3d\u2502")
(global-linum-mode)
; not for certain modes
(setq linum-disabled-modes-list '(org-mode org-agenda-mode help-mode mail-mode lisp-interaction-mode completion-list-mode buffer-menu-mode eww-mode))
(defun linum-on () (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)) (linum-mode 1)))
; color
(set-face-foreground 'linum "white")

; highlight current line number
(load-file "~/.emacs.d/hlinum.el")
(require 'hlinum)
(hlinum-activate)

; auto complete
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "/usr/share/auto-complete/dict/")
(require 'auto-complete-config)
(ac-config-default)

; seek & destroy

(global-set-key "\M-j" 'replace-regexp)
(global-set-key "\M-k" 'query-replace)

; column marker
;; (load-file "~/.emacs.d/fill-column-indicator.el")
;; (require 'fill-column-indicator)
;; (add-hook 'after-change-major-mode-hook 'fci-mode)

(load-file "~/.emacs.d/python-docstring/python-docstring.el")
(require 'python-docstring)
(add-hook 'python-mode-hook 'python-docstring-mode)

; gnuplot
(setq auto-mode-alist
      (append '(("\\.\\(gp\\|plt\\|gnuplot\\)$" . gnuplot-mode)) auto-mode-alist))

; don't add extra spaces when pasting from outside
(electric-indent-mode 0)

; line duplication, with or without commenting the first one
(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the original" 
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))
;; duplicate a line
(global-set-key (kbd "C-c y") 'djcb-duplicate-line)
;; duplicate a line and comment the first
(global-set-key (kbd "C-c c") (lambda()(interactive)(djcb-duplicate-line t)))

; remember when I were in files
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

; flycheck, lint error highlighting
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-python-pylint-executable "pylint3")
(setq flycheck-display-errors-delay 0)

; rjsx-mode installed from melpa
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(eval-after-load 'flycheck
  '(flycheck-add-mode 'javascript-eslint 'rjsx-mode))
(setq js2-strict-trailing-comma-warning nil)
(setq js2-basic-offset 2)

; use latex mode for .tikz files
(add-to-list 'auto-mode-alist '("\\.tikz" . latex-mode))

;;
