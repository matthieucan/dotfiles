; packages: python-rope, python-ropemacs, pymacs

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((TeX-master . "main")))))

(cond
 (window-system
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 123 :width normal :foundry "unknown" :family "monofur"))))
   '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "#9F9FFF"))))
   '(font-lock-constant-face ((((class color) (min-colors 88) (background dark)) (:foreground "#7F3F00"))))
   '(font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#afff7f" :slant oblique))))
   '(font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "#00afaf" :weight bold))))
   '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen3"))))
   '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#ff9696"))))
   '(font-lock-warning-face ((((class color) (min-colors 88) (background dark)) (:foreground "orange" :weight bold))))
   '(my-lisp-fold-face ((default (:foreground "#7F7FFF" :slant italic)) (nil nil))))
  )
 )

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
(show-paren-mode 1)

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

(add-hook 'python-mode-hook
          (lambda ()
            (pymacs-load "ropemacs" "rope-")))

(add-hook 'python-mode-hook 'flymake-mode)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")
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
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

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
