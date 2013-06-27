(custom-set-variables
 '(column-number-mode t)
 '(inhibit-startup-screen t)
)
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 123 :width normal :foundry "unknown" :family "monofur"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "#9F9FFF"))))
 '(font-lock-constant-face ((((class color) (min-colors 88) (background dark)) (:foreground "#7F3F00"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#afff7f" :slant oblique))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "#00afaf" :weight bold))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen3"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#ff9696"))))
 '(font-lock-warning-face ((((class color) (min-colors 88) (background dark)) (:foreground "orange" :weight bold))))
 '(my-lisp-fold-face ((default (:foreground "#7F7FFF" :slant italic)) (nil nil))))

(fset 'λ 'lambda)

(global-set-key (kbd "C-x C-b")
                (λ (&optional arg) 
                  (interactive "P") 
                  (set-window-buffer nil (get-buffer-create "*Buffer List*")) 
                  (list-buffers (not arg))
                  (beginning-of-buffer)))

(set-face-font 'default '"-*-monospace-medium-r-normal-*-13-120-*-iso8859-15")

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
