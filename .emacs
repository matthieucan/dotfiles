(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(save-place t nil (saveplace))
 '(sh-assignment-regexp (quote ((csh . "\\<\\([a-zA-Z0-9_]+\\)\\(\\[.+\\]\\)?[ 	]*[-+*/%^]?=") (ksh88 . "\\<\\([a-zA-Z0-9_]+\\)\\(\\[.+\\]\\)?[ 	]*\\([-+*/%&|~^]\\|<<\\|>>\\)?=") (rc . "\\<\\([a-zA-Z0-9_*]+\\)[ 	]*=") (sh . "\\<\\([a-zA-Z0-9_]+\\)=") (bash . "\\<\\([a-zA-Z0-9_]+\\)\\(\\[.*\\]\\)?[ 	]*[-+*/%^]?=(?"))))
 '(sh-basic-offset 2)
 '(sh-indent-after-switch (quote +))
 '(sh-indent-for-case-alt (quote +))
 '(sh-indent-for-case-label 0)
 '(sh-indentation 2)
 '(standard-indent 2)
 '(vc-handled-backends (quote (RCS CVS SCCS Bzr Git Hg Mtn Arch))))
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

(setq display-time-day-and-date t)
(display-time)
(display-battery-mode t)

(fset 'λ 'lambda)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(set-frame-parameter nil 'background-mode "dark")

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/plugins")
;;(load "smooth-scrolling")
;;(load "mine/C++")
;;(load "mine/Lisp")

(setq visible-bell t)
(setq enable-local-eval t)

(defmacro dowhile (pred &rest body)
  (declare (indent 1))
  `(while (progn ,@body ,pred)))

(defun string-begin (str beg)
  (and 
   (>= (length str) (length beg))
   (string-equal beg (substring str 0 (length beg)))))

(defun my-split (list pred)
  (let ((ret (cons nil nil)))
    (while (consp list)
      (if (funcall pred (car list))
          (setcar ret (cons (car list) (car ret)))
        (setcdr ret (cons (car list) (cdr ret))))
      (setq list (cdr list)))
    ret))

(defun file-buffer-p (buf)
  (when (buffer-file-name buf) t))
(defun my-c++-file-p (buf)
  (eq (buffer-local-value 'major-mode buf) 'c++-mode))

(defun my-get-files (dir &optional recursive regex)
  (let (ret)
    (if (file-directory-p dir)
	(dolist (file (directory-files dir))
	  (cond
	   ((and (file-regular-p (expand-file-name file dir))
		 (string-match regex file) )
	    (push (expand-file-name file dir) ret) )
	   
	   ((and recursive
		 (not (or (string= file ".") (string= file "..")))
                 (file-directory-p (expand-file-name file dir)) )
	    (setq ret (nconc ret 
                             (my-get-files (expand-file-name file dir) t regex) )) ) ) ) )
    ret) )

(defun find-files (dir regex &optional recursive)
  (interactive "DFind files in : \nsMatching regex : \nP")
  (dolist (file (my-get-files dir (consp recursive) regex))
    (find-file-noselect file) ) )

(defun my-perform-replace-in-buffers (buffers from-string replacements query-flag regexp-flag delimited-flag 
                                              &optional repeat-count map start end)
  (let ((c-win (selected-window))
        (c-buf (current-buffer))) 
    (condition-case nil
        (save-current-buffer 
          (dolist (buf buffers) 
            (set-window-buffer c-win buf)
            (set-buffer buf)
            (save-excursion
              (goto-char (point-min))
              (perform-replace from-string replacements query-flag regexp-flag
                               delimited-flag repeat-count map start end))))
      (quit nil))
    (set-window-buffer c-win c-buf)
    (set-buffer c-buf)))

(fset 'my-query-replace-in-buffers 
      `(lambda (from-string to-string &optional delimited start end)
         ,(interactive-form 'query-replace)
         (my-perform-replace-in-buffers (buffer-list) from-string to-string t nil delimited nil nil start end)))
      
(global-set-key (kbd "C-c M-%") 'my-query-replace-in-buffers)

(fset 'my-query-replace-regexp-in-buffers 
      `(lambda (from-string to-string &optional delimited start end)
         ,(interactive-form 'query-replace-regexp)
         (my-perform-replace-in-buffers (buffer-list) from-string to-string t t delimited nil nil start end)))
      
(global-set-key (kbd "C-c C-M-%") 'my-query-replace-regexp-in-buffers)

(defun my-toggle-fullscreen ()
  (interactive)
  (modify-frame-parameters 
   nil 
   (if (not (frame-parameter nil 'fullscreen))
       '((fullscreen . fullboth))
     '((fullscreen)) ) ) )

(defun my-insert-newline ()
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-according-to-mode))

; keymap settings
(define-key lisp-interaction-mode-map [tab] 'my-lisp-indent-or-complete)
(define-key emacs-lisp-mode-map       [tab] 'my-lisp-indent-or-complete)
(define-key lisp-mode-map             [tab] 'my-lisp-indent-or-complete)

(global-set-key (kbd "C-<return>") 'my-insert-newline)
(global-set-key (kbd "C-c c")      'compile)
(global-set-key (kbd "C-c l")      'my-load-project-dir)
(global-set-key (kbd "C-x M-f")    'find-files)
(global-set-key (kbd "C-x C-b")
                (λ (&optional arg) 
                  (interactive "P") 
                  (set-window-buffer nil (get-buffer-create "*Buffer List*")) 
                  (list-buffers (not arg))
                  (beginning-of-buffer)))
(global-set-key [S-f9]             'my-toggle-fullscreen)

(push '("." . "/tmp") backup-directory-alist)

(global-auto-revert-mode)
(defun my-auto-revert-modes (&rest modes)
  (dolist (elem (car (my-split auto-mode-alist 
                              (λ (assoc)
                                (memq (cdr assoc) modes))))) 
    (add-to-list 'revert-without-query (car elem))))
(my-auto-revert-modes 'c++-mode 'c-mode 'lisp-mode)

(push '("\\.lc[pbi]\\'" . emacs-lisp-mode) auto-mode-alist)

(global-set-key (kbd "C-\\") (λ () (interactive) (insert "λ"))) 
(setq scheme-program-name "mzscheme")

(defface my-scheme-paren-face '((default (:foreground "#0000ff"))) "foo")
(font-lock-add-keywords 'scheme-mode
   '(("\\((\\|)\\)" (1 my-scheme-paren-face prepend))) t)
(font-lock-add-keywords 'scheme-mode
   '(("(\\(λ\\|require\\|define-for-syntax\\)\\_>" (1 font-lock-keyword-face t))))

;;(menu-bar-mode -1)
(tool-bar-mode -1)
(setq truncate-partial-width-windows nil)
(display-time-mode t)
(setq c-default-style "bsd")
(setq-default c-basic-offset 4)
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
; add a ";" in front of each line 
(setq-default inferior-S+6-program-name "Splus")
(setq-default inferior-R-program-name "R")

; coffee mode
(load-file "~/.emacs.d/coffee-mode.el")


(load-file "~/.emacs.d/markdown-mode.el")
(autoload 'markdown-mode "markdown-mode.el" 
	"Major mode for editing Markdown files" t) 
	(setq auto-mode-alist 
		(cons '("\\.md" . markdown-mode) auto-mode-alist)
	)
