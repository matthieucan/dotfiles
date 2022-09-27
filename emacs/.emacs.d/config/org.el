;;; org-mode --- configuration

;;; Commentary:

;;; Code:

(require 'org)

(require 'ox-latex)

; org modules
(load-file "~/.emacs.d/org-clock-csv.el")
(add-to-list 'org-modules 'org-habit t)

; color code blocks in org-mode
(setq org-src-fontify-natively t)

; org-mode bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; https://lists.gnu.org/archive/html/emacs-orgmode/2018-04/msg00455.html
(defun my-skip-entries-below-scheduled-entry ()
  "Exclude sub-items from todo list in agenda if their parent is scheduled."
  (when (save-excursion
          (org-up-heading-safe)
          (org-get-scheduled-time (point)))
    (org-end-of-subtree t t)))

(defun my-skip-entries ()
  "Skip todo entries."
  (or (org-agenda-skip-entry-if 'scheduled 'deadline) (my-skip-entries-below-scheduled-entry)))

; hide some tags in agenda views
(setq org-agenda-hide-tags-regexp "picnic\\|ongoing\\|anothertag")

; org-mode my agenda view
(setq org-agenda-custom-commands
      '(
        ("h" "My agenda view"
         (
          (agenda "")
          (tags-todo "-goodidea"
                     (
                      (org-agenda-skip-function '(my-skip-entries))
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-list-sublevels nil)
                      (org-agenda-overriding-header "General TODO list")
                      (org-agenda-sorting-strategy '(priority-down todo-state-down))))
          (tags "goodidea" ((org-agenda-overriding-header "Ideas worth thinking about")))
          )
         ((org-agenda-tag-filter-preset '("-picnic")))
         )
        ("p" "Picnic"
         (
          (agenda ""
                  (
                   (org-agenda-files '("~/org/picnic.org"))
                   ;; (org-agenda-sorting-strategy '(priority-down todo-state-down))
                   )
                  )
          (tags "ongoing" ((org-agenda-overriding-header "Ongoing projects")))
          (tags "task" ((org-agenda-overriding-header "Clockable tasks")))
          (todo "DELG"
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

; org-agenda time grid settings
(setq org-agenda-time-grid
      (quote
       ((today require-timed remove-match)
        (800 1000 1200 1400 1600 1800 2000)
        "....."
        "----------------")))

; org-mode priorities
(setq org-priority-faces '((?A . (:foreground "darkred" :background "white" :weight bold))
                           (?B . (:foreground "yellow"))
                           (?C . (:foreground "green"))))

; org-mode todo items
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

; org-mode agenda, when jumping to entry, narrow buffer
(advice-add 'org-agenda-goto :after
            (lambda (&rest args)
              (org-narrow-to-subtree)))

;org-mode agenda files
;; (load-library "find-lisp")
;; (setq org-agenda-files
;;       (append (find-lisp-find-files "~/org" "\.org$")))
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
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

; reload org files automatically
(add-hook 'org-mode-hook 'auto-revert-mode)

; french holidays
;(require 'french-holidays)
;(setq calendar-holidays holiday-french-holidays)

; geographical location, for sunset & sunrise times
(setq calendar-latitude 52.4)
(setq calendar-longitude 4.9)

; agenda: don't display DONE items
(setq org-agenda-skip-scheduled-if-done 1)
(setq org-agenda-skip-deadline-if-done 1)
(setq org-agenda-skip-timestamp-if-done 1)

; agenda: right-align tags on column 95
(setq org-agenda-tags-column -95)

; agenda: tag placement
;; Place tags close to the right-hand side of the window
(add-hook 'org-agenda-mode-hook '(lambda ()
                                  (setq org-agenda-tags-column (- (window-width) 24))))
;(setq org-tags-column -120)


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
   ;; (sh . t)
   ;; (shell . t)
   ;; (scala . t)
   ))
;; add additional languages with '((language . t)))

;; use python3
(setq org-babel-python-command "python3")


(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "python")))  ; don't ask for python
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

; mobile-org

(setq org-mobile-directory "~/ownCloud/MobileOrg/")

; org plot
(global-set-key "\M-\C-g" 'org-plot/gnuplot)

; org capture
(setq org-default-notes-file "~/org/log.org")
(define-key global-map "\C-cp" 'org-capture)
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  Added on %U\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/log.org")
         "* %?\n  Entered on %U\n%i\n  %a")
        ("J" "Journal (displayed in agenda)" entry (file+datetree "~/org/log.org")
         "* %?\n  Entered on %T\n%i\n  %a")
        ))

; log time when task is done
(setq org-log-done (quote time))
(setq org-log-into-drawer t)

; Set org duration format. Only show hours:minutes in clock tables
(setq org-duration-format 'h:mm)

; org-jira
(setq org-jira-working-dir "~/org/jira")
(setq jiralib-url "https://picnic.atlassian.net")
(setq org-jira-jira-status-to-org-keyword-alist
      '(("In Progress" . "CURR")
        ("In Review" . "REVW")
        ("To Do" . "TODO")
        ("Blocked" . "BLOC")
        ("Open" . "OPEN")
        ("Draft" . "OPEN")
        ("Backlog" . "OPEN")
        ("To Deploy" . "DONE")))

; set custom JQL
(setq org-jira-custom-jqls
  '(
    (:jql " project = 'DATA' AND assignee = currentUser() AND status IN ('Blocked','In Progress','In Review','Open','To Do','To Deploy') ORDER BY created DESC "
          :limit 100
          :filename "DATA")
    (:jql " project = 'PY' AND assignee = currentUser() AND status IN ('Blocked','Draft','In Progress','In Review','Open','To Do') ORDER BY created DESC "
          :limit 100
          :filename "PY")
    (:jql " project = 'MUR' AND assignee = currentUser() AND status IN ('Backlog','Blocked','In Progress','In Review','To Do') ORDER BY created DESC "
          :limit 100
          :filename "MUR")
   ))

; do not create deadline entries, I don't use them and it makes issues disappear from agenda
(setq org-jira-deadline-duedate-sync-p nil)

; bind sortcut to refresh JIRA issues
(global-set-key "\C-cj" 'org-jira-get-issues-from-custom-jql)

; org-mode: move narrowed view 1 level up
(defun my/org-narrow-parent ()
  "Select parent of current subtree and narrow to it."
  (interactive)
  (widen)
 (outline-up-heading 1 t)
 (org-reveal)
 (org-narrow-to-subtree)
 (outline-hide-subtree) ; hide and cycle renders better wrt empty lines between subtrees
 (org-cycle))
(global-set-key "\C-cu" 'my/org-narrow-parent)

;;; org.el ends here
