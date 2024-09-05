;;; mygtd -- a getting things done implementation

;; Copyright (C) 2023   Tristan de Cacqueray

;; Package-Requires: ((org-ql))

;;; Commentary:
;;;    Use "C-h v" to display variable documentations (or "C-c C-d" for symbol at point with helpful)
;;; Code:

(require 'org-ql)

;; Unfortunately org-mode does not support timestamp with timezone, so this ensure correct format on export.
(setq org-icalendar-timezone "America/New_York")

;; Files
(setq org-report-files '("~/org/projects.org.gpg" "~/org/jira.org"))
(setq tc/inbox "~/org/inbox.org.gpg")
(setq tc/journal "~/org/journal.org.gpg")

;; Look for agenda item in these files
(setq org-agenda-files '("~/org/projects.org.gpg" "~/org/home.org.gpg" "~/org/inbox.org.gpg"))
;; (debug-watch 'org-agenda-files)
(use-package org
  :config
  (setq-default
   ;; Tell org where are the files
   org-directory "~/org/"

   ;; Display image inline
   org-startup-with-inline-images t

   ;; Insead of "..." show "⤵" when there's hidden folded content
   ;; Some characters to choose from: …, ⤵, ▼, ↴, ⬎, ⤷, and ⋱
   org-ellipsis "⤵"

   ;; Show headings up to level 2 by default when opening an org files
   org-startup-folded 'content

   ;; Simple TODO sequence, DONE and CANCELLED are terminal
   org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                             (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
   org-todo-keyword-faces (quote (("TODO" :foreground "dark orange")
                                  ("NEXT" :foreground "blue" :weight bold)
                                  ("DONE" :foreground "forest green" :weight bold)
                                  ("WAITING" :foreground "orange" :weight bold)
                                  ("HOLD" :foreground "magenta" :weight bold)
                                  ("CANCELLED" :foreground "forest green" :weight bold)))

   ;; disable org-todo pop-up on C-c C-t
   org-use-fast-todo-selection 'expert

   ;; But don't bother with notes when using shift arrows
   org-treat-S-cursor-todo-selection-as-state-change nil

   ;; Only show one star, though this is overridden by org-bullets
   ;; org-hide-leading-stars t

   ;; Mail link description format, %c if from or to when sent by me
   org-email-link-description-format "Email %c (%d): %s"
   )

  ;; Ensure shift arrows don't execute org commands, e.g. change todo state.
  (setq-default org-support-shift-select nil)
  ;; should make windmove key work in org-mode
  (setq-default org-replace-disputed-keys t)

  ;; Enable windmove-default-keybindings in org-mode
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)


  ;; Custom links
  ;; http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
  (defvar yt-iframe-format
    ;; You may want to change your width and height.
    (concat "<iframe width=\"560\""
            " height=\"315\""
            " src=\"https://www.youtube.com/embed/%s\""
            " frameborder=\"0\""
            " allow=\"accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture\""
            " allowfullscreen></iframe>"))
  (org-add-link-type
   "yt"
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/"
              handle)))
   (lambda (path desc backend)
     (cl-case backend
       (md (format yt-iframe-format   path (or desc "")))
       (html (format yt-iframe-format path (or desc "")))
       (latex (format "\href{%s}{%s}" path (or desc "video"))))))
  )


(use-package org-capture
  :bind ("C-c c" . org-capture)
  :demand
  :config
  (setq
   ;; Use any org-agendas file as refile target, only first level
   org-refile-targets '((org-agenda-files :maxlevel . 1))
   ;; Use full outline paths for refile targets
   org-refile-use-outline-path t
   ;; Targets complete directly
   org-outline-path-complete-in-steps nil
   )
  (setq org-refile-use-cache nil)
  (setq org-capture-templates
        '(
          ("t" "todo" entry (file tc/inbox)
           "* TODO %?\n/Entered on/ %U\n")
          ("d" "done" entry (file tc/inbox)
           "* DONE %?\nCLOSED: %U\n")
          ("j" "Journal" entry (file+olp+datetree tc/journal)
           "* %?\n")
          ))

  (define-key global-map (kbd "<f5>") (lambda () (interactive) (org-capture nil "t")))

  (defun tc/org-capture-done ()
    (interactive)
    (org-capture nil "d"))

  (define-key global-map (kbd "<f8>") 'tc/org-capture-done)

  (defun tc/org-capture-journal ()
    "Capture a journal item"
    (interactive)
    (org-capture nil "j"))
  (define-key global-map (kbd "C-c j") 'tc/org-capture-journal)
  )

;; example agenda config
(defun tc/org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "a"))

(use-package org-agenda
  :bind ("<f12>"   . tc/org-agenda-show-agenda-and-todo)
  :config
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (setq
   ;; Start agenda at today
   org-agenda-start-on-weekday nil
   ;; Do not dim blocked tasks
   org-agenda-dim-blocked-tasks nil
   ;; Compact the block agenda view
   org-agenda-compact-blocks t
   ))

;; gtd settings from [[file:/srv/github.com/rougier/emacs-GTD/GTD.org]]
(setq-default org-agenda-hide-tags-regexp ".")
(setq-default org-agenda-prefix-format
              '((agenda . " %i %-12:c%?-12t% s")
                (todo   . " ")
                (tags   . " %i %-12:c")
                (search . " %i %-12:c")))

(setq org-habit-following-days 7
      org-habit-preceding-days 35
      org-habit-show-habits t)

;; Toggle to show all habits ;; todo: make that into a view
(setq org-habit-show-all-today nil)

(setq-default org-agenda-custom-commands
              '(("g" "Get Things Done (GTD)"
                 ((agenda ""
                          ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                           (org-deadline-warning-days 0)))
                  (todo "NEXT"
                        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                         (org-agenda-prefix-format "  %i %-12:c [%e] ")
                         (org-agenda-overriding-header "\nTasks\n")))
                  (tags-todo "inbox"
                             ((org-agenda-prefix-format "  %?-12t% s")
                              (org-agenda-overriding-header "\nInbox\n")))
                  (tags "CLOSED>=\"<today>\""
                        ((org-agenda-overriding-header "\nCompleted today\n")))))))

;; f4 shows the gtd view
(define-key global-map (kbd "<f4>") (lambda () (interactive) (org-agenda nil "g")))

(defun tc/get-buffer-content ()
  "return buffer or region content."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties 1 (buffer-size))
    )
  )

(add-to-list 'load-path "/srv/github.com/TristanCacqueray/emacs-toolbox")
;; Update schedule events when the agenda is displayed
(require 'org-next-event)
(add-hook 'org-agenda-mode-hook 'org-next-event-render)

(require 'org-report)
;; From anywhere, f6 shows the daily report
(define-key global-map (kbd "<f6>") 'org-report-daily-show)

;; In the daily report calendar view, f6 format the report
(define-key org-agenda-mode-map (kbd "<f6>") 'org-report-daily)

;; Make sure the org is saved
(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

(provide 'mygtd)
