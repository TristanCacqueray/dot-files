;;; mygtd -- a getting things done implementation

;; Copyright (C) 2023   Tristan de Cacqueray

;; Package-Requires: ((org-ql))

;;; Commentary:
;;;    Use "C-h v" to display variable documentations (or "C-c C-d" for symbol at point with helpful)
;;; Code:

(require 'org-ql)

(setq-default org-default-notes-file "~/org/gtd.org.gpg")

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

   ;; Auto tag task on change
   org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" (). t))
                                        ("WAITING" ("WAITING" . t))
                                        ("HOLD" ("WAITING") ("HOLD" . t))
                                        (done ("WAITING") ("HOLD"))
                                        ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                        ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                        ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

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
            " allowfullscreen></iframe>")


    )
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
  (setq org-capture-templates
        '(
          ("t" "todo" entry (file+headline "~/org/gtd.org.gpg" "Inbox")
           "* TODO %? %a\n/Entered on/ %U\n")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org.gpg")
           "* %?\n")
          ))

  (defun tc/org-capture-todo ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "t"))

  (define-key global-map (kbd "<f5>") 'tc/org-capture-todo)

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
   ;; Look for agenda item in every org files
   org-agenda-files '("~/org")
   ;; Match encrypted files too
   org-agenda-file-regexp "\\`[^.].*\\.org\\(.gpg\\)?\\'"
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

;; Review agenda from: https://stackoverflow.com/a/22440571
;; Common settings for all reviews
(setq tc/org-agenda-review-settings
      '((org-agenda-files '("~/org/gtd.org.gpg"
                            ))
        (org-agenda-show-all-dates t)
        (org-agenda-start-with-log-mode t)
        (org-agenda-start-with-clockreport-mode t)
        (org-agenda-archives-mode t)
        ;; I don't care if an entry was archived
        (org-agenda-hide-tags-regexp
         (concat org-agenda-hide-tags-regexp
                 "\\|ARCHIVE"))
        ))
(add-to-list 'org-agenda-custom-commands
             `("w" "Week in review"
               agenda ""
               ;; agenda settings
               ,(append
                 tc/org-agenda-review-settings
                 '((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 0)
                   (org-agenda-overriding-header "Week in Review"))
                 )
               ("~/org/review/week.html")
               ))
(add-to-list 'org-agenda-custom-commands
             `("d" "Day in review"
               agenda ""
               ;; agenda settings
               ,(append
                 tc/org-agenda-review-settings
                 '((org-agenda-span 'day)
                   (org-agenda-overriding-header "Day in Review"))
                 )
               ("~/org/review/day.html")
               ))
(add-to-list 'org-agenda-custom-commands
             `("m" "Month in review"
               agenda ""
               ;; agenda settings
               ,(append
                 tc/org-agenda-review-settings
                 '((org-agenda-span 'month)
                   (org-agenda-start-day "01")
                   (org-read-date-prefer-future nil)
                   (org-agenda-overriding-header "Month in Review"))
                 )
               ("~/org/review/month.html")
               ))


(defun tc/get-buffer-content ()
  "return buffer or region content."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties 1 (buffer-size))
    )
  )

(defun s-unlines (xs)
  "The inverse of 's-lines'."
  (s-join "\n" xs))

;;; remove elem when f return true
(defun tc/drop (f xs)
  "The inverse of 'seq-filter'."
  (seq-filter (lambda (s) (not (funcall f s))) xs))

(progn
  "First attempt at creating daily report by processing the agenda view buffer"
  (defun tc/get-done (line)
    "Match closed line and return a formated string"
    (if-let ((show (lambda (cat ev) (s-join " " (list "**" (s-pad-left 13 " " cat) "-" (tc/remove-links ev)))))
             (match (s-match-strings-all "^ *\\([^:]+\\): .* Closed: .* DONE \\(.*\\)$" line))
             )
        (apply show (cdr (car match)))
      )
    )
  (defun tc/is-org-date (line)  (s-match "[A-Z][a-z]+ *[0-9]+ [A-Z][a-z]+ [0-9]+.*" line))

  (defun tc/process-agenda (line) (if (tc/is-org-date line) line (tc/get-done line)))

  (defun tc/daily-report ()
    "Produce a report for team daily"
    (let* (
           (lines (s-lines (tc/get-buffer-content)))
           (events (tc/drop 's-blank? (mapcar 'tc/process-agenda lines)))
           )
      (message (s-unlines events))
      )
    )
  )

(defun tc/remove-links (s)
  (s-trim (s-replace-regexp "\\[\\[.*\\]\\]" "" s)))

(defun tc/daily-format-item (item)
  "Format 'org-ql-select' output. ITEM is a prop list."
  (let* ((properties (cadr item))
         (title (plist-get properties :raw-value))
         (category (org-entry-get (plist-get properties :org-marker) "CATEGORY"))
         )
    (format "* %s - %s" (s-pad-left 13 " " category) (tc/remove-links title))
    )
  )
(defun tc/daily-format (items)
  "Format all ITEMS."
  (let ((today (format-time-string "%Y-%m-%d")))
    (s-unlines (cons today (cons "" (cons "tdecacqu:" (mapcar 'tc/daily-format-item items)))))
    )
  )

(defun tc/mk-daily-query ()
  ;; TODO: make that one day during the week
  '(and (done) (ts :from -3 :to today))
)

(defun tc/show-daily-report ()
  "Show daily report."
  (interactive)
  (org-ql-search "~/org/gtd.org.gpg" (tc/mk-daily-query)
    :super-groups '((:auto-ts t))))

;; From anywhere, f6 shows the daily report
(define-key global-map (kbd "<f6>") 'tc/daily-report)

(defun tc/mk-daily-report ()
  "Produce a report for team daily."
  (interactive)
  (let* ((entries (org-ql-select "~/org/gtd.org.gpg"
                    (tc/mk-daily-query)
                    :action 'element-with-markers
                    ))
         (report (tc/daily-format entries))
         (*buffer* (get-buffer-create "*tc/daily*")))
    (with-current-buffer *buffer*
      (erase-buffer)
      (insert report)
      )
    (switch-to-buffer-other-window *buffer*)
    )
  )

;; In the daily report calendar view, f6 format the report
(define-key org-agenda-mode-map (kbd "<f6>") 'tc/mk-daily-report)

(provide 'mygtd)
