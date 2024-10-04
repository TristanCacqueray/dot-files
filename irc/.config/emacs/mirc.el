;;; mirc -- my irc!

;; Copyright © 2024 Tristan de Cacqueray
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package erc
  :config
  (setq erc-server "irc.libera.chat"
        erc-nick "tristanC"
        erc-user-full-name "Tristan"
        erc-track-shorten-start 8
        erc-kill-buffer-on-part t
        erc-use-auth-source-for-nickserv-password t
        erc-auto-query 'bury)
  )

(provide 'mirc)

;;; mirc.el ends here
