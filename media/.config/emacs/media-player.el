;;; media-player -- setup a media player

;; Copyright © 2024 Tristan de Cacqueray
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package ready-player
  :load-path "/srv/github.com/xenodium/ready-player"
  :custom
  ;; keep the buffer separator to avoid confusion
  (ready-player-hide-modeline nil)
  ;; disable video window, I just want music. TODO: implement a behavior toggle
  (ready-player-open-playback-commands
   '(("mpv" "--audio-display=no" "--video=no" "--input-ipc-server=<<socket>>")))
  ;; easy access to my library
  (ready-player-my-media-collection-location "~/Music/")
  :config
  (ready-player-mode +1))

(provide 'media-player)

;;; media-player.el ends here
