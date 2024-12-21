(require 'tidal)
;; todo: grab the output of `ls $(nix build modulatix#tidal)/bin/ghci'
(setq tidal-interpreter "/nix/store/nw9blhcbq2ci748yh81givw2k88a2hfz-ghc-9.6.6-with-packages/bin/ghci")
(setq tidal-boot-script-path "~/.config/tidal/BootTidal.hs")
