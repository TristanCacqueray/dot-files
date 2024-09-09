let Podenv = ~/src/github.com/podenv/local-hub/Podenv.dhall

let Hub = ~/src/github.com/podenv/local-hub/Applications/package.dhall

in  Podenv.Application::{
    , name = "devenv"
    , description = Some "A developer environment"
    , namespace = Some "host"
    , runtime = Hub.nix.useInstallables [ "github:podenv/devenv" ]
    , command = [ "nix-shell", "--arg", "withX", "true", "--command", "emacs" ]
    , environ = [ "EDITOR=emacsclient", "GDK_BACKEND=x11" ]
    , volumes = [ "~/src", "~/.config/devenv:~" ]
    , capabilities = Podenv.Capabilities::{
      , x11 = True
      , terminal = True
      , wayland = True
      , dbus = True
      , dri = True
      , pipewire = True
      , pulseaudio = True
      , network = True
      }
    }
