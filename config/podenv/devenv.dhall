let Podenv = env:PODENV_PRELUDE

let Hub = env:PODENV_HUB

let env =
      Podenv.Env::{
      , name = "devenv"
      , description = Some "A developer environment"
      , command = Some
        [ "nix-shell", "--arg", "withX", "true", "--command", "emacs" ]
      , network = Some "host"
      , home = Some "~/.config/devenv"
      , environ = Some (toMap { USER = "user", EDITOR = "emacsclient" })
      , hostname = Some "devenv"
      , work-dir = Some "/home/user"
      , mounts = Some [ { host-path = "~/src", container-path = "~/src" } ]
      , capabilities = Podenv.Capabilities::{
        , x11 = Some True
        , terminal = Some True
        , network = Some True
        }
      }

in  Hub.Runtimes.Nixos.Create.Latest env
