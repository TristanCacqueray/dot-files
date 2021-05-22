let Podenv = env:PODENV_PRELUDE

let Hub = env:PODENV_HUB

let env =
      Podenv.Env::{
      , name = "devenv"
      , network = Some "host"
      , home = Some "~/.config/devenv"
      , environ = Some (toMap { USER = "fedora", EDITOR = "emacsclient" })
      , hostname = Some "devenv"
      , work-dir = Some "/home/user"
      , mounts = Some [ { host-path = "~/src", container-path = "~/src" } ]
      }

in  Hub.Runtimes.Nixos.Create.Latest env
