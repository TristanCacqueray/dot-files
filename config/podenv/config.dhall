let Podenv = ~/src/github.com/podenv/local-hub/package.dhall

let local = ~/.config/podenv/local.dhall ? {=}

let -- todo: migrate ./devenv.dhall
    devenv =
      {=}

in  Podenv.Hub // local
