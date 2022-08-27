-- ~/src/github.com/podenv/local-hub/package.dhall
let Podenv = env:PODENV

let local = ~/.config/podenv/local.dhall ? {=}

let -- todo: migrate ./devenv.dhall
    devenv =
      {=}

in  Podenv.Hub // local
