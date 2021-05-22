let Podenv = env:PODENV_PRELUDE

let Hub = env:PODENV_HUB

let local = ./local.dhall ? ([] : List Podenv.Env.Type)

let devenv = ./devenv.dhall

let setDNS =
      Hub.Functions.mapEnv
        (\(env : Podenv.Env.Type) -> env // { dns = Some "192.168.42.42" })

let envs = Hub.Defaults # [ devenv ] # local

in  setDNS envs
