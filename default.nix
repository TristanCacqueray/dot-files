args@{ withX ? false }:
let
  nixpkgs = import ~/src/github.com/podenv/devenv/nixpkgs.nix;
  devenv = import ~/src/github.com/podenv/devenv/default.nix {
    withEmacs = true;
    withTools = true;
    withShake = true;
    withHaskell = true;
    withRust = true;
    withPurescript = true;
    withAts = true;
    withDarcs = true;
    withNix = true;
    withElixir = true;
    withX = withX;
    withReason = true;
    withLsp = true;
    withPython = true;
    withRpm = true;
    withDhall = true;
    withHy = true;
    withRacket = true;
    withW3M = true;
    withIntel = withX;
    withVulkan = withX;
    withOpenGL = withX;
    withNeuron = true;
    withGLSL = true;
    withGo = true;
    withProtobuf = true;
    withThrift = true;
    withTypescript = true;
    withPlantuml = true;
    withNotMuch = withX;
    withOrg = true;
    withRest = true;
    withGraphQL = true;
  };
  # TODO: add module custom env to devenv and move this to !withX
  custom-env = (if withX then
    ""
  else ''
    export TERM=xterm-256color
  '');
in nixpkgs.mkShell {
  buildInputs = devenv.devenv;
  shellHook = devenv.shellEnv.shellHook + custom-env;
}
