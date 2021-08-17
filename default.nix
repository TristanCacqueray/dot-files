args@{ withX ? false }:
let
  devenv = import ~/src/github.com/podenv/devenv/default.nix {
    withEmacs = true;
    withEmacsEvil = false;
    withTools = true;
    withShake = true;
    withHaskell = true;
    withRust = true;
    withPurescript = true;
    withAts = true;
    withDarcs = true;
    withNix = true;
    withGit = true;
    withElixir = false;
    withX = withX;
    withRescript = true;
    withReason = false;
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
    withSolarized = withX;
    withYaml = true;
    withMarkdown = true;
    withRestructuredText = true;
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
    withRuntime = false;
  };

in devenv.devenv ++ [devenv.profile]
