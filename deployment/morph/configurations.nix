let
  plutus = import ../../. { system = "x86_64-linux"; };
  pkgs = plutus.pkgs;
  # machines.json is a file generated by terraform. We want defaults if it doesn't exist
  # so that we can run tests of the NixOS config on Hydra
  machines =
    let p = ./machines.json;
    in
    if builtins.pathExists p
    then plutus.pkgs.lib.importJSON p
    else { rootSshKeys = [ ]; monitoringSshKeys = [ ]; };
  stdOverlays = [ ];
  nixpkgsLocation = https://github.com/NixOS/nixpkgs/archive/5272327b81ed355bbed5659b8d303cf2979b6953.tar.gz;
  ports = {
    http = 80;
    ssh = 22;
    prometheus = 9090;
    nodeExporter = 9100;
    webGhcExporter = 9091;
    pab-webserver = 8080;
  };
  options = { inherit stdOverlays machines nixpkgsLocation ports; };
  monitoringKeys = machines.monitoringSshKeys;
  defaultMachine = (import ./default-machine.nix) options;
  web-ghc = plutus.web-ghc;
  webGhcMachine = import ./webghc.nix;
  marloweDash = plutus.marlowe-dashboard;
  marloweDashMachine = import ./marlowe-dash.nix;
  prometheusMachine = import ./prometheus.nix;
  plutus-pab = plutus.plutus-pab;
  marlowe-app = plutus.marlowe-app;
  pabMachine = import ./pab.nix;
in
{
  # We partially apply mkInstance, it also expects other values like hostName
  # however this means we can add it later on a host-by-host basis while haveing exactly 
  # the same config that we can test separately in Hydra with a fake values
  marloweDash = marloweDashMachine.mkInstance (options // { inherit defaultMachine marloweDash pkgs; });
  webGhc = webGhcMachine.mkInstance (options // { inherit defaultMachine web-ghc monitoringKeys; });
  prometheus = prometheusMachine.mkInstance (options // { inherit defaultMachine monitoringKeys; });
  pab = pabMachine.mkInstance (options // { inherit defaultMachine monitoringKeys plutus-pab marlowe-app pkgs;inherit (plutus) marlowe-dashboard; });
  inherit pkgs ports;
}
