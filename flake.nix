{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          beamPackages = with pkgs.beam_minimal; packagesWith interpreters.erlangR25;
        in
          rec {
            packages = flake-utils.lib.flattenTree rec {
              erlang-test-runner =
                beamPackages.callPackage ./nix/test_runner.nix { inherit self; };
            };
            defaultPackage = packages.erlang-test-runner;
            devShells.default = pkgs.mkShell {
              packages = builtins.attrValues {
                inherit (beamPackages) erlang rebar3;
                inherit (pkgs) alejandra nil;
              };
            };
          }
    );
}
