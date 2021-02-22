{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs-channels/nixos-20.03";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          beamPackages = pkgs.beam.packagesWith
            pkgs.beam.interpreters.erlangR22_nox;
        in
          rec {
            packages = flake-utils.lib.flattenTree rec {
              erlang-test-runner =
                beamPackages.callPackage ./nix/test_runner.nix { inherit self; };
            };
            defaultPackage = packages.erlang-test-runner;
            devShell = pkgs.mkShell {
              buildInputs = with beamPackages; [ erlang rebar3 ] ++ (with pkgs; [ nixpkgs-fmt ]);
            };
          }
    );
}
