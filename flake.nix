{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-parts.url = "github:hercules-ci/flake-parts";
    devenv.url = "github:cachix/devenv";
  };

  outputs = inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.devenv.flakeModule ];
      systems = nixpkgs.lib.systems.flakeExposed;

      perSystem = { lib, pkgs, ... }: {
        devenv.shells.default = {
          packages = [ pkgs.hyperfine ];
          languages.clojure.enable = true;
          languages.java.jdk.package = pkgs.graalvm-ce;
        };
      };
    };
}
