{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-parts.url = "github:hercules-ci/flake-parts";
    devenv.url = "github:cachix/devenv";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.devenv.flakeModule ];
      systems = inputs.nixpkgs.lib.systems.flakeExposed;

      perSystem =
        { pkgs, ... }:
        {
          devenv.shells.default = {
            packages = [
              pkgs.hyperfine
              (pkgs.leiningen.override { jdk = pkgs.graalvm-ce; })
            ];
            languages.clojure.enable = true;
            languages.java.jdk.package = pkgs.graalvm-ce;
          };
        };
    };
}
