{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
        inputs.fourmolu-nix.flakeModule
      ];
      perSystem = { self', inputs', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          # To avoid unnecessary rebuilds, we filter projectRoot:
          # https://community.flake.parts/haskell-flake/local#rebuild
          projectRoot = builtins.toString (lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./exe
              ./poly.cabal
            ];
          });
          devShell = {
            tools = hp: {
              treefmt = config.treefmt.build.wrapper;
            } // config.treefmt.build.programs;
          };
          packages = {
            hspray.source = "0.5.2.0";
          };
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;
          programs.fourmolu = {
            enable = true;
            package = config.fourmolu.wrapper;
          };
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;
        };
        fourmolu.settings = {
          indentation = 2;
          comma-style = "leading";
          record-brace-space = true;
          indent-wheres = true;
          import-export-style = "diff-friendly";
          respectful = true;
          haddock-style = "multi-line";
          newlines-between-decls = 1;
          extensions = [ "ImportQualifiedPost" ];
        };
        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Format the source tree";
            exec = config.treefmt.build.wrapper;
            category = "Dev Tools";
          };
          ghcid = {
            description = "Run the project with ghcid auto-recompile";
            exec = ''
              ghcid -c "cabal repl $*" --warnings
            '';
            category = "Primary";
          };
          dev = {
            description = "Run the project with ghciwatch";
            exec = ''
              ghciwatch --clear --watch test/ --watch src/ --watch exe/ --test-ghci Main.main --before-reload-shell treefmt
            '';
            category = "Primary";
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.poly;
        apps.default = self'.apps.poly;

        devShells.default = pkgs.mkShell {
          name = "poly";
          meta.description = "Haskell development environment";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.treefmt.build.devShell
            config.mission-control.devShell
          ];
          nativeBuildInputs = with pkgs; [
            ghciwatch
            nixd
            just
            (python3.withPackages (p: with p; [ sympy numpy ]))
            (rWrapper.override { packages = [ rPackages.spray ]; })
            # (julia.withPackages [ "TypedPolynomials" "LinearAlgebra" ]) # broken
          ] ++ (with pkgs.haskellPackages; [
            pointfree
          ]);
        };
      };
    };
}
