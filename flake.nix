{
  description = "pikabook.me";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    feedback.url = "github:NorfairKing/feedback";
    sydtest.url = "github:NorfairKing/sydtest";
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    pre-commit-hooks,
    feedback,
    ...
  } @ inputs: let
    pkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.default
          inputs.opt-env-conf.overlays.${system}
          inputs.sydtest.overlays.${system}
        ];
      };
    src = nix-filter.lib {
      root = ./.;
      include = [
        "src/"
        "test/"
        "test-resources"
        "app/"
        "migrations/"
        "package.yaml"
        "LICENSE"
        "assets/"
      ];
    };
  in
    {
      overlays.default = final: prev: rec {
        hirc = final.haskell.lib.justStaticExecutables (
          final.haskellPackages.hirc.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ ["--ghc-options=-O2"];
          })
        );

        haskellPackages = prev.haskell.packages.ghc912.override (old:
          with final.haskell.lib; {
            overrides =
              final.lib.composeExtensions
              (old.overrides or (_: _: {}))
              (self: super: {
                hirc =
                  self.generateOptparseApplicativeCompletions ["hirc"]
                  (self.callCabal2nix "hirc" src {});
              });
          });
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = pkgsFor system;
        precommitCheck = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            actionlint.enable = true;
            alejandra.enable = true;
            beautysh.enable = true;
            check-merge-conflicts.enable = true;
            hlint.enable = true;
            hpack.enable = true;
            markdownlint.enable = true;
            nil.enable = true;
            ormolu.enable = true;
            ripsecrets.enable = true;
            shellcheck.enable = true;
          };
        };
      in rec {
        packages.default = packages.hirc;
        packages.hirc = pkgs.hirc;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: with p; [hirc];
          buildInputs = with pkgs;
          with pkgs.haskellPackages;
            [
              alejandra
              cabal-install
              feedback.packages.${system}.default
              haskell-language-server
              hlint
              nil
              ormolu
              pkgs.helix
              statix
              yaml-language-server
            ]
            ++ packages.hirc.buildInputs;

          shellHook = ''
            ${precommitCheck.shellHook}
          '';
        };

        checks = {pre-commit-check = precommitCheck;};
      }
    );
  nixConfig = {
    extra-substituters = [
      "https://opensource.cachix.org"
      "https://haskell-language-server.cachix.org"
      "https://feedback.cachix.org"
    ];
    extra-trusted-public-keys = [
      "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc="
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "feedback.cachix.org-1:8PNDEJ4GTCbsFUwxVWE/ulyoBMDqqL23JA44yB0j1jI="
    ];
  };
}
