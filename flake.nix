# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
{
  description = "clash-protocols-memmap development environment";

  inputs = {
    clash-compiler.url = "github:clash-lang/clash-compiler?rev=5ec337c04139f74dd706db77ccb25f92ad5dc3dc";
    clash-protocols = {
      url = "github:clash-lang/clash-protocols?rev=9b12c63dabaaa5a8228286bf9406b58c44a48359";
      inputs.clash-compiler.follows = "clash-compiler";
    };
  };
  outputs = { self, flake-utils, clash-compiler, clash-protocols, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # The 'default' version of ghc to use
        default-version = clash-compiler.ghcVersion.${system};
        # A list of all ghc versions this package supports
        supported-versions = clash-compiler.supportedGhcVersions.${system};

        regular-pkgs = import clash-compiler.inputs.nixpkgs {
          inherit system;
        };

        all-overlays = builtins.listToAttrs (map (compiler-version:
          let
            overlay = final: prev: {
              # Append the package set with clash-cores
              clash-protocols-memmap = (prev.developPackage {
                root = ./clash-protocols-memmap;
                overrides = _: _: final;
              });
              clash-bitpackc = (prev.developPackage {
                root = ./clash-bitpackc;
                overrides = _: _: final;
              });
            } // clash-protocols.overlays.${system}.${compiler-version} final prev;
          in
            { name = compiler-version; value = overlay; }
          ) supported-versions);

        all-hs-pkgs = builtins.mapAttrs (compiler-version: overlay:
          let
            pkgs = (import clash-compiler.inputs.nixpkgs {
              inherit system;
            }).extend clash-compiler.overlays.${compiler-version};
            clash-pkgs = pkgs."clashPackages-${compiler-version}";

            hs-pkgs = clash-pkgs.extend overlay;
          in
            hs-pkgs
          ) all-overlays;

        minimal-shell = hs-pkgs: hs-pkgs.shellFor {
          packages = p: [
            p.clash-protocols-memmap
            p.clash-bitpackc
          ];

          # https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
          buildInputs = [
            regular-pkgs.bashInteractive
          ];

          nativeBuildInputs = [
            hs-pkgs.cabal-install
            hs-pkgs.cabal-plan
            hs-pkgs.fourmolu
          ];
        };

        all-shells = clash-compiler.inputs.nixpkgs.lib.attrsets.concatMapAttrs (name: hs-pkgs: {
            # The difference between the `-minimal` and `-full` is the addition of HLS in the full version
            # This is because HLS is slow to compile and not everyone uses it
            # We default to using the `-minimal` version when `nix develop`ing
            "${name}-minimal" = minimal-shell hs-pkgs;
            "${name}-full" = (minimal-shell hs-pkgs).overrideAttrs (fAttr: pAttr: {
              nativeBuildInputs = pAttr.nativeBuildInputs ++ [
                hs-pkgs.haskell-language-server
              ];
            });
          }) all-hs-pkgs;

        all-packages = builtins.mapAttrs (_: hs-pkgs: {
          clash-protocols-memmap = hs-pkgs.clash-protocols-memmap;
          clash-bitpackc = hs-pkgs.clash-bitpackc;
        }) all-hs-pkgs;
      in
      {
        # Expose the overlay of each supported version which adds clash-cores
        # The base of the overlay is clash-pkgs
        overlays = all-overlays // { default = all-overlays.${default-version}; };

        # A devShell for each supported version
        #
        # These can be invoked using `nix develop .#ghc9101-minimal`
        #
        # Please do note that if you work with Nix, you need to remove ALL the `cabal*.project` files at
        # the root of the directory! Cabal prioritizes local source overrides over Nix, which causes
        # the many packages to incorrectly be fetched.
        devShells = all-shells // { default = all-shells."${default-version}-minimal"; };

        # Packages for each version of GHC, with a default package being set to the default-version's version
        packages = all-packages;
      });
}
