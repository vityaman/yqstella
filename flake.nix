{
  description = "YQStella";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    # GHC 9.8.4 (LTS 23.28) lives here; nixos-24.11 only had up to ghc982.
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { self, nixpkgs, nixpkgs-unstable }:
    let
      inherit (nixpkgs.lib) genAttrs cleanSource optionalAttrs;

      cleanSrc = cleanSource self;

      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = f: genAttrs systems (system: f nixpkgs.legacyPackages.${system} nixpkgs-unstable.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (
        pkgs: pkgsU:
        let
          ghc = pkgsU.haskell.compiler.ghc984;
          stackWrapped = pkgs.writeShellScriptBin "stack" ''
            export PATH="${ghc}/bin''${PATH:+:}$PATH"
            exec ${pkgs.stack}/bin/stack --system-ghc --no-install-ghc "$@"
          '';
        in
        {
          default = pkgs.mkShell (
            {
              buildInputs = with pkgs; [
                stackWrapped
                haskellPackages.hpack
                haskellPackages.BNFC
                haskellPackages.alex
                haskellPackages.happy
                gnumake
                gnupatch
                haskellPackages.ormolu
                haskellPackages.hlint
                git
                findutils
                bash
                which
              ];

              shellHook = ''
                export LC_ALL=C.UTF-8
              '';
            }
            // optionalAttrs pkgs.stdenv.isLinux {
              LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            }
          );
        }
      );

      packages = forAllSystems (
        pkgs: pkgsU:
        let
          ghc984 = pkgsU.haskell.compiler.ghc984;
          hlib = pkgsU.haskell.lib;
          hpkgs = pkgsU.haskell.packages.ghc984;

          yqstellaSrc = pkgs.stdenvNoCC.mkDerivation (
            {
              name = "yqstella-src";
              src = cleanSrc;

              nativeBuildInputs = with pkgsU; [
                haskellPackages.hpack
                haskellPackages.BNFC
                haskellPackages.alex
                haskellPackages.happy
                gnumake
                gnupatch
                ghc984
              ];

              # UTF-8 .cf / BNFC: avoid hGetContents invalid arg in minimal Nix build env
              buildPhase = ''
                runHook preBuild
                export LC_ALL=C.UTF-8
                export LANG=C.UTF-8
                export PATH="${ghc984}/bin''${PATH:+:}$PATH"
                ${pkgsU.bash}/bin/bash ./syntax/stella/codegen.bash
                ${pkgsU.bash}/bin/bash ./syntax/yson/codegen.bash
                hpack
                runHook postBuild
              '';

              installPhase = ''
                runHook preInstall
                mkdir -p $out
                cp -a . $out
                runHook postInstall
              '';
            }
            // nixpkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
              LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            }
          );

          yqstellaHaskell = hlib.dontCheck (hlib.dontHaddock (hpkgs.callCabal2nix "yqstella" yqstellaSrc { }));

          yqstellaImage =
            if pkgs.stdenv.isLinux then
              pkgs.dockerTools.buildLayeredImage {
                name = "yqstella";
                tag = "latest";
                maxLayers = 25;
                contents = [ yqstellaHaskell ];
                config = {
                  Entrypoint = [ "${yqstellaHaskell}/bin/yqstella" ];
                };
              }
            else
              null;
        in
        {
          yqstella = yqstellaHaskell;
          # OCI image tarball (linux). Load: docker load < result
          # Prefer building .#yqstella (binary) by default; images are large.
          default = yqstellaHaskell;
        }
        // optionalAttrs (yqstellaImage != null) { inherit yqstellaImage; }
      );
    };
}
