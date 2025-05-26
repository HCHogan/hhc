{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
        };
        name = "hhc";
        src = ./.;
        llvm16 = pkgs.llvmPackages_16;
      in {
        packages.default = pkgs.stdenv.mkDerivation {
          pname = name;
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = [
            pkgs.haskell.compiler.ghc912
            pkgs.cabal-install

            llvm16.libllvm
            llvm16.bintools
            llvm16.libcxxClang
          ];

          unpackPhase = "true";

          buildPhase = ''
            mkdir -p /tmp/cabal
            mkdir -p /tmp/dist-newstyle
            export CABAL_DIR=/tmp/cabal
            if [ ! -f "$CABAL_DIR/config" ]; then
              cabal user-config init
            fi
            cabal update
            cd $src
            cabal -j build --enable-tests --builddir=/tmp/dist-newstyle
          '';

          checkPhase = ''
            export CABAL_DIR=/tmp/cabal
            cabal test --enable-tests --show-details=always
          '';

          installPhase = ''
            export CABAL_DIR=/tmp/cabal
            cd "$src"
            cabal install \
              --builddir=/tmp/dist-newstyle \
              --install-method=copy \
              --installdir="$out/bin" \
              --overwrite-policy=always
          '';
        };
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            gnumake
            llvm16.libllvm
            llvm16.bintools
            llvm16.libcxxClang

            haskell.compiler.ghc912
            haskell.packages.ghc912.haskell-language-server
            haskellPackages.hoogle
            haskellPackages.ghci-dap
            haskellPackages.haskell-debug-adapter
            haskellPackages.fast-tags
            haskellPackages.alex
            haskellPackages.happy
            haskellPackages.cabal-fmt
            haskellPackages.ormolu
            cabal-install
            hlint
          ];
          shellHook = ''
            export SHELL=$(which zsh)
            exec zsh
          '';
        };
      }
    );
}
