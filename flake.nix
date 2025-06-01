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
        basePkg = pkgs.haskellPackages.callCabal2nix name src {};

        pkg = basePkg.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [pkgs.llvmPackages_16.libllvm pkgs.llvmPackages_16.bintools];
        });
      in {
        packages.default = pkg;
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
        };
      }
    );
}
