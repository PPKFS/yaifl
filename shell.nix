{ pkgs ? import <nixpkgs> {} }:

let
  ghc = pkgs.haskell.compiler.ghc902;
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      cabal-install
      ghc
      binutils
      zlib
      zlib.dev
      zlib.out
      pkgs.haskell.packages.ghc902.haskell-language-server
    ];
    shellHook = ''
      eval $(egrep ^export ${ghc}/bin/ghc)
    '';
  }
