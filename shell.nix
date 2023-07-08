{ pkgs ? import <nixpkgs> {} }:

let
  unstable = import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { };
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      cabal-install
      haskell.compiler.ghc945
      binutils
      delta
      zlib
      zlib.dev
      zlib.out
      haskell.packages.ghc94.haskell-language-server
    ];
    shellHook = ''
      eval $(egrep ^export ${unstable.ghc}/bin/ghc)
    '';
  }
