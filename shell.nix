{ pkgs ? import <nixpkgs> {} }:

let
  unstable = import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { };
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      cabal-install
      unstable.ghc
      binutils
      delta
      zlib
      zlib.dev
      zlib.out
      unstable.haskellPackages.haskell-language-server
    ];
    shellHook = ''
      eval $(egrep ^export ${unstable.ghc}/bin/ghc)
    '';
  }
