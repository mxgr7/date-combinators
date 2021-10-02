# -*- compile-command: "nix-shell --run 'cabal exec -- ghc-pkg list'"; -*-
{ pkgs ? import <nixpkgs> {}, sources ? import ./nix/sources.nix {} }:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/make-package-set.nix
pkgs.haskellPackages.developPackage {
    root = ./.;
    withHoogle = false;
    returnShellEnv = false;
    overrides = self: _:  {
      yahp = self.callCabal2nix "yahp" sources.yahp {};
      interval = self.callCabal2nix "interval" sources.interval {};
    };
    source-overrides = {
      vector-algorithms = "0.9.0.1";
      chronos = sources.chronos;
    };
    modifier = with pkgs.haskell.lib; drv:
      disableLibraryProfiling (dontHaddock (addBuildTools drv
        (with pkgs.haskellPackages; [cabal-install ghcid])));
  }
