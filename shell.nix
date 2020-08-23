{ compiler ? "ghc883"
, nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/c59ea8b8a0e7f927e7291c14ea6cd1bd3a16ff38.tar.gz") {}
}:

let
  inherit (nixpkgs) pkgs;

  essence-of-live-coding = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding";
    ver = "0.2.3";
    sha256 = "1hqjpc27k9qjnyhmbhk181pl87h7mc8jxbcc09wwd0f9kx6sq9is";
  } {};
  essence-of-live-coding-gloss = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding-gloss";
    ver = "0.2.3";
    sha256 = "1wb0y7xpmfh5d5lpjqfg9k1vnzyqhkv73hvjvxp0qz3f2d4wf6qh";
  } {};
  essence-of-live-coding-pulse = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding-pulse";
    ver = "0.2.3";
    sha256 = "1g3r1lzbh01sbpnvwmyxkl07cpgmrin22hp1wxy00g7bl3zwzr23";
  } {};
  essence-of-live-coding-warp = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding-warp";
    ver = "0.2.3";
    sha256 = "196ld5hqfnmri9kxksyqy2s6jp6lnjfipa9709j2bc49i6g72hzv";
  } {};
  http-client = haskellPackages.callHackageDirect {
    pkg = "http-client";
    ver = "0.7.1"; # For some reason, nixpkgs holds an older version of this
    sha256 = "07wli92lcvj4sh8xw1lx71b1bw1wimg6cr891h5027jszyvrcaz3";
  } {};

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      inherit
        essence-of-live-coding
        essence-of-live-coding-gloss
        essence-of-live-coding-pulse
        essence-of-live-coding-warp
        http-client
      ;
    };
  };

  myPkgs = haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
    essence-of-live-coding-tutorial = ./.;
    # Uncomment the following lines if you have forked essence-of-live-coding and insert the appropriate path
    # essence-of-live-coding = ../essence-of-live-coding/essence-of-live-coding;
    # essence-of-live-coding-gloss = ../essence-of-live-coding/essence-of-live-coding-gloss;
  });
in
myPkgs.shellFor {
  packages = p: with p; [
    essence-of-live-coding-tutorial
  ];
  buildInputs = with myPkgs; [
    cabal-install
    ghcid
    ghcide
    hlint
    # Add further build tools as you like
  ];
}
