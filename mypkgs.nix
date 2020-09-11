{ compiler ? "ghc883"
# Leave the next line to use a fairly recent nixos-unstable.
, nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/16fc531784ac226fb268cc59ad573d2746c109c1.tar.gz") {}
# Comment the above and uncomment the following for bleeding-edge nixos-unstable. You might want to `cachix use manuelbaerenz` or wait a long time for builds.
# , nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/c59ea8b8a0e7f927e7291c14ea6cd1bd3a16ff38.tar.gz") {}
# If you have a nix-channel installed locally which you want to use, uncomment and possibly edit the following line, and comment the lines above.
# , nixpkgs ? import <nixpkgs> {}
}:

let
  inherit (nixpkgs) pkgs;

  essence-of-live-coding = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding";
    ver = "0.2.4";
    sha256 = "08pjcsjwsb13ld4f1r1kkzmssx6rfv3qsgnwn17i1ag6dqjq5hrd";
  } {};
  essence-of-live-coding-gloss = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding-gloss";
    ver = "0.2.4";
    sha256 = "03rwwn8a3sjl97mznbymm512j1jak647rawf7pr6cvaz5ljg6kw3";
  } {};
  essence-of-live-coding-pulse = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding-pulse";
    ver = "0.2.4";
    sha256 = "10w6pzi7bazk4h8ci67pwg2fl71kwxhf2vbpdalds37dxiq3a6p0";
  } {};
  essence-of-live-coding-warp = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding-warp";
    ver = "0.2.4";
    sha256 = "01am1azb62qsq49cky6accpbxmr7c81ci1jkrb3gjp9c3jcs9kkv";
  } {};

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      inherit
        essence-of-live-coding
        essence-of-live-coding-gloss
        essence-of-live-coding-pulse
        essence-of-live-coding-warp
      ;
      http-client = super.http-client_0_7_1;
    };
  };

  myPkgs = haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
    essence-of-live-coding-tutorial = ./.;
    # Uncomment the following lines if you have forked essence-of-live-coding and insert the appropriate path
    # essence-of-live-coding = ../essence-of-live-coding/essence-of-live-coding;
    # essence-of-live-coding-gloss = ../essence-of-live-coding/essence-of-live-coding-gloss;
  });
in
myPkgs
