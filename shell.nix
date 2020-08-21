{ compiler ? "ghc883"
, nixpkgs ? import <nixpkgs> {}
}:

let
  inherit (nixpkgs) pkgs;

  essence-of-live-coding = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding";
    ver = "0.2.2";
    sha256 = "03jlggbbm51wh0my41ywz1cq11v1vgvj21l2sic7gdg8hc2afnq5";
  } {};
  essence-of-live-coding-gloss = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding-gloss";
    ver = "0.2.2";
    sha256 = "10l8rb4zb2rsdsa8awkggwyf2gm40d5m1rl33bpdmz1rn984apav";
  } {};
  essence-of-live-coding-pulse = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding-pulse";
    ver = "0.2.2";
    sha256 = "0ykv261ph0858wja899d8jkx25c1nzgxm4wyw0bnvhz2r0nq7nr5";
  } {};
  essence-of-live-coding-warp = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding-warp";
    ver = "0.2.2";
    sha256 = "08h5yymvq1pr2jz6i230pkhh3bgwjjj1sazi5h3jr69nmn1zkxgh";
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
