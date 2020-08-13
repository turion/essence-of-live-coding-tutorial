{ compiler ? "ghc883"
, nixpkgs ? import <nixpkgs> {}
}:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super : {
      essence-of-live-coding = haskellPackages.callHackageDirect {
        pkg = "essence-of-live-coding";
        ver = "0.2.0.1";
        sha256 = "105dcfklvcjfgrdg47q36lln81mglds4jjq1g8drflmc59p2s0hk";
      } {};
      essence-of-live-coding-gloss = haskellPackages.callHackageDirect {
        pkg = "essence-of-live-coding-gloss";
        ver = "0.2.0.1";
        sha256 = "11az32n65r7k1j1wm38zddilfzm1p6rrdd2w9rynrsslx8pddiqz";
      } {};
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
    ghcid
    ghcide
    hlint
    # Add further build tools as you like
  ];
}
