{ compiler ? "ghc883"
, nixpkgs ? import <nixpkgs> {}
}:

let
  inherit (nixpkgs) pkgs;

  essence-of-live-coding = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding";
    ver = "0.2.1";
    sha256 = "105dcfklvcjfgrdg47q36lln81mglds4jjq1g8drflmc59p2s0hk";
  } {};
  essence-of-live-coding-gloss = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding-gloss";
    ver = "0.2.1";
    sha256 = "11az32n65r7k1j1wm38zddilfzm1p6rrdd2w9rynrsslx8pddiqz";
  } {};
  essence-of-live-coding-pulse = haskellPackages.callHackageDirect {
    pkg = "essence-of-live-coding-pulse";
    ver = "0.2.0.1";
    sha256 = "01cnvs1xjdb0pgjsl6qxrhq17b5qz9kw539v2j25iac08cqhzh7y";
  } {};
  # essence-of-live-coding-pulse = haskellPackages.callHackageDirect {
  #   pkg = "essence-of-live-coding-pulse";
  #   ver = "0.2.1";
  #   sha256 = "1z1a0lvalcbazr1dv2zklaf6z47pc25hhzkm7h851qrvgqa05c31";
  # } {};

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      inherit essence-of-live-coding essence-of-live-coding-gloss essence-of-live-coding-pulse;
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
