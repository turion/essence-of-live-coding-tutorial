{ compiler ? "ghc883"
, nixpkgs ? import <nixpkgs> {}
}:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler};

  myPkgs = haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
    essence-of-live-coding-tutorial = ./.;
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
