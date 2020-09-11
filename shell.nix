let myPkgs = import ./mypkgs.nix {};
in myPkgs.shellFor {
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
