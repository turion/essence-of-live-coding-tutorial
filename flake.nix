{
  description = "Tutorial application for essence-of-live-coding";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
  };

  outputs = { self, nixpkgs, haskell-flake-utils, ... }@inputs:
    haskell-flake-utils.lib.simpleCabal2flake {
      inherit self nixpkgs;
      systems = [ "x86_64-linux" ];

      name = "essence-of-live-coding-tutorial";

      hpPreOverrides = { pkgs }: self: super:
        with pkgs.haskell.lib;
        with haskell-flake-utils.lib;
        tunePackages pkgs super {
          essence-of-live-coding = [ (jailbreakUnbreak pkgs) ];
        };
    };
}
