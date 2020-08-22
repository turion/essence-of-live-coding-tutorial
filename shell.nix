{ compiler ? "ghc883"
, nixpkgs ? import <nixpkgs> {}
, glfw ? true
}:

let
  inherit (nixpkgs) pkgs;

  # Use self in overrides instead
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
  # GLFW-b = (haskellPackages.callHackageDirect {
  #   pkg = "GLFW-b";
  #   ver = "1.4.8.4"; # Gloss needs an old version
  #   sha256 = "05psnzz78gpv6mbm84zbgnadbz8rcis08ylakv89i0harz0vzf30";
  # } {}).overrideAttrs (_: { doCheck = false; }); # The test suite fails for whatever reason

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      inherit
        essence-of-live-coding
        essence-of-live-coding-pulse
        essence-of-live-coding-warp
        http-client
        # GLFW-b
      ;
      GLFW-b = pkgs.haskell.lib.dontCheck (self.callHackageDirect {
        pkg = "GLFW-b";
        ver = "1.4.8.4"; # Gloss needs an old version
        sha256 = "05psnzz78gpv6mbm84zbgnadbz8rcis08ylakv89i0harz0vzf30";
      } {});

      gloss = pkgs.haskell.lib.disableCabalFlag (pkgs.haskell.lib.enableCabalFlag (super.gloss.overrideAttrs (old: {
        # configureFlags = if glfw
        #   then ["-fglfw" "-f-glut" ]
        #   else [];
        buildInputs = old.buildInputs ++ (with self; [
          GLFW-b
        ]);
      })) "GLFW") "GLUT";

      # gloss = pkgs.haskell.lib.disableCabalFlag (pkgs.haskell.lib.enableCabalFlag super.gloss "GLFW") "GLUT";

      essence-of-live-coding-gloss = essence-of-live-coding-gloss.overrideAttrs (old: {
        configureFlags = if glfw
          then ["-fglfw" "-f-glut" ]
          else [];
        buildInputs = old.buildInputs ++ (with self; [
          gloss
          GLFW-b
        ]);
      });

      essence-of-live-coding-tutorial = (super.callCabal2nix "essence-of-live-coding-tutorial" ./. {}).overrideAttrs (old: {
        buildInputs = old.buildInputs ++ (with self; [
          gloss
          GLFW-b
          essence-of-live-coding-gloss
        ]);
      });

    };
  };

  myPkgs = haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
    # essence-of-live-coding-tutorial = ./.;
    # Uncomment the following lines if you have forked essence-of-live-coding and insert the appropriate path
    # essence-of-live-coding = ../essence-of-live-coding/essence-of-live-coding;
    # essence-of-live-coding-gloss = ../essence-of-live-coding/essence-of-live-coding-gloss;
    # GLFW-b = "1.4.8.4";
  });
in
myPkgs.shellFor {
  packages = p: with p; [
    # gloss
    # GLFW-b
    # GLURaw
    # OpenGLRaw
    essence-of-live-coding-tutorial
    # essence-of-live-coding
    # essence-of-live-coding-gloss
    # essence-of-live-coding-pulse
    # essence-of-live-coding-warp
    GLUT
  ];
  buildInputs = with myPkgs; [
    cabal-install
    ghcid
    ghcide
    hlint
    GLUT
  ];
}
