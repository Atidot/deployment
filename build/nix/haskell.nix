{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc864"
, haskellPackages ? nixpkgs.haskell.packages.${compiler}
}:
with nixpkgs;
rec {
  ease = package: with haskell.lib;
    ( doJailbreak
    ( dontHaddock
    ( dontCheck
    ( package
    ))));

  stratosphereSrc = fetchGit {
    url = https://github.com/Atidot/stratosphere;
    rev = "8981ed145a0e582981403fb7ee7e10d5be5de508";
  };


  atidotDeploymentSrc       = ../../atidot-deployment;
  atidotDeploymentEditorSrc = ../../atidot-deployment-editor;

  projectPackages = hspkgs: with haskell.lib; {
    stratosphere             = hspkgs.callCabal2nix "stratosphere"             "${stratosphereSrc}" {};
    atidot-deployment        = hspkgs.callCabal2nix "atidot-deployment"        "${atidotDeploymentSrc}" {};
    atidot-deployment-editor = hspkgs.callCabal2nix "atidot-deployment-editor" "${atidotDeploymentEditorSrc}" {};
  };

  packages = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
      (self: hspkgs:
        projectPackages hspkgs
      );
  });
}
