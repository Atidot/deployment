{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghcjs"
}:
with nixpkgs;
let
  haskell = import ./haskell.nix { inherit nixpkgs compiler; };
  ease    = haskell.ease;
  haskellPackages' = haskell.packages;

  haskellPackages = haskellPackages'.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
        (self: hspkgs: {
          comonad          = ease hspkgs.comonad;
          http-types       = ease hspkgs.http-types;
          lens             = ease hspkgs.lens;
          aeson            = ease hspkgs.aeson;
          semigroupoids    = ease hspkgs.semigroupoids;
          exceptions       = ease hspkgs.exceptions;
          bifunctors       = ease hspkgs.bifunctors;
          QuickCheck       = ease hspkgs.QuickCheck;
          tasty-quickcheck = ease hspkgs.tasty-quickcheck;
          scientific       = ease hspkgs.scientific;
          temporary        = ease hspkgs.temporary;
          graphviz         = ease hspkgs.graphviz;
          text-short       = ease hspkgs.text-short;
          text-metrics     = ease hspkgs.text-metrics;
        });
  });

  haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
    atidot-deployment
    atidot-deployment-editor
  ]);

in
stdenv.mkDerivation rec {
  name = "atidot-deployment-js";

  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs = [
    haskellEnv
  ];
}
