{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc864"
}:
with nixpkgs;
let
  haskell = import ./haskell.nix { inherit nixpkgs compiler; };
  haskellPackages = haskell.packages;

  haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
    atidot-deployment
  ]);

in
stdenv.mkDerivation rec {
  name = "deployment";

  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs = [
    haskellEnv
  ];
}
