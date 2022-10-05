{ compiler ? "ghc92" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  haskellOverrides = hself: hsuper: {
    "qualified-imports-plugin" =
      hself.callCabal2nix
        "qualified-imports-plugin"
        (gitignore ./.)
        {};
  };

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = haskellOverrides;
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."qualified-imports-plugin"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.haskell-language-server
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
  };

in
{
  inherit shell haskellOverrides;
  "qualified-imports-plugin" = myHaskellPackages."qualified-imports-plugin";
}
