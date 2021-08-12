# default.nix
{ compilerVersion ? "ghc8104", pkgsLink ? (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/59ac1c0c4831cfe80a145f52793a8805facfecfc.tar.gz)}:
let
  # Packages
  config = { allowBroken = true;
             allowUnfree = true;
           };
  pkgs = import pkgsLink { inherit config; };

  # Haskell compilier
  compiler = pkgs.haskell.packages."${compilerVersion}";

  #Package
  pkg = compiler.developPackage {
    root = builtins.filterSource
            (path: type:
              baseNameOf path != ".git"
              && baseNameOf path != "dist")
            ./.;  # Filter out directories. Is recursive, but excluding a directory excludes all paths below it too.
  };
  buildInputs = [
                  pkgs.ghcid
                  pkgs.cabal-install
                ];
in (pkg.overrideAttrs(attrs: {
  buildInputs = attrs.buildInputs ++ buildInputs;
}))

