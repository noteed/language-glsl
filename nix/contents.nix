let

  sources = import ./sources.nix;
  defNixpkgs = import sources.nixpkgs { };
  nix-filter = import sources.nix-filter;

in { nixpkgs ? defNixpkgs }:

let inherit (nixpkgs.lib.attrsets) getAttrFromPath;
in {
  # Lists all packages made available through this nix project.
  # The format is `{ <pkgName> : <pkgDir> }` (we refer to this as pInfo).
  # The used directory should be the path of the directory relative to the root
  # of the project.
  pkgList = {
    language-glsl = nix-filter {
      root = ../.;
      include = with nix-filter; [
        "language-glsl.cabal"
        (and "bin" (or_ (matchExt "hs") isDirectory))
        (and "." (or_ (matchExt "hs") isDirectory))
        (and "tests" (or_ (matchExt "hs") isDirectory))
        # Required for the test suite.
        (and "glsl" (or_ (matchExt "glsl") isDirectory))
        "LICENSE"
      ];
    };
  };

  # Get an attribute from a string path from a larger attrSet
  getPkg = pkgs: pPath: getAttrFromPath [pPath] pkgs;
}
