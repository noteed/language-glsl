self: super:
let

  lib = super.lib;
  sources = import ./sources.nix;
  contents = import ./contents.nix { nixpkgs = super; };
  inherit (super.lib.attrsets) mapAttrs;

  ourOverrides = selfh: superh:
    let
      callCabalOn = name: dir:
        selfh.callCabal2nix "${name}" dir { };

    in mapAttrs callCabalOn contents.pkgList;

in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) ourOverrides;
  });

  haskellPackagesNoProfiling = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) ourOverrides;
  });
}
