let
  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };

  nix-filter = import sources.nix-filter;

in rec
  {
    # Build with nix-build -A <attr>
    binaries = nixpkgs.haskellPackages.language-glsl;
    # binaries + haddock are also available as binaries.all.
    haddock = nixpkgs.haskellPackages.language-glsl.doc;

    # A shell to try out our binaries
    # Run with nix-shell default.nix -A shell
    shell = nixpkgs.mkShell {
      buildInputs = [
        binaries
      ];
      shellHook = ''
        # TODO Uncomment when the program uses optparse-applicative.
        # source <(glsl-pprint --bash-completion-script `which glsl-pprint`)
      '';
    };
  }
