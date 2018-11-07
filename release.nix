let

  config   = { allowUnfree = true; };
  overlays = [
    (newPkgs: oldPkgs: rec {

      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          shared-memory = haskellPackagesNew.callPackage ./default.nix { };
        };
      };

    })
  ];

  nixpkgs = import ./nix/18_09.nix;
  pkgs    = import nixpkgs { inherit config overlays; };

in

  { shared-memory = pkgs.haskellPackages.shared-memory; }
