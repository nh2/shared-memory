let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

in
  fetchNixpkgs {
     rev          = "a8ff2616603a6ff6bfb060368c12a973c8e007f6";
     sha256       = "15l57ra62w9imqv3cfx9qp1fag3mqp95x0hdh81cqjb663qxihlg";
     outputSha256 = "1nkpbwdx1jgr2pv5arllk6k56h3xc61jal7qi66g21qsx6daf0g3";
  }
