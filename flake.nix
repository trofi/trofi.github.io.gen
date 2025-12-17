{
  description = "Development environment for trofi.github.io.gen";
  inputs = {
    nixpkgs.url = "flake:nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
    };
  in {
    devShells.default = pkgs.mkShell {
      nativeBuildInputs = [
        (pkgs.haskellPackages.ghcWithPackages (p: [
          p.hakyll
          p.mtl
          p.pandoc-types
          p.pandoc
          p.text
        ]))
        pkgs.gnuplot
        pkgs.graphviz
      ];
    };
  });
}
