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
        pkgs.rustc
        pkgs.cargo
        pkgs.rust-analyzer

        pkgs.gnuplot
        pkgs.graphviz

        # for run_watcher.sh
        pkgs.inotify-tools
      ];
    };
  });
}
