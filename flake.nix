{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages = {
          default = self.packages.${system}.sqitch_tool;

          sqitch_tool = pkgs.ocamlPackages.buildDunePackage {
            pname = "sqitch_tool";
            version = "0.0.0";
            duneVersion = "3";
            src = ./.;

            propagatedBuildInputs = with pkgs.ocamlPackages; [fmt uutf fileutils cmdliner];

            #strictDeps = true;
            doCheck = false;
          };
        };

        devShells = {
          default = pkgs.mkShell {
            packages = with pkgs.ocamlPackages; [
              ocaml-lsp
              ocp-indent
            ];
            inputsFrom = [
              self.packages.${system}.sqitch_tool
            ];
          };
        };
      });
}
