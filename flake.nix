{
  description = "A TOML library for Zig";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    zig.url = "github:mitchellh/zig-overlay";
  };

  outputs = {self, nixpkgs, flake-utils, flake-compat, zig}:
    let
      overlays = [
        (final: prev: { 
          zigpkgs = zig.packages.${prev.system};
        })
      ];
      systems = builtins.attrNames zig.packages;
    in
      flake-utils.lib.eachSystem systems (system:
        let
          pkgs = import nixpkgs { inherit overlays system; };
        in
          rec {
            devShell = pkgs.mkShell {
              buildInputs = (with pkgs; [
                zigpkgs.default
                bashInteractive
                zls
                gdb
                lldb
              ]);
            };
          }
      );
}
