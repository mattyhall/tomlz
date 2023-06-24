{
  description = "A TOML library for Zig";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    zig = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zls = {
      url = "github:erikarvstedt/zls/fix-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {self, nixpkgs, flake-utils, flake-compat, zig, zls}:
    let
      overlays = [
        (final: prev: { 
          zigpkgs = zig.packages.${prev.system};
        })
        (final: prev: {
          zlspkgs = zls.packages.${prev.system};
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
                zigpkgs.master-2023-06-23
                zlspkgs.default
                bashInteractive
                gdb
                lldb
              ]);
            };
          }
      );
}
