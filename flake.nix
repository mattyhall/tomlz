{
  description = "A TOML library for Zig";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    zls = {
      url = "github:zigtools/zls";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    zig = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {self, nixpkgs, flake-utils, zls, flake-compat, zig}:
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
          {
            devShell = pkgs.mkShell {
              buildInputs = (with pkgs; [
                zigpkgs.master-2024-06-18
                zlspkgs.default
                bashInteractive
                gdb
                lldb
              ]);
            };
          }
      );
}
