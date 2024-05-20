{
  description = "A TOML library for Zig";

  inputs = {
    # 0.12.0 is on revision 07c88f35d25ba5d0fff5074900582ce89c0d6ad0
    # look at: https://lazamar.co.uk/nix-versions/?package=zig
    nixpkgs-zig-pinned.url = "github:NixOS/nixpkgs/07c88f35d25ba5d0fff5074900582ce89c0d6ad0";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs-zig-pinned, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
        devShell =
          with nixpkgs-zig-pinned.legacyPackages.${system};
          mkShell {
            buildInputs = [
              nixpkgs-zig-pinned.legacyPackages.${system}.zig
              nixpkgs-zig-pinned.legacyPackages.${system}.zls
              nixpkgs-zig-pinned.legacyPackages.${system}.lldb
            ];
            shellHook = ''
              '';
          };
      }
  );
}
