{
  description = "A flake for a haskell Gameboy emulator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    pkgName = "hboy";
    haskPkgs = pkgs.haskell.packages.ghc948;

  in {
    packages.${system}.${pkgName} = pkgs.haskellPackages.callCabal2nix pkgName ./. {};
    defaultPackage.${system} = self.packages.${system}.${pkgName};

    devShells = {
      ${system}.default = pkgs.mkShell {
        buildInputs = [( haskPkgs.ghcWithPackages ( p: [
          p.haskell-language-server
          p.ghcid
        ])) ];
      };
    };
  };
}
