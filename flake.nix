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
    packages.${system}.${pkgName} = haskPkgs.callCabal2nix pkgName ./. {};
    defaultPackage.${system} = self.packages.${system}.${pkgName};

    devShells = {
      ${system}.default = pkgs.mkShell {
        buildInputs = [
          ( haskPkgs.ghcWithPackages ( p: [
            p.haskell-language-server
            p.ghcid
            p.cabal-install
          ]))

          pkgs.pkg-config
        ];

        inputsFrom = builtins.attrValues self.packages.${system};
      };
    };
  };
}
