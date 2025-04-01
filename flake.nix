{ description = "A flake for a haskell Gameboy emulator";

  outputs = { self, nixpkgs ? import <nixpkgs> }: let
    pkgName = "hboy";
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    haskPkgs = pkgs.haskell.packages.ghc965;
  in {
    packages.${system}.${pkgName} = haskPkgs.developPackage { root  = ./.; };
    defaultPackage.${system} = self.packages.${system}.${pkgName};

    devShells = {
      ${system}.default = pkgs.mkShell {
        buildInputs = [
          ( haskPkgs.ghcWithPackages ( p: [
            p.haskell-language-server
            p.ghcid
            p.hlint
          ]))

          pkgs.cabal-install

          pkgs.pkg-config
          pkgs.SDL2 pkgs.SDL2_mixer
        ];
      };
    };
  };
}
