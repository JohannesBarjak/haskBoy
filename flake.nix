{ description = "A flake for a haskell Gameboy emulator";

  outputs = { self, nixpkgs ? import <nixpkgs> }: let
    pkgName = "hboy";
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    haskPkgs = pkgs.haskell.packages.ghc96;
    in {
      packages.${system}.${pkgName} =
        let src = pkgs.nix-gitignore.gitignoreSource [] ./.;
        in haskPkgs.callCabal2nix "" src {};
    defaultPackage.${system} = self.packages.${system}.${pkgName};

    devShells = {
      ${system}.default = haskPkgs.shellFor {
        buildInputs = with pkgs; [
          ( with haskPkgs;
            [
              haskell-language-server
              ghcid
              hlint
            ])

          cabal-install
          pkg-config
        ];

        packages = haskPkgs: [
          self.defaultPackage.${system}
        ];

        withHoogle = true;
      };
    };
  };
}
