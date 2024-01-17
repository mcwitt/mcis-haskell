{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system}.default = pkgs.haskellPackages.callCabal2nix "mcsplit" ./. { };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        packages = _: [ self.packages.${system}.default ];
        withHoogle = true;
        buildInputs = with pkgs.haskellPackages; [
          cabal-install
          haskell-language-server
          hlint
          ormolu
        ];
      };
    };
}
