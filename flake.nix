{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = { nixpkgs, ... }: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in {
    devShells.x86_64-linux.default = pkgs.mkShell {
      buildInputs = let
        hsPkgs = pkgs.haskellPackages.ghcWithPackages (haskellPackages: with haskellPackages; [
          # tools
          cabal-install
          haskell-language-server

          # libraries
          ghc-tcplugin-api
        ]);
      in [
        hsPkgs
      ];
    };
  };
}
