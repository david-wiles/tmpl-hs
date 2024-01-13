let
  pkgs = import <nixpkgs> { };
  name = "tmpl";
  buildInputs = [ pkgs.haskell-language-server
                 (pkgs.haskellPackages.ghcWithPackages
                 (hsPkgs: with hsPkgs; []))
  ];
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
  }