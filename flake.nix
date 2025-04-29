{
  description = "Advent of Code 2024";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixvim-1gmar = {
      url = "github:1gmar/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = {
    self,
    nixvim-1gmar,
    nixpkgs,
    ...
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
  in {
    packages.${system}.neovim = nixvim-1gmar.legacyPackages.${system}.makeNixvimWithModule {
      inherit pkgs;
      module = {
        filetype.extension = {pl = "prolog";};
        imports = [nixvim-1gmar.nixvimModule];
        plugins = {
          lsp.servers.prolog_ls = {
            enable = true;
            cmd = [
              "swipl"
              "-f"
              "./defaults.pl"
              "-g"
              "use_module('./packs/lsp_server/prolog/lsp_server.pl')."
              "-g"
              "lsp_server:main"
              "-t"
              "halt"
              "--"
              "stdio"
            ];
            filetypes = ["prolog"];
            package = null;
            rootMarkers = [".git"];
          };
          treesitter.grammarPackages = pkgs.lib.mkForce (
            with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
              prolog
              nix
            ]
          );
        };
      };
    };
    devShells.${system}.default = pkgs.mkShell {
      packages = [pkgs.swi-prolog self.packages.${system}.neovim];
    };
  };
}
