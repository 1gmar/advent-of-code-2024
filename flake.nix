{
  description = "Advent of Code 2024";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    parent = {
      url = "/home/igmar/nixos";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = {
    self,
    parent,
    nixvim,
    nixpkgs,
    ...
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
  in {
    packages.${system}.neovim = nixvim.legacyPackages.${system}.makeNixvimWithModule {
      inherit pkgs;
      module = {
        imports = [parent.nixosModules.neovim];
        plugins = {
          lsp.servers.prolog_ls = {
            enable = true;
            cmd = [
              "swipl"
              "-g"
              "use_module('/home/igmar/code/advent-of-code-2024/packs/lsp_server/prolog/lsp_server')."
              "-g"
              "lsp_server:main"
              "-t"
              "halt"
              "--"
              "stdio"
            ];
            filetypes = ["prolog"];
            package = null;
            rootDir = "require('lspconfig/util').root_pattern(\"pack.pl\")";
          };
          treesitter.grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
            prolog
            nix
          ];
        };
      };
    };
    devShells.${system}.default = pkgs.mkShell {
      packages = [pkgs.swi-prolog self.packages.${system}.neovim];
    };
  };
}
