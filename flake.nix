{
  description = "static nix-tools";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
      ];
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        # Per-system attributes can be defined here. The self' and inputs'
        # module parameters provide easy access to attributes of the same
        # system.

        # packages unpacks the relevant package for the current system.
        packages.nix-tools = let nix-tools = import ./default.nix pkgs; in pkgs.runCommand "nix-tools" {
          nativeBuildInputs = [ pkgs.unzip ];
        } ''
            mkdir -p $out/bin
            cd $out/bin
            ${pkgs.unzip}/bin/unzip ${nix-tools.${system}}
          '';

        # apps provide a shorthand for all the contained ones in the package.
        # WARN: update this whenever the set of packaged packages is updated in the main flake.nix
        apps = builtins.listToAttrs
          (map (name: { inherit name; value = { type = "app"; program = "${self'.packages.nix-tools}/bin/${name}"; }; })
           [ "cabal" "cabal-name" "cabal-to-nix" "hackage-to-nix" "hashes-to-nix" "hpack" "lts-to-nix" "make-install-plan" "plan-to-nix" "stack-repos" "stack-to-nix" "truncate-index" ]);
      };
    };
}
