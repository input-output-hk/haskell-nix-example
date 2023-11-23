{
  description = "A simple flake for building the hello package with haskell.nix";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # for caching you want to follow haskell.nix's nixpkgs-unstable pins.
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      # choose the compiler you want. For now we use ghc963.
      compiler-nix-name = "ghc963";
    in
    flake-utils.lib.eachDefaultSystem (system:
      let

        # This sets up the `pkgs`, by importing the nixpkgs flake and
        # adding the haskellNix overlay.
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay ];
          # Also ensure we are using haskellNix config. Otherwise we won't be
          # selecting the correct wine version for cross compilation.
          inherit (haskellNix) config;
        };

        # If we are building a haskell project (e.g. the current directory)
        # we'd use this to get the haskell packages from the current project.
        # This expects a `cabal.project` file to be present.
        hsPkgs = pkgs.haskell-nix.project' {
          inherit compiler-nix-name;
          src = ./.;
        };

        # for this simple demo, we'll just use a package from hackage. Namely the
        # trivial `hello` package. See https://hackage.haskell.org/package/hello
        helloPkg = pkgs.haskell-nix.hackage-package {
          inherit compiler-nix-name;
          name = "hello";
          version = "1.0.0.2";
        };
      in
      # This is the _very_ basic flake output. It just builds the hello package,
      # and has no cross logic, also set it to the default package.
      #
      # Thus `nix build` on will just build the `hello` package for the current
      # system. We can also build it explicitly with `nix build .#hello`.
      #
      # If we are on a system that supports multiple targets (e.g. Apple Silicon
      # with aarch64-darwin and x86_64-darwin set in `extra-platforms`; you can
      # check this with `nix show-config | grep extra-platforms`).
      #
      # Then we can build the package for both targets with
      # - nix build .#hello --system x86_64-darwin
      # - nix build .#hello --system aarch64-darwin
      # or explicitly
      # - nix build .#packages.x86_64-darwin.hello
      # - nix build .#packages.aarch64-darwin.hello
      let
        nativePackages = {
          packages.hello = helloPkg.components.exes.hello;
          defaultPackage = self.packages.${system}.hello;
        };
      # then, how do we get cross compiled packages?
        linuxCrossPackages = let
          # using aarch64-multiplatform-musl here, gives us fully static binaries.
          helloPkg-static-musl = pkgs.pkgsCross.aarch64-multiplatform-musl.haskell-nix.hackage-package {
            inherit compiler-nix-name;
            name = "hello";
            version = "1.0.0.2";
          };
          # if we wanted, we could also use aarch64-multiplatform here, which will
          # give us dynamically linked ones.
          helloPkg-dynamic = pkgs.pkgsCross.aarch64-multiplatform.haskell-nix.hackage-package {
            inherit compiler-nix-name;
            name = "hello";
            version = "1.0.0.2";
          };
          # we can also cross compile from linux to x86_64-windows.
          helloPkg-mingw = pkgs.pkgsCross.mingwW64.haskell-nix.hackage-package {
            inherit compiler-nix-name;
            name = "hello";
            version = "1.0.0.2";
          };
          # and there is also a ucrt64 windows version. But that's only available
          # from 9.4+.
          helloPkg-ucrt64 = pkgs.pkgsCross.ucrt64.haskell-nix.hackage-package {
            inherit compiler-nix-name;
            name = "hello";
            version = "1.0.0.2";
          };
          # Or javascript
          helloPkg-javascript = pkgs.pkgsCross.ghcjs.haskell-nix.hackage-package {
            inherit compiler-nix-name;
            name = "hello";
            version = "1.0.0.2";
          };
        # we can only cross compile x86_64-linux -> aarch64-linux.
        in pkgs.lib.optionalAttrs (system == "x86_64-linux") {
            packages.hello-static-musl = helloPkg-static-musl.components.exes.hello;
            packages.hello-dynamic = helloPkg-dynamic.components.exes.hello;
            packages.hello-mingw = helloPkg-mingw.components.exes.hello;
            packages.hello-ucrt64 = helloPkg-ucrt64.components.exes.hello;
            packages.hello-javascript = helloPkg-javascript.components.exes.hello;
        };
        # helper function to add `hydraJobs` to the flake output.
        addHydraJobs = pkgs: pkgs // { hydraJobs = pkgs.packages; };
      # turn them into a merged flake output.
      in addHydraJobs (pkgs.lib.recursiveUpdate nativePackages linuxCrossPackages)
    );
  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # use zw3rk and iog cache. zw3rk has the haskell.nix artifacts cached.
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = "true";
  };
}