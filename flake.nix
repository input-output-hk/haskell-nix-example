{
  description = "A simple flake for building the hello package with haskell.nix";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix?ref=angerman/fix-aarch64-musl";
    # for caching you want to follow haskell.nix's nixpkgs-unstable pins.
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    kupo.url = "github:CardanoSolutions/kupo";
    kupo.flake = false;

    ogmios.url = "github:CardanoSolutions/ogmios";
    ogmios.flake = false;

    hydra.url = "github:input-output-hk/hydra";
    hydra.flake = false;

    # kupo needs the crypto overlays from iohk-nix
    iohkNix.url = "github:input-output-hk/iohk-nix";
    # kupo also needs cardano-haskell-packages
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }@inputs:
    # choose the compiler you want. For now we use ghc963.
    let compiler-nix-name = "ghc963"; in
    let flake = flake-utils.lib.eachDefaultSystem (system:
      let

        # This sets up the `pkgs`, by importing the nixpkgs flake and
        # adding the haskellNix overlay.
        # We need the iohkNix overlays to get the necessary cryto packages.
        # secp256k1, blst, and libsodium.
        pkgs = import nixpkgs {
          inherit system;
          overlays = with inputs; [
            iohkNix.overlays.crypto
            haskellNix.overlay
            iohkNix.overlays.haskell-nix-extra
            iohkNix.overlays.haskell-nix-crypto
            iohkNix.overlays.cardano-lib
            iohkNix.overlays.utils
            (import ./packaging.nix)
            (final: prev: {
              static-libsodium-vrf = final.libsodium-vrf.overrideDerivation (old: {
                configureFlags = old.configureFlags ++ [ "--disable-shared" ];
              });
              static-secp256k1 = final.secp256k1.overrideDerivation (old: {
                configureFlags = old.configureFlags ++ ["--enable-static" "--disable-shared" ];
              });
              static-gmp = (final.gmp.override { withStatic = true; }).overrideDerivation (old: {
                configureFlags = old.configureFlags ++ ["--enable-static" "--disable-shared" ];
              });
              static-openssl = (final.openssl.override { static = true; });
              static-zlib = final.zlib.override { shared = false; };
              static-pcre = final.pcre.override { shared = false; };
            })
          ];
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

        # If we want to use a source-referenced flake we can do this as well
        kupoPkgs = pkgs: pkgs.haskell-nix.project' {
          # kupo builds with 8107
          compiler-nix-name = "ghc8107";
          # strip the package.yaml from the source. haskell.nix's tooling will
          # choke on this special one.
          src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
            name = "kupo-src";
            src = inputs.kupo;
            filter = path: type:
              builtins.all (x: x) [
                (baseNameOf path != "package.yaml")
              ];
          };
          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };
          sha256map = {
            "https://github.com/CardanoSolutions/direct-sqlite"."82c5ab46715ecd51901256144f1411b480e2cb8b" = "1r1g6nf65d9n436ppcjky3gkywpnx4y0a3v88ddngchmf8za3qky";
            "https://github.com/CardanoSolutions/text-ansi"."dd81fe6b30e78e95589b29fd1b7be1c18bd6e700" = "0c9ckqcl4lahmkkfhj95bwwj4l2w8hlw0429gi1yly2mbdb688cq";
          };
          modules = [{
            packages.double-conversion.ghcOptions = [
              # stop putting U __gxx_personality_v0 into the library!
              "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
              # stop putting U __cxa_guard_release into the library!
              "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
            ];
            packages.plutus-core.patches = [
              # This patch is needed to fix a build error on aarch64-linux.
              #
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl> plutus-core/src/PlutusCore/Evaluation/Machine/ExBudgetingDefaults.hs:67:6: error:
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>     • Exception when trying to run compile-time code:
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>         /build/plutus-core-1.5.0.1/plutus-core/src/PlutusCore/Evaluation/Machine: getDirectoryContents:openDirStream: invalid argument (Invalid argument)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>       Code: readJSONFromFile DFP.cekMachineCostsFile
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>     • In the Template Haskell splice
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>         $$(readJSONFromFile DFP.cekMachineCostsFile)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>       In the expression: $$(readJSONFromFile DFP.cekMachineCostsFile)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>       In an equation for ‘defaultCekMachineCosts’:
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>           defaultCekMachineCosts
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>             = $$(readJSONFromFile DFP.cekMachineCostsFile)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>    |
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl> 67 |   $$(readJSONFromFile DFP.cekMachineCostsFile)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>    |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
              (builtins.toFile "plutus-core.patch" ''
              diff --git a/plutus-core/src/Data/Aeson/THReader.hs b/plutus-core/src/Data/Aeson/THReader.hs
              index 4b812b3..5290f87 100644
              --- a/plutus-core/src/Data/Aeson/THReader.hs
              +++ b/plutus-core/src/Data/Aeson/THReader.hs
              @@ -5,10 +5,11 @@ import Data.Aeson
               import Language.Haskell.TH.Syntax
               import Language.Haskell.TH.Syntax.Compat
               import TH.RelativePaths
              +import qualified Data.ByteString.Lazy as LBS

               readJSONFromFile :: (FromJSON a, Lift a) => String -> SpliceQ a
               readJSONFromFile name = liftSplice $ do
              -    contents <- qReadFileLBS name
              +    contents <- qRunIO $ LBS.readFile name
                   case (eitherDecode contents) of
                       Left err  -> fail err
                       Right res -> examineSplice [||res||]
              '')
            ];
          }
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.kupo.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
            ];
          })
          # Fix compilation with newer ghc versions
          ({ lib, config, ... }:
            lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
            # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
            # to call out to all kinds of silly tools that GHC doesn't really provide.
            # For this reason, we try to get away without re-installing lib:ghc for now.
            reinstallableLibGhc = false;
          })
          ];
        };
        ogmiosPkgs = pkgs: pkgs.haskell-nix.project' {
          # kupo builds with 8107
          compiler-nix-name = "ghc8107";
          # strip the package.yaml from the source. haskell.nix's tooling will
          # choke on this special one.
          src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
            name = "ogmios-src";
            src = "${inputs.ogmios}/server";
            filter = path: type:
              builtins.all (x: x) [
                (baseNameOf path != "package.yaml")
              ];
          };
          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };
          modules = [{
            packages.double-conversion.ghcOptions = [
              # stop putting U __gxx_personality_v0 into the library!
              "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
              # stop putting U __cxa_guard_release into the library!
              "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
            ];
            packages.plutus-core.patches = [
              # This patch is needed to fix a build error on aarch64-linux.
              #
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl> plutus-core/src/PlutusCore/Evaluation/Machine/ExBudgetingDefaults.hs:67:6: error:
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>     • Exception when trying to run compile-time code:
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>         /build/plutus-core-1.5.0.1/plutus-core/src/PlutusCore/Evaluation/Machine: getDirectoryContents:openDirStream: invalid argument (Invalid argument)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>       Code: readJSONFromFile DFP.cekMachineCostsFile
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>     • In the Template Haskell splice
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>         $$(readJSONFromFile DFP.cekMachineCostsFile)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>       In the expression: $$(readJSONFromFile DFP.cekMachineCostsFile)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>       In an equation for ‘defaultCekMachineCosts’:
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>           defaultCekMachineCosts
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>             = $$(readJSONFromFile DFP.cekMachineCostsFile)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>    |
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl> 67 |   $$(readJSONFromFile DFP.cekMachineCostsFile)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>    |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
              (builtins.toFile "plutus-core.patch" ''
              diff --git a/plutus-core/src/Data/Aeson/THReader.hs b/plutus-core/src/Data/Aeson/THReader.hs
              index 4b812b3..5290f87 100644
              --- a/plutus-core/src/Data/Aeson/THReader.hs
              +++ b/plutus-core/src/Data/Aeson/THReader.hs
              @@ -5,10 +5,11 @@ import Data.Aeson
               import Language.Haskell.TH.Syntax
               import Language.Haskell.TH.Syntax.Compat
               import TH.RelativePaths
              +import qualified Data.ByteString.Lazy as LBS

               readJSONFromFile :: (FromJSON a, Lift a) => String -> SpliceQ a
               readJSONFromFile name = liftSplice $ do
              -    contents <- qReadFileLBS name
              +    contents <- qRunIO $ LBS.readFile name
                   case (eitherDecode contents) of
                       Left err  -> fail err
                       Right res -> examineSplice [||res||]
              '')
            ];
          }
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.kupo.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
            ];
          })];
        };
        hydraPkgs = pkgs: pkgs.haskell-nix.project' {
          compiler-nix-name = "ghc963";
          src = inputs.hydra;

          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };
          modules = [{
            packages.double-conversion.ghcOptions = [
              # stop putting U __gxx_personality_v0 into the library!
              "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
              # stop putting U __cxa_guard_release into the library!
              "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
            ];
            packages.plutus-core.patches = [
              # This patch is needed to fix a build error on aarch64-linux.
              #
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl> plutus-core/src/PlutusCore/Evaluation/Machine/ExBudgetingDefaults.hs:67:6: error:
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>     • Exception when trying to run compile-time code:
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>         /build/plutus-core-1.5.0.1/plutus-core/src/PlutusCore/Evaluation/Machine: getDirectoryContents:openDirStream: invalid argument (Invalid argument)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>       Code: readJSONFromFile DFP.cekMachineCostsFile
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>     • In the Template Haskell splice
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>         $$(readJSONFromFile DFP.cekMachineCostsFile)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>       In the expression: $$(readJSONFromFile DFP.cekMachineCostsFile)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>       In an equation for ‘defaultCekMachineCosts’:
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>           defaultCekMachineCosts
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>             = $$(readJSONFromFile DFP.cekMachineCostsFile)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>    |
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl> 67 |   $$(readJSONFromFile DFP.cekMachineCostsFile)
              # plutus-core-lib-plutus-core-aarch64-unknown-linux-musl>    |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
              (builtins.toFile "plutus-core.patch" ''
              diff --git a/plutus-core/src/Data/Aeson/THReader.hs b/plutus-core/src/Data/Aeson/THReader.hs
              index 4b812b3..5290f87 100644
              --- a/plutus-core/src/Data/Aeson/THReader.hs
              +++ b/plutus-core/src/Data/Aeson/THReader.hs
              @@ -5,10 +5,11 @@ import Data.Aeson
               import Language.Haskell.TH.Syntax
               import Language.Haskell.TH.Syntax.Compat
               import TH.RelativePaths
              +import qualified Data.ByteString.Lazy as LBS

               readJSONFromFile :: (FromJSON a, Lift a) => String -> SpliceQ a
               readJSONFromFile name = liftSplice $ do
              -    contents <- qReadFileLBS name
              +    contents <- qRunIO $ LBS.readFile name
                   case (eitherDecode contents) of
                       Left err  -> fail err
                       Right res -> examineSplice [||res||]
              '')
            ];
          }
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.kupo.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
            ];
          })];
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
        kupoPackages.packages = {
          kupo-native            = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-kupo";                                             } (kupoPkgs pkgs                                     ).hsPkgs.kupo.components.exes.kupo;
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          kupo-static-musl       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-kupo-static";                     } (kupoPkgs pkgs.pkgsCross.musl64                    ).hsPkgs.kupo.components.exes.kupo;
          kupo-static-musl-arm64 = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-kupo-static"; } (kupoPkgs pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.kupo.components.exes.kupo;
          kupo-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-kupo";             } (kupoPkgs pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.kupo.components.exes.kupo;

          # kupo requires the unix package, so we can't build mingwW64 or ucrt64 really.
          # kupo-mingw       = (kupoPkgs pkgs.pkgsCross.mingwW64                  ).hsPkgs.kupo.components.exes.kupo;
          # however if it didn't, this could be built with 9.4+
          # kupo-ucrt64 = (kupoPkgs pkgs.pkgsCross.ucrt64).hsPkgs.kupo.components.exes.kupo;

          # and this likely won't work at all due to all the c level dependencies; also we'd want to use 9.6+
          # kupo-javascript = (kupoPkgs pkgs.pkgsCross.ghcjs).hsPkgs.kupo.components.exes.kupo;
        };

        # Ogmios
        ogmiosPackages.packages = {
          ogmios-native       = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-ogmios";                                                  } (ogmiosPkgs pkgs                                     ).hsPkgs.ogmios.components.exes.ogmios;
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          ogmios-static-musl       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-ogmios-static";                     } (ogmiosPkgs pkgs.pkgsCross.musl64                    ).hsPkgs.ogmios.components.exes.ogmios;
          ogmios-static-musl-arm64 = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-ogmios-static"; } (ogmiosPkgs pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.ogmios.components.exes.ogmios;
          ogmios-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-ogmios";             } (ogmiosPkgs pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.ogmios.components.exes.ogmios;
        };

        hydraPackages.packages = {
          hydra-native       = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-hydra-node";                                                  } (hydraPkgs pkgs                                     ).hsPkgs.hydra-node.components.exes.hydra-node;
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          hydra-static-musl       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-hydra-node-static";                     } (hydraPkgs pkgs.pkgsCross.musl64                    ).hsPkgs.hydra-node.components.exes.hydra-node;
          hydra-static-musl-arm64 = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-hydra-node-static"; } (hydraPkgs pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.hydra-node.components.exes.hydra-node;
          hydra-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-hydra-node";             } (hydraPkgs pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.hydra-node.components.exes.hydra-node;
        };

        # helper function to add `hydraJobs` to the flake output.
        addHydraJobs = pkgs: pkgs // { hydraJobs = pkgs.packages; };
      # turn them into a merged flake output.
      in addHydraJobs (pkgs.lib.recursiveUpdate
                        (pkgs.lib.recursiveUpdate
                         (pkgs.lib.recursiveUpdate
                          (pkgs.lib.recursiveUpdate nativePackages linuxCrossPackages) kupoPackages) ogmiosPackages) hydraPackages)
    ); in with (import nixpkgs { system = "x86_64-linux"; overlays = [(import ./download.nix)]; }); lib.recursiveUpdate flake { hydraJobs.index = hydra-utils.mkIndex flake; };
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