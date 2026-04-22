{
  description = "A simple flake for building the hello package with haskell.nix";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskellNix.inputs.hackage.follows = "hackageNix";

    iserv-proxy = {
      url = "github:stable-haskell/iserv-proxy?ref=iserv-syms";
      flake = false;
    };
    haskellNix.inputs.iserv-proxy.follows = "iserv-proxy";
    # Pin nixpkgs independently — haskell.nix's nixpkgs-unstable dropped
    # ghc943 which breaks the bootstrap overlay.
    nixpkgs.url = "github:NixOS/nixpkgs/647e5c14cbd5067f44ac86b74f014962df460840";

    kupo.url = "github:CardanoSolutions/kupo?ref=v2.7";
    kupo.flake = false;

    ogmios.url = "github:CardanoSolutions/ogmios?rev=199daf67062e7c9efa735a0ba7d80d49108a56a0";
    ogmios.flake = false;

    hydra.url = "github:input-output-hk/hydra?rev=899aefd341789ac0fbcf8c22a9bdbfcc83329b5b";
    hydra.flake = false;

    db-sync.url = "github:input-output-hk/cardano-db-sync?ref=13.1.0.2";
    db-sync.flake = false;

    encoins.url = "github:encryptedcoins/encoins-relay";
    encoins.flake = false;

    cardano-node.url = "github:IntersectMBO/cardano-node?ref=10.6.4";
    cardano-node.flake = false;

    cardano-node-pre.url = "github:IntersectMBO/cardano-node?ref=10.7.1";
    cardano-node-pre.flake = false;

    # Pruned ImmutableDB variant (cardano-node + ouroboros-consensus with pruning support)
    pruned-cardano-node.url = "github:angerman/cardano-node?ref=angerman/pruned-mode-mithril";
    pruned-cardano-node.flake = false;

    pruned-ouroboros-consensus.url = "github:angerman/ouroboros-consensus?ref=angerman/pruned-immutabledb";
    pruned-ouroboros-consensus.flake = false;

    cardano-cli.url = "github:IntersectMBO/cardano-cli?ref=cardano-cli-10.15.1.0";
    cardano-cli.flake = false;

    cardano-addresses.url = "github:IntersectMBO/cardano-addresses?ref=4.0.0";
    cardano-addresses.flake = false;

    bech32.url = "github:IntersectMBO/bech32?ref=v1.1.7";
    bech32.flake = false;

    nix-tools.url = "github:input-output-hk/haskell.nix?dir=nix-tools";
    nix-tools.flake = false;

    cabal-install.url = "github:erikd/cabal?ref=erikd/pre-post-build-hooks";
    cabal-install.flake = false;

    # rust stuff
    crane.url = "github:ipetkov/crane";
    crane.inputs.nixpkgs.follows = "nixpkgs";

    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.inputs.flake-utils.follows = "flake-utils";

    mithril.url = "github:input-output-hk/mithril?ref=2524.0";
    mithril.flake = false;

    # kupo needs the crypto overlays from iohk-nix
    iohkNix.url = "github:input-output-hk/iohk-nix";
    # kupo also needs cardano-haskell-packages
    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, haskellNix, ... }@inputs:
    # choose the compiler you want. For now we use ghc964.
    let compiler-nix-name = "ghc966"; in
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
              # kyua signal-handling tests are broken inside the Nix sandbox on
              # aarch64-darwin (SIGTERM delivered as SIGABRT, handlers never fire).
              # atf depends on kyua for its test suite, and libiconv depends on atf,
              # so the failure cascades through the entire darwin stdenv.  Disable
              # atf's tests to break the cascade.
              atf = prev.atf.overrideAttrs { doCheck = false; };

              static-libsodium-vrf = final.libsodium-vrf.overrideDerivation (old: {
                configureFlags = old.configureFlags ++ [ "--disable-shared" ];
              });
              static-secp256k1 = final.secp256k1.overrideDerivation (old: {
                configureFlags = old.configureFlags ++ ["--enable-static" "--disable-shared" ];
              });
              dyn-static-secp256k1 = final.secp256k1.overrideDerivation (old: {
                configureFlags = old.configureFlags ++ ["--enable-static" "--enable-shared" ];
              });
              static-gmp = (final.gmp.override { withStatic = true; }).overrideDerivation (old: {
                configureFlags = old.configureFlags ++ ["--enable-static" "--disable-shared" ];
              });
              static-postgresql = (final.postgresql.override { gssSupport = false; }).overrideDerivation (old: {
                # ensure we don't drop static libraries.
                dontDisableStatic = true;
                postInstall = old.postInstall + ''
                  # drop all dynamic stuff.
                  rm -fR $out/lib/*.dylib
                  rm -fR $lib/lib/*.dylib
                '';
              });
              # libcxxabi was merged into libcxx in recent nixpkgs
              static-libcxxabi = if final ? libcxxabi
                then (final.libcxxabi.override { enableShared = false; })
                else final.libcxx;
              static-libblst = (final.libblst.override { enableShared = false; }).overrideDerivation (old: {
                postFixup = "";
              });
              static-openssl = (final.openssl.override { static = true; });
              static-ncurses = final.ncurses.override { enableStatic = true; };
              static-snappy = final.snappy.overrideAttrs (old: {
                cmakeFlags = (builtins.filter (f: f != "-DBUILD_SHARED_LIBS=ON") (old.cmakeFlags or [])) ++ [
                  "-DBUILD_SHARED_LIBS=OFF"
                ];
              });
              static-zlib = final.zlib.override { shared = false; };
              static-pcre = final.pcre.override { shared = false; };
              static-lmdb = final.lmdb.overrideDerivation (old: {
                postInstall = old.postInstall + ''
                  # drop all dynamic stuff.
                  rm -fR $out/lib/*.so
                  rm -fR $lib/lib/*.so
                '';
              });
            })
          ];
          # Also ensure we are using haskellNix config. Otherwise we won't be
          # selecting the correct wine version for cross compilation.
          inherit (haskellNix) config;
        };
        rsPkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import rust-overlay)
            (final: prev: {
              static-openssl = (final.openssl.override { static = true; });
            })
          ];
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
          compiler-nix-name = "ghc963";
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
          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
                       "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
                       "https://chap.intersectmbo.org" = inputs.CHaP;
                      };
          sha256map = {
            "https://github.com/CardanoSolutions/ogmios"."01f7787216e7ceb8e39c8c6807f7ae53fc14ab9e" = "16mdvv3qws92fins673jb2qiw7b9vd0pwzwvq9zvi6zkhhhkfdfm";
            "https://github.com/CardanoSolutions/direct-sqlite"."82c5ab46715ecd51901256144f1411b480e2cb8b" = "1r1g6nf65d9n436ppcjky3gkywpnx4y0a3v88ddngchmf8za3qky";
            "https://github.com/CardanoSolutions/text-ansi"."dd81fe6b30e78e95589b29fd1b7be1c18bd6e700" = "0c9ckqcl4lahmkkfhj95bwwj4l2w8hlw0429gi1yly2mbdb688cq";
            "https://github.com/CardanoSolutions/text-ansi"."e204822d2f343b2d393170a2ec46ee935571345c" = "16ki7wsf7wivxn65acv4hxwfrzmphq4zp61lpxwzqkgrg8shi8bv";
          };
          modules = [
          # {
          #   packages.double-conversion.ghcOptions = [
          #     # stop putting U __gxx_personality_v0 into the library!
          #     "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
          #     # stop putting U __cxa_guard_release into the library!
          #     "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
          #   ];
          # }
          (pkgs.lib.mkIf (pkgs.hostPlatform.isMusl && pkgs.hostPlatform.isAarch64) {
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
          })
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.kupo.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
            ];
          })
          ];
        };
        ogmiosPkgs = pkgs: pkgs.haskell-nix.project' {
          # ogmios builds with 8107
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
          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
          };
          modules = [{
            # packages.double-conversion.ghcOptions = [
            #   # stop putting U __gxx_personality_v0 into the library!
            #   "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
            #   # stop putting U __cxa_guard_release into the library!
            #   "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
            # ];
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
          compiler-nix-name = "ghc966";
          src = inputs.hydra;

          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
          };
          modules = [
          # proto-lens packages need protoc at build time (matches
          # upstream hydra's nix/hydra/project.nix).
          ({ pkgs, ... }: {
            packages.proto-lens-protobuf-types.components.library.build-tools =
              [ pkgs.buildPackages.protobuf ];
            packages.proto-lens-etcd.components.library.build-tools =
              [ pkgs.buildPackages.protobuf ];
          })
          {
            # packages.double-conversion.ghcOptions = [
            #   # stop putting U __gxx_personality_v0 into the library!
            #   "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
            #   # stop putting U __cxa_guard_release into the library!
            #   "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
            # ];
            packages.gitrev.patches = [
              (builtins.toFile "gitrev.patch" ''
              diff --git a/src/Development/GitRev.hs b/src/Development/GitRev.hs
              index b664692..603ad1b 100644
              --- a/src/Development/GitRev.hs
              +++ b/src/Development/GitRev.hs
              @@ -62,7 +62,9 @@ runGit :: [String] -> String -> IndexUsed -> Q String
               runGit args def useIdx = do
                 let oops :: SomeException -> IO (ExitCode, String, String)
                     oops _e = return (ExitFailure 1, def, "")
              +      none :: SomeException -> IO (Maybe FilePath)
              +      none _e = return Nothing
              -  gitFound <- runIO $ isJust <$> findExecutable "git"
              +  gitFound <- runIO $ isJust <$> findExecutable "git" `catch` none
                 if gitFound
                   then do
                     -- a lot of bookkeeping to record the right dependencies
              '')
            ];
          }
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.hydra-node.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
            ];
          })
          (pkgs.lib.mkIf (pkgs.hostPlatform.isMusl && pkgs.hostPlatform.isAarch64) {
            packages.plutus-tx.patches = [
              (builtins.toFile "plutus-tx.patch" ''
              From 895a8a4af848ec29f9165fbff585f391d2c3358b Mon Sep 17 00:00:00 2001
              From: Moritz Angermann <moritz.angermann@gmail.com>
              Date: Sun, 10 Dec 2023 16:23:24 +0800
              Subject: [PATCH] Update TH.hs

              Just don't force load it.
              ---
              src/PlutusTx/TH.hs | 1 -
              1 file changed, 1 deletion(-)

              diff --git a/src/PlutusTx/TH.hs b/src/PlutusTx/TH.hs
              index 49f26f6585e..02a0f927dd5 100644
              --- a/src/PlutusTx/TH.hs
              +++ b/src/PlutusTx/TH.hs
              @@ -46,7 +46,6 @@ going to typecheck, and the result is always a 'CompiledCode', so that's also fi
               -- | Compile a quoted Haskell expression into a corresponding Plutus Core program.
               compileUntyped :: TH.Q TH.Exp -> TH.Q TH.Exp
               compileUntyped e = do
              -    TH.addCorePlugin "PlutusTx.Plugin"
                   loc <- TH.location
                   let locStr = TH.pprint loc
                   -- See note [Typed TH]
              '')
            ];
          })
          (pkgs.lib.mkIf (pkgs.hostPlatform.isMusl && pkgs.hostPlatform.isAarch64) {
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
          })
          # Fix compilation with newer ghc versions
          ({ lib, config, ... }:
            lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
            # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
            # to call out to all kinds of silly tools that GHC doesn't really provide.
            # For this reason, we try to get away without re-installing lib:ghc for now.
            reinstallableLibGhc = false;
          })
          ({ lib, pkgs, ... }: {
            # This is primarily here, so that we can _force_ static/also dependencies, which then hopefully get rolled into the
            # -staticlib of the plutus-tx-plugin 😓.
            packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
            packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.dyn-static-secp256k1 pkgs.libblst ] ];
          })
          ];
        };
        dbSyncPkg = compiler-nix-name: pkgs: pkgs.haskell-nix.project' {
          inherit compiler-nix-name;
          src = inputs.db-sync;

          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
          };
          modules = [
            {
              # db-sync annoyingly has symlinks to the schema files, which are in ../schema,
              # which in turn are stripped by nix away :-/
              #
              # TODO: Maybe we should just copy them over from ${src}/schema into schema in the postUnpack phase?
              #       Alternative: maybe I can _materialize_ the src? (e.g. resolve any relative symlinks by duplicating them)
              packages.cardano-db-sync.package.extraSrcFiles =
                [ "../schema/*.sql" ];
              packages.cardano-db.package.extraSrcFiles =
                ["../config/pgpass-testnet"];
            }
            ({
              # packages.double-conversion.ghcOptions = [
              #   # stop putting U __gxx_personality_v0 into the library!
              #   "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
              #   # stop putting U __cxa_guard_release into the library!
              #   "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
              # ];
              # Just say no to systemd.
              # packages.cardano-config.flags.systemd = false;
              packages.cardano-node.flags.systemd = false;
            })
            ({
              packages.cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              # TODO: why is this necessary? This shouldn't be the default.
              #       does db-sync set this flag?
              packages.cardano-crypto-praos.flags.external-libsodium-vrf = true;
            })
            # Fix compilation with newer ghc versions
            ({ lib, config, ... }:
              lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
              # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
              # to call out to all kinds of silly tools that GHC doesn't really provide.
              # For this reason, we try to get away without re-installing lib:ghc for now.
              reinstallableLibGhc = false;
            })
            (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
              packages.cardano-db-sync.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
                "-L${__trace "${static-postgresql}/lib ${lib.getLib static-postgresql}/lib" (lib.getLib static-postgresql)}/lib"
                "-L${static-postgresql}/lib" "-optl-Wl,-lpgport" "-optl-Wl,-lpgcommon"
              ];
            })
          (pkgs.lib.mkIf (pkgs.hostPlatform.isMusl && pkgs.hostPlatform.isAarch64) {
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
          })
            # ok, so postgresql for static is pretty broken in nixpkgs.
            # The only way I could see "fix" it was to disable multiple outputs,
            # and patch a configure flag, which pointed to $(lib).
            #
            # The below hack is a bit less invasive, keeps the separate outputs,
            # but adds $out/lib of postgresql to the search path, as well as adding
            # libpgport and libpgcommon to the libraries we depend on.
            #
            # TODO: Figure out how to fix postgresql in upstream nixpkgs for musl
            #       properly to include _all_ (including pgport and pgcommon) libraries
            #       in $lib. We should also check why the static version of postgresql
            #       as per the pkg-config script does _not_ include -lpgport -lpgcommon.
            (pkgs.lib.mkIf pkgs.hostPlatform.isMusl {
              packages.cardano-db-sync.ghcOptions = with pkgs; [
                # Add pgport and pgcommon as link dependencies.
                "-L${pkgs.postgresql}/lib" "-optl-Wl,-lpgport" "-optl-Wl,-lpgcommon"
              ];
            })
          ];
        };
        nixToolsPkg = pkgs: pkgs.haskell-nix.project' {
          # cabalProjectLocal = ''
          # allow-newer: *:base, *:ghc-prim, *:template-haskell
          # '';
          compiler-nix-name = "ghc928";
          # subdir option, that retains the inputs logic.
          src = let subdir = input: path: input // { outPath = "${input}/${path}"; };
                in subdir inputs.nix-tools "nix-tools";

          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
          };
          modules = [
          #   ({
          #   packages.double-conversion.ghcOptions = [
          #     # stop putting U __gxx_personality_v0 into the library!
          #     "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
          #     # stop putting U __cxa_guard_release into the library!
          #     "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
          #   ];
          # })

          # just say no to this gitXXX shit
          { packages.hnix.patches = [
            (builtins.toFile "plutus-core.patch" ''
diff --git a/src/Nix/Options/Parser.hs b/src/Nix/Options/Parser.hs
index 3aeb0e5..bea0ac9 100644
--- a/src/Nix/Options/Parser.hs
+++ b/src/Nix/Options/Parser.hs
@@ -214,11 +214,7 @@ versionOpt = shortVersionOpt <*> debugVersionOpt
   debugVersionOpt =
     infoOption
       ( fold
-          [ "Version: ", showVersion version
-          , "\nCommit: ", $(gitHash)
-          , "\n  date: ", $(gitCommitDate)
-          , "\n  branch: ", $(gitBranch)
-          ]
+          [ "Version: ", showVersion version ]
       )
       (  long "long-version"
       <> help "Show long debug version form"

'')
          ];
          }
          # Fix compilation with newer ghc versions
          ({ lib, config, ... }:
            lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
            # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
            # to call out to all kinds of silly tools that GHC doesn't really provide.
            # For this reason, we try to get away without re-installing lib:ghc for now.
            reinstallableLibGhc = false;
          })
          ({
            # This is stupid. We should be able to set dontStrip globally.  The
            # fact that we can't inherit is bullshit.
            packages.nix-tools.components.exes = {
              cabal-name.dontStrip = false;
              cabal-to-nix.dontStrip = false;
              hackage-to-nix.dontStrip = false;
              hashes-to-nix.dontStrip = false;
              lts-to-nix.dontStrip = false;
              make-install-plan.dontStrip = false;
              plan-to-nix.dontStrip = false;
              stack-repos.dontStrip = false;
            };
          })
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.cabal-install.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
            ];
            packages.hpack.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
            ];
            packages.nix-tools.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
            ];
          })
          ];
        };

        encoinsPkg = pkgs: pkgs.haskell-nix.project' {
          cabalProjectLocal = ''
          package postgres-libpq
            flags: +use-pkg-config
          '';
          compiler-nix-name = "ghc8107";
          src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
            name = "encoins-src";
            src = inputs.encoins;
          };

          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
          };

          sha256map = {
            "https://github.com/encryptedcoins/cardano-server"."eae2d8293162bb399e8136bd5b4e54f8fd5488d3" = "0af98y93cv8kw5jl05vy4nm12sh6lhwm4lcnj4wyvyyyjly4jw9k";
            "https://github.com/encryptedcoins/cardano-server"."0b5cb40a96c2ee15411706eae5c4354e7acf0324" = "0wnzd54q1z5a4w5cy0q1anyvzcmd1yg22wj6amjv2raqshvzwndj";
            "https://github.com/encryptedcoins/plutus-tx-extra.git"."8c54d7f687fcf49011d7aa0961d99fc88019797e" = "04h8spwqjdni860i995gfxiw5jrzkrw49dcj9l1dghgwsyg5vpiq";
            "https://github.com/encryptedcoins/plutus-apps-extra"."0cbc7f4ae053765ad006c178132820df9e3c4f06" = "02fzf43kkyfi4p0aqsp70v9c19pr482zn436dqxfg76fvd280i3l";
            "https://github.com/encryptedcoins/csl-types.git"."0587b018a1cd905c129cc1420b7a2027ee988e8e" = "0prr6941z3a78fvvk8h2d5bxag5jk2w1gc0hqszid2zm7zmlvi2p";
            "https://github.com/encryptedcoins/encoins-bulletproofs"."f62ea3caf1490df449474b5e87577e037206c546" = "1c5s6dw06gs1p4ixa1wnzy815zaakvjmc1yxm5rmm4p6lj9j58df";
            "https://github.com/encryptedcoins/encoins-core.git"."d89301187aecadb0274dbb8cdd8f5d93a7287130" = "0dah8b0gpzxax19d27vmdja81xyl9f5fsdgpw8w3hxfml1zmjacz";
            "https://github.com/input-output-hk/plutus-apps"."68efca7eda4afd1c14698adf697d70acb5a489e2" = "1ng8h0njfc4lrrvlpd6a05019ap79h861nn5zx3rnhg59i8j2wjm";
            "https://github.com/input-output-hk/cardano-wallet"."18a931648550246695c790578d4a55ee2f10463e" = "0i40hp1mdbljjcj4pn3n6zahblkb2jmpm8l4wnb36bya1pzf66fx";
            "https://github.com/Quviq/quickcheck-contractmodel"."cc43f13f98c704e0d53dbdef6a98367918f8c5c1" = "1ya604zn6jaq2qis1qgrlc21q08a7qmwwaj19f9m86y6dmvq5gk5";
            "https://github.com/input-output-hk/cardano-addresses"."b7273a5d3c21f1a003595ebf1e1f79c28cd72513" = "129r5kyiw10n2021bkdvnr270aiiwyq58h472d151ph0r7wpslgp";
            "https://github.com/input-output-hk/cardano-ledger"."da3e9ae10cf9ef0b805a046c84745f06643583c2" = "1jg1h05gcms119mw7fz798xpj3hr5h426ga934vixmgf88m1jmfx";
          };
          modules = [({ lib, ...}: {
            packages.postgresql-libpq.flags.use-pkg-config = true;
            # work around the sodium-vrf inlining via custom setups. Sigh, this
            # is bad.
            packages.encoins-relay-apps.package.buildType = lib.mkForce "Simple";
            packages.cardano-server.package.buildType = lib.mkForce "Simple";
            packages.encoins-relay-verifier.package.buildType = lib.mkForce "Simple";
            packages.encoins-relay-server.package.buildType = lib.mkForce "Simple";

          })
          (pkgs.lib.mkIf (!pkgs.hostPlatform.isx86_64) {
            # scrypt only available on x86_64, due to use of SSE2.
            packages.cardano-wallet-core.flags.scrypt = false;
          })
          {
            reinstallableLibGhc = false;
          }
          ];
        };
        # Fetch proto-lens with submodules and fix symlinks (same workaround
        # as cardano-node upstream — the CHaP-embedded hash is stale).
        protoLensSrc = pkgs.fetchgit {
          url = "https://github.com/google/proto-lens";
          rev = "20de5227947b0c37dd6852dcc6f2db1cd5889cee";
          sha256 = "sha256-VUYU2swjU7L8Zdu6Zfz6jo2ulW5uPhAamt2GjH5hZRY=";
          fetchSubmodules = true;
        };
        fixProtoLensSrc = pkgs.runCommand "proto-lens-fixed" {} ''
          mkdir -p $out
          cp -a ${protoLensSrc}/. $out/
          chmod -R +w $out
          rm -rf $out/proto-lens/proto-lens-imports/google
          cp -r ${protoLensSrc}/google/protobuf/src/google $out/proto-lens/proto-lens-imports/
          rm -rf $out/proto-lens-protobuf-types/proto-src
          cp -r ${protoLensSrc}/google/protobuf/src $out/proto-lens-protobuf-types/proto-src
          chmod -R -w $out
        '';

        cardanoNodePkg = pkgs: pkgs.haskell-nix.project' {
          compiler-nix-name = "ghc966";
          src = inputs.cardano-node;

          cabalProjectLocal = ''
          package cardano-node
            flags: -systemd
          package cardano-tracer
            flags: -systemd
          '';

          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://chap.intersectmbo.org/" = inputs.CHaP;
            "https://github.com/google/proto-lens/20de5227947b0c37dd6852dcc6f2db1cd5889cee" = fixProtoLensSrc;
          };
          modules = [({
            # packages.double-conversion.ghcOptions = [
            #   # stop putting U __gxx_personality_v0 into the library!
            #   "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
            #   # stop putting U __cxa_guard_release into the library!
            #   "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
            # ];
            # Just say no to systemd.
            # packages.cardano-config.flags.systemd = false;
            packages.cardano-node.flags.systemd = false;
            packages.cardano-tracer.flags.systemd = false;
          })
          # Fix compilation with newer ghc versions
          ({ lib, config, ... }:
            lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
            # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
            # to call out to all kinds of silly tools that GHC doesn't really provide.
            # For this reason, we try to get away without re-installing lib:ghc for now.
            reinstallableLibGhc = false;
          })
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.cardano-node.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
                "-L${lib.getLib static-lmdb}/lib"
            ];
            packages.cardano-cli.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
                "-L${lib.getLib static-lmdb}/lib"
                "-L${lib.getLib static-ncurses}/lib"
            ];
            packages.cardano-submit-api.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
                "-L${lib.getLib static-lmdb}/lib"
            ];
          })
          ];
        };

        # cardano-node 10.7.0 pre-release
        cardanoNodePrePkg = pkgs: pkgs.haskell-nix.project' {
          compiler-nix-name = "ghc966";
          src = inputs.cardano-node-pre;

          cabalProjectLocal = ''
          package cardano-node
            flags: -systemd
          package cardano-tracer
            flags: -systemd
          constraints: QuickCheck < 2.17
          '';

          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://chap.intersectmbo.org/" = inputs.CHaP;
            "https://github.com/google/proto-lens/20de5227947b0c37dd6852dcc6f2db1cd5889cee" = fixProtoLensSrc;
          };
          modules = [
          # proto-lens-protobuf-types needs protoc at build time (matches
          # upstream cardano-node's nix/haskell.nix).
          ({ pkgs, ... }: {
            packages.proto-lens-protobuf-types.components.library.build-tools =
              [ pkgs.buildPackages.protobuf ];
          })
          ({
            packages.cardano-node.flags.systemd = false;
            packages.cardano-tracer.flags.systemd = false;
          })
          ({ lib, config, ... }:
            lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
            reinstallableLibGhc = false;
          })
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.cardano-node.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
                "-L${lib.getLib static-lmdb}/lib"
                "-L${lib.getLib static-snappy}/lib"
            ];
            packages.cardano-cli.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
                "-L${lib.getLib static-lmdb}/lib"
                "-L${lib.getLib static-ncurses}/lib"
            ];
            packages.cardano-submit-api.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
                "-L${lib.getLib static-lmdb}/lib"
            ];
          })
          ];
        };

        # Pruned ImmutableDB variant of cardano-node.
        # Uses angerman/cardano-node (pruned-mode-mithril) as source, and overrides
        # ouroboros-consensus with the pruned-immutabledb branch via source-repository-package.
        prunedCardanoNodePkg = pkgs: pkgs.haskell-nix.project' {
          compiler-nix-name = "ghc966";
          src = inputs.pruned-cardano-node;

          cabalProjectLocal = ''
          package cardano-node
            flags: -systemd
          package cardano-tracer
            flags: -systemd

          -- Override ouroboros-consensus with pruned ImmutableDB support
          source-repository-package
            type: git
            location: https://github.com/angerman/ouroboros-consensus
            tag: ${inputs.pruned-ouroboros-consensus.rev}
            subdir:
              ouroboros-consensus
              ouroboros-consensus-cardano
              ouroboros-consensus-diffusion
              ouroboros-consensus-protocol
              sop-extras
              strict-sop-core
          '';

          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://chap.intersectmbo.org/" = inputs.CHaP;
            # Map ouroboros-consensus directly to the flake input to avoid
            # haskell.nix creating a fetchgit derivation whose name (URL + rev
            # + 6 subdirs) exceeds the 207-char Nix limit.
            "https://github.com/angerman/ouroboros-consensus" = inputs.pruned-ouroboros-consensus;
          };

          modules = [({
            packages.cardano-node.flags.systemd = false;
            packages.cardano-tracer.flags.systemd = false;
          })
          ({ lib, config, ... }:
            lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
            reinstallableLibGhc = false;
          })
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.cardano-node.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
                "-L${lib.getLib static-lmdb}/lib"
            ];
            packages.cardano-submit-api.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
                "-L${lib.getLib static-lmdb}/lib"
            ];
          })
          ];
        };

        cardanoCliPkg = pkgs: pkgs.haskell-nix.project' {
          compiler-nix-name = "ghc966";
          src = inputs.cardano-cli;

          cabalProjectLocal = ''
          '';

          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://chap.intersectmbo.org/" = inputs.CHaP;
          };
          modules = [({
            # packages.double-conversion.ghcOptions = [
            #   # stop putting U __gxx_personality_v0 into the library!
            #   "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
            #   # stop putting U __cxa_guard_release into the library!
            #   "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
            # ];
          })
          # Fix compilation with newer ghc versions
          ({ lib, config, ... }:
            lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
            # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
            # to call out to all kinds of silly tools that GHC doesn't really provide.
            # For this reason, we try to get away without re-installing lib:ghc for now.
            reinstallableLibGhc = false;
          })
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.cardano-cli.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
                "-L${lib.getLib static-ncurses}/lib"
            ];
          })
          ];
        };
        cardanoAddressesPkg = pkgs: pkgs.haskell-nix.project' {
          compiler-nix-name = "ghc966";
          src = inputs.cardano-addresses;
          cabalProjectLocal = "";
          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://chap.intersectmbo.org/" = inputs.CHaP;
          };
          modules = [({
            # packages.double-conversion.ghcOptions = [
            #   # stop putting U __gxx_personality_v0 into the library!
            #   "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
            #   # stop putting U __cxa_guard_release into the library!
            #   "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
            # ];
          })
          # Fix compilation with newer ghc versions
          ({ lib, config, ... }:
            lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
            # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
            # to call out to all kinds of silly tools that GHC doesn't really provide.
            # For this reason, we try to get away without re-installing lib:ghc for now.
            reinstallableLibGhc = false;
          })
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.cardano-addresses.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
            ];
          })
          ];
        };
        bech32Pkgs = pkgs: pkgs.haskell-nix.project' {
          compiler-nix-name = "ghc966";
          src = inputs.bech32;
          cabalProjectLocal = "";
          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
            "https://chap.intersectmbo.org/" = inputs.CHaP;
          };
          modules = [({
            # packages.double-conversion.ghcOptions = [
            #   # stop putting U __gxx_personality_v0 into the library!
            #   "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
            #   # stop putting U __cxa_guard_release into the library!
            #   "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
            # ];
          })
          # Fix compilation with newer ghc versions
          ({ lib, config, ... }:
            lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
            # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
            # to call out to all kinds of silly tools that GHC doesn't really provide.
            # For this reason, we try to get away without re-installing lib:ghc for now.
            reinstallableLibGhc = false;
          })
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.bech32.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libsodium-vrf}/lib"
                "-L${lib.getLib static-secp256k1}/lib"
                "-L${lib.getLib static-openssl}/lib"
                "-L${lib.getLib static-libblst}/lib"
            ];
          })
          ];
        };
        # for this simple demo, we'll just use a package from hackage. Namely the
        # trivial `hello` package. See https://hackage.haskell.org/package/hello
        helloPkg = pkgs.haskell-nix.hackage-package {
          inherit compiler-nix-name;
          name = "hello";
          version = "1.0.0.2";
        };
        cabalPkg = pkgs: pkgs.haskell-nix.project' {
          compiler-nix-name = "ghc966";
          src = inputs.cabal-install // { filterPath = { path, ... }: path; };
          modules = [
            ({ lib, config, ... }:{
              packages.Cabal.patches = lib.mkForce [];
            })
            (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
              packages.cabal-install.components.exes.cabal.ghcOptions = with pkgs; [
                "-L${lib.getLib static-gmp}/lib"
                "-L${lib.getLib static-libcxxabi}/lib"
              ];
            })
          ];
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
            # packages.hello-dynamic = helloPkg-dynamic.components.exes.hello;
            packages.hello-mingw = helloPkg-mingw.components.exes.hello;
            packages.hello-ucrt64 = helloPkg-ucrt64.components.exes.hello;
            # packages.hello-javascript = helloPkg-javascript.components.exes.hello; -- GHCJS linker bug with GHC 9.6.6
        };
        kupoPackages.packages = pkgs.lib.optionalAttrs (system == "x86_64-darwin" || system == "aarch64-darwin") {
          kupo-native            = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-kupo";                                             } (kupoPkgs pkgs                                     ).hsPkgs.kupo.components.exes.kupo;
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          kupo-static-musl       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-kupo-static";                     } (kupoPkgs pkgs.pkgsCross.musl64                    ).hsPkgs.kupo.components.exes.kupo;
          kupo-static-musl-arm64 = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-kupo-static"; } (kupoPkgs pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.kupo.components.exes.kupo;
          # kupo-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-kupo";             } (kupoPkgs pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.kupo.components.exes.kupo;

          # kupo requires the unix package, so we can't build mingwW64 or ucrt64 really.
          # kupo-mingw       = (kupoPkgs pkgs.pkgsCross.mingwW64                  ).hsPkgs.kupo.components.exes.kupo;
          # however if it didn't, this could be built with 9.4+
          # kupo-ucrt64 = (kupoPkgs pkgs.pkgsCross.ucrt64).hsPkgs.kupo.components.exes.kupo;

          # and this likely won't work at all due to all the c level dependencies; also we'd want to use 9.6+
          # kupo-javascript = (kupoPkgs pkgs.pkgsCross.ghcjs).hsPkgs.kupo.components.exes.kupo;
        };

        # Ogmios
        ogmiosPackages.packages = pkgs.lib.optionalAttrs (system == "x86_64-darwin" || system == "aarch64-darwin") {
          ogmios-native       = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-ogmios";                                                  } (ogmiosPkgs pkgs                                     ).hsPkgs.ogmios.components.exes.ogmios;
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          ogmios-static-musl       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-ogmios-static";                     } (ogmiosPkgs pkgs.pkgsCross.musl64                    ).hsPkgs.ogmios.components.exes.ogmios;
          ogmios-static-musl-arm64 = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-ogmios-static"; } (ogmiosPkgs pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.ogmios.components.exes.ogmios;
          # ogmios-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-ogmios";             } (ogmiosPkgs pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.ogmios.components.exes.ogmios;
        };

        # --ghc-option='-fplugin-library=${plutus-tx-plugin}/libplutus-tx-plugin.a;plutus-tx-plugin;PlutusTx.Plugin;[]'
        hydraPackages.packages = pkgs.lib.optionalAttrs (system == "x86_64-darwin" || system == "aarch64-darwin") {
          hydra-native       = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-hydra-node";                                                  } (hydraPkgs pkgs                                     ).hsPkgs.hydra-node.components.exes.hydra-node;
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          plutus-tx-plugin   = (hydraPkgs pkgs).hsPkgs.plutus-tx-plugin.components.library.override {
            ghcOptions = [ "-staticlib" "-fno-link-rts" "-this-unit-id" "plutus-tx-plugin" "-shared" ];
            postInstall = ''
              cp liba.a $out/libplutus-tx-plugin.a
            '';
            };

          hydra-static-musl       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-hydra-node-static";                     } (hydraPkgs pkgs.pkgsCross.musl64                    ).hsPkgs.hydra-node.components.exes.hydra-node;
          hydra-static-musl-arm64 = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-hydra-node-static"; } (hydraPkgs pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.hydra-node.components.exes.hydra-node;
          # hydra-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-hydra-node";             } (hydraPkgs pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.hydra-node.components.exes.hydra-node;
        };

        # dbSyncPackages.packages = pkgs.lib.optionalAttrs (system == "x86_64-darwin" || system == "aarch64-darwin") {
        #   db-sync                   = let plat = pkgs;                                      hsPkgs = (dbSyncPkg "ghc810" pkgs).hsPkgs;
        #                               in pkgs.packaging.asZip { name = "${plat.hostPlatform.system}-db-sync-${hsPkgs.cardano-db-sync.components.exes.cardano-db-sync.version}";        } hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
        #   db-sync-8107                   = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-db-sync-8107";                                             } (dbSyncPkg "ghc8107" pkgs                                     ).hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
        # } // pkgs.lib.optionalAttrs (system == "aarch64-linux") {
        #   db-sync-static-musl       = let plat = pkgs.pkgsCross.musl64;                     hsPkgs = (dbSyncPkg "ghc810" pkgs).hsPkgs;
        #                               in pkgs.packaging.asZip { name = "${plat.hostPlatform.system}-db-sync-static-${hsPkgs.cardano-db-sync.components.exes.cardano-db-sync.version}-${inputs.db-sync.shortRev}"; } hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
        # } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
        #   db-sync-static-musl       = let plat = pkgs.pkgsCross.musl64;                     hsPkgs = (dbSyncPkg "ghc810" plat).hsPkgs;
        #                               in pkgs.packaging.asZip { name = "${plat.hostPlatform.system}-db-sync-static-${hsPkgs.cardano-db-sync.components.exes.cardano-db-sync.version}"; } hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
        #   db-sync-static-musl-arm64 = let plat = pkgs.pkgsCross.aarch64-multiplatform-musl; hsPkgs = (dbSyncPkg "ghc810" plat).hsPkgs;
        #                               in pkgs.packaging.asZip { name = "${plat.hostPlatform.system}-db-sync-static-${hsPkgs.cardano-db-sync.components.exes.cardano-db-sync.version}"; } hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
        #   # db-sync-dynamic-arm64     = let plat = pkgs.pkgsCross.aarch64-multiplatform;      hsPkgs = (dbSyncPkg "ghc810" plat).hsPkgs;
        #   #                             in pkgs.packaging.asZip { name = "${plat.hostPlatform.system}-db-sync-${hsPkgs.cardano-db-sync.components.exes.cardano-db-sync.version}";        } hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
        # } // pkgs.lib.optionalAttrs (system == "aarch64-linux") {
        #   # we can not build this, text-2 prohibits aarch64 with 8107.
        #   # db-sync-8107-static-musl       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-db-sync-8107-static";                     } (dbSyncPkg "ghc8107" pkgs.pkgsCross.musl64                    ).hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
        # } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
        #   db-sync-8107-static-musl       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-db-sync-8107-static";                     } (dbSyncPkg "ghc8107" pkgs.pkgsCross.musl64                    ).hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
        #   # we can not build this, text-2 prohibits aarch64 with 8107.
        #   # db-sync-8107-static-musl-arm64 = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-db-sync-8107-static"; } (dbSyncPkg "ghc8107" pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
        #   # db-sync-8107-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-db-sync-8107";             } (dbSyncPkg "ghc8107" pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
        # };

        encoinsPackages.packages = {
          encoins-relay-server = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-encoins-relay-server"; } (encoinsPkg pkgs).hsPkgs.encoins-relay-server.components.exes.encoins-relay-server;
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          encoins-relay-server-static-musl       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-encoins-relay-server-static";                     } (encoinsPkg pkgs.pkgsCross.musl64                    ).hsPkgs.encoins-relay-server.components.exes.encoins-relay-server;
          encoins-relay-server-static-musl-arm64 = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-encoins-relay-server-static"; } (encoinsPkg pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.encoins-relay-server.components.exes.encoins-relay-server;
          # encoins-relay-server-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-encoins-relay-server";             } (encoinsPkg pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.encoins-relay-server.components.exes.encoins-relay-server;
        };

        cardanoNodePackages.packages =
          let node = pkgs: map (exe: (cardanoNodePkg pkgs).hsPkgs.${exe}.components.exes.${exe}) [
                "cardano-node" "cardano-submit-api"
              ]
              ++ [
                (cardanoCliPkg pkgs).hsPkgs.cardano-cli.components.exes.cardano-cli
                (cardanoAddressesPkg pkgs).hsPkgs.cardano-addresses.components.exes.cardano-address
                (bech32Pkgs pkgs).hsPkgs.bech32.components.exes.bech32
              ];
              pkg = comps: pkgs.packaging.asZip {
                name = let comp = if __isList comps then __head comps else comps; in builtins.concatStringsSep "-" [
                  comp.stdenv.hostPlatform.system    # arch, e.g. aarch64-darwin
                  comp.passthru.identifier.name      # pkg name, e.g. cabal-install
                  comp.version                       # component version, e.g. 3.10.3.0
                  comp.src.origSrc.shortRev          # source rev, e.g. 256f85d
                ];
              } comps;
          in pkgs.lib.optionalAttrs (system == "x86_64-darwin" || system == "aarch64-darwin") {
            cardano-tools = pkg (node pkgs);
          } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
            cardano-tools-static       = pkg (node pkgs.pkgsCross.musl64);
            cardano-tools-static-arm64 = pkg (node pkgs.pkgsCross.aarch64-multiplatform-musl);
            cardano-tools-ucrt         = pkg (node pkgs.pkgsCross.ucrt64);
            cardano-tools-mingwW64     = pkg (node pkgs.pkgsCross.mingwW64);
          };

        # cardano-node 10.7.0 pre-release builds
        cardanoNodePrePackages.packages =
          let node = pkgs: map (exe: (cardanoNodePrePkg pkgs).hsPkgs.${exe}.components.exes.${exe}) [
                "cardano-node" "cardano-submit-api"
              ]
              ++ [
                (cardanoCliPkg pkgs).hsPkgs.cardano-cli.components.exes.cardano-cli
                (cardanoAddressesPkg pkgs).hsPkgs.cardano-addresses.components.exes.cardano-address
                (bech32Pkgs pkgs).hsPkgs.bech32.components.exes.bech32
              ];
              pkg = comps: pkgs.packaging.asZip {
                name = let comp = if __isList comps then __head comps else comps; in builtins.concatStringsSep "-" [
                  comp.stdenv.hostPlatform.system
                  "cardano-node-pre"
                  comp.version
                  comp.src.origSrc.shortRev
                ];
              } comps;
          in pkgs.lib.optionalAttrs (system == "x86_64-darwin" || system == "aarch64-darwin") {
            cardano-tools-pre = pkg (node pkgs);
          } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
            cardano-tools-pre-static       = pkg (node pkgs.pkgsCross.musl64);
            cardano-tools-pre-static-arm64 = pkg (node pkgs.pkgsCross.aarch64-multiplatform-musl);
            # Windows cross disabled for 10.7.0 pre-release (Win32-network version conflict)
            # cardano-tools-pre-ucrt         = pkg (node pkgs.pkgsCross.ucrt64);
            # cardano-tools-pre-mingwW64     = pkg (node pkgs.pkgsCross.mingwW64);
          };

        # Pruned ImmutableDB variant — only cardano-node and cardano-submit-api
        # (cardano-cli, cardano-addresses, bech32 are unaffected by pruning patches)
        prunedCardanoNodePackages.packages =
          let node = pkgs: map (exe: (prunedCardanoNodePkg pkgs).hsPkgs.${exe}.components.exes.${exe}) [
                "cardano-node" "cardano-submit-api"
              ];
              pkg = comps: pkgs.packaging.asZip {
                name = let comp = if __isList comps then __head comps else comps; in builtins.concatStringsSep "-" [
                  comp.stdenv.hostPlatform.system
                  "pruned-cardano-node"
                  comp.version
                  comp.src.origSrc.shortRev
                ];
              } comps;
          in pkgs.lib.optionalAttrs (system == "x86_64-darwin" || system == "aarch64-darwin") {
            pruned-cardano-tools = pkg (node pkgs);
          } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
            pruned-cardano-tools-static       = pkg (node pkgs.pkgsCross.musl64);
            pruned-cardano-tools-static-arm64 = pkg (node pkgs.pkgsCross.aarch64-multiplatform-musl);
          };

        cabalInstallPackages.packages =
          let cabal = pkgs: (cabalPkg pkgs).hsPkgs.cabal-install.components.exes.cabal.overrideDerivation (_: { patches = []; });
              pkg = comps: pkgs.packaging.asZip {
                name = let comp = if __isList comps then __head comps else comps; in builtins.concatStringsSep "-" [
                  comp.stdenv.hostPlatform.system    # arch, e.g. aarch64-darwin
                  comp.passthru.identifier.name      # pkg name, e.g. cabal-install
                  comp.version                       # component version, e.g. 3.10.3.0
                  comp.src.origSrc.shortRev          # source rev, e.g. 256f85d
                ];
              } comps;
          in pkgs.lib.optionalAttrs (system == "x86_64-darwin" || system == "aarch64-darwin") {
            cabal-install-static       = pkg (cabal pkgs);
          } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
            cabal-install-hooks        = pkgs.buildPackages.stdenv.mkDerivation {
                name = "cabal-hooks";
                buildInputs = [ pkgs.buildPackages.gnutar ];

                phases = [ "installPhase" ];

                installPhase = ''
                  mkdir -p "cabal-hooks";
                  ls ${./cabal-hooks}
                  cp ${./cabal-hooks}/* cabal-hooks/
                  mkdir -p $out
                  (cd cabal-hooks && tar -czf $out/cabal-hooks.tar.gz *)

                  mkdir -p $out/nix-support
                  echo "file binary-dist \"$(echo $out/*.tar.gz)\"" \
                  > $out/nix-support/hydra-build-products
                '';
              };
            cabal-install-static       = pkg (cabal pkgs.pkgsCross.musl64);
            cabal-install-static-arm64 = pkg (cabal pkgs.pkgsCross.aarch64-multiplatform-musl);
            cabal-install-ucrt         = pkg (cabal pkgs.pkgsCross.ucrt64);
            cabal-install-mingwW64     = pkg (cabal pkgs.pkgsCross.mingwW64);
          };
          #  // {
          #   cabal-install-dynamic      = pkg (cabal pkgs);
          # };

        nixToolsPackages.packages =
        let nix-tools = pkgs:
                          (map (exe: (nixToolsPkg pkgs).hsPkgs.nix-tools.components.exes.${exe}.overrideDerivation (_: { stripDebugFlags = []; }))
                                   ["cabal-name" "cabal-to-nix" "hackage-to-nix" "hashes-to-nix" "lts-to-nix" "make-install-plan" "plan-to-nix" "stack-repos" "stack-to-nix" "truncate-index" ])
                           ++
                           (with (nixToolsPkg pkgs).hsPkgs;
                                   [ cabal-install.components.exes.cabal hpack.components.exes.hpack ])
                          ;
            pkg = comps: pkgs.packaging.asZip {
              name = (let comp = if __isList comps then __head comps else comps; in
                      builtins.concatStringsSep "-" ([
                        comp.stdenv.hostPlatform.system    # arch, e.g. aarch64-darwin
                        comp.passthru.identifier.name      # pkg name, e.g. cabal-install
                        comp.version                       # component version, e.g. 3.10.3.0
                      ] ++ (if (comp.src ? origSrc && comp.src.origSrc ? shortRev) then [
                        comp.src.origSrc.shortRev          # source rev, e.g. 256f85d
                      ] else [])));
              } comps;
        in pkgs.lib.optionalAttrs (system == "x86_64-darwin" || system == "aarch64-darwin") {
          nix-tools-static          = pkg (nix-tools pkgs);
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          nix-tools-static          = pkg (nix-tools pkgs.pkgsCross.musl64);
          nix-tools-static-arm64    = pkg (nix-tools pkgs.pkgsCross.aarch64-multiplatform-musl);
          # nix-tools-static-ucrt     = pkg (nix-tools pkgs.pkgsCross.ucrt64);
          # nix-tools-static-mingwW64 = pkg (nix-tools pkgs.pkgsCross.mingwW64);
        };
        nixToolsPackagesNoIfd.packages =
        pkgs.lib.optionalAttrs (system == "x86_64-darwin" || system == "aarch64-darwin") {
          "nix-tools-static-no-ifd" = pkgs.runCommand "${pkgs.hostPlatform.system}-all-nix-tools" {
            requiredSystemFeatures = [ "recursive-nix" ];
            nativeBuildInputs = [ pkgs.nix pkgs.gitMinimal ]
                              ++ map (x: "${x}") (builtins.attrValues inputs)
                              ++ map (x: "${x}") (builtins.attrValues inputs.haskellNix.inputs)
                              ++ map (x: "${x}") (builtins.attrValues inputs.flake-utils.inputs)
                              ++ map (x: "${x}") (builtins.attrValues inputs.iohkNix.inputs)
                              ;
          } ''
            export HOME=$(mktemp -d)
            mkdir $out
            cp  $(nix --offline --extra-experimental-features "flakes nix-command" \
                  build --accept-flake-config --no-link --print-out-paths \
                  --system ${pkgs.hostPlatform.system} \
                  ${./.}#nix-tools-static)/*.zip $out/
          '';
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          "nix-tools-static-no-ifd" = pkgs.runCommand "x86_64-linux-all-nix-tools" {
            requiredSystemFeatures = [ "recursive-nix" ];
            nativeBuildInputs = [ pkgs.nix pkgs.gitMinimal ]
                              ++ map (x: "${x}") (builtins.attrValues inputs)
                              ++ map (x: "${x}") (builtins.attrValues inputs.haskellNix.inputs)
                              ++ map (x: "${x}") (builtins.attrValues inputs.flake-utils.inputs)
                              ++ map (x: "${x}") (builtins.attrValues inputs.iohkNix.inputs)
                              ;
          } ''
            export HOME=$(mktemp -d)
            mkdir $out
            cp  $(nix --offline --extra-experimental-features "flakes nix-command" \
                  build --accept-flake-config --no-link --print-out-paths \
                  ${./.}#nix-tools-static)/*.zip $out/
          '';
          "nix-tools-static-arm64-no-ifd" = pkgs.runCommand "aarch64-linux-all-nix-tools" {
            requiredSystemFeatures = [ "recursive-nix" ];
            nativeBuildInputs = [ pkgs.nix pkgs.gitMinimal ]
                              ++ map (x: "${x}") (builtins.attrValues inputs)
                              ++ map (x: "${x}") (builtins.attrValues inputs.haskellNix.inputs)
                              ++ map (x: "${x}") (builtins.attrValues inputs.flake-utils.inputs)
                              ++ map (x: "${x}") (builtins.attrValues inputs.iohkNix.inputs)
                              ;
          } ''
            export HOME=$(mktemp -d)
            mkdir $out
            cp  $(nix --offline --extra-experimental-features "flakes nix-command" \
                  build --accept-flake-config --no-link --print-out-paths \
                  ${./.}#nix-tools-static-arm64)/*.zip $out/
          '';
        };
        mithrilPackages.packages = pkgs.lib.optionalAttrs (system == "x86_64-linux")
          # Use a writable copy of the source to avoid Nix store permission
          # errors when crane/cargo tries to modify Cargo.lock.
          (let mithrilSrc = pkgs.runCommand "mithril-src" {} ''
                cp -r ${inputs.mithril} $out
                chmod -R u+w $out
              '';
              mithril-signer = let
              rustToolchain = rsPkgs.rust-bin.stable.latest.default.override {
                targets = [ "x86_64-unknown-linux-musl" ];
              };
              craneLib = (inputs.crane.mkLib pkgs.pkgsCross.musl64).overrideToolchain rustToolchain;
              in craneLib.buildPackage (rec {
                src = mithrilSrc;
                strictDeps = true;

                inherit (craneLib.crateNameFromCargoToml { cargoTomlContents = builtins.readFile "${inputs.mithril}/mithril-signer/Cargo.toml";}) pname version;
                cargoExtraArgs = "-p mithril-signer";
                nativeBuildInputs = [
                  rsPkgs.pkg-config
                  rsPkgs.gnum4
                ];
                buildInputs = with rsPkgs.pkgsCross.musl64; [ static-openssl ];

                # rustdoc needs libgcc
                # LD_LIBRARY_PATH = "${pkgs.lib.getLib rsPkgs.pkgsCross.musl64.libgcc}/lib";

                # force-cross needed for `gmp-mpfr-sys'
                CARGO_FEATURE_FORCE_CROSS = "true";
                CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";
                CARGO_BUILD_RUSTFLAGS = "-C target-feature=+crt-static";
              });

              mithril-signer-arm = let
              rustToolchain = rsPkgs.rust-bin.stable.latest.default.override {
                targets = [ "aarch64-unknown-linux-musl" ];
              };
              craneLib = (inputs.crane.mkLib pkgs.pkgsCross.aarch64-multiplatform-musl).overrideToolchain rustToolchain;
              in craneLib.buildPackage (rec {
                src = mithrilSrc;
                strictDeps = true;

                depsBuildBuild = with pkgs.pkgsCross.aarch64-multiplatform-musl.buildPackages; [
                  # qemu
                ];

                inherit (craneLib.crateNameFromCargoToml { cargoTomlContents = builtins.readFile "${inputs.mithril}/mithril-signer/Cargo.toml";}) pname version;
                cargoExtraArgs = "-p mithril-signer";
                nativeBuildInputs = [
                  rsPkgs.pkg-config
                  rsPkgs.gnum4
                ];
                buildInputs = with rsPkgs.pkgsCross.aarch64-multiplatform-musl; [ static-openssl ];

                # rustdoc needs libgcc
                # LD_LIBRARY_PATH = "${pkgs.lib.getLib rsPkgs.pkgsCross.musl64.libgcc}/lib";

                # force-cross needed for `gmp-mpfr-sys'
                CARGO_FEATURE_FORCE_CROSS = "true";
                CARGO_BUILD_TARGET = "aarch64-unknown-linux-musl";
                # I have no idea why we fail to link libc here. WTF.
                CARGO_BUILD_RUSTFLAGS = "-C target-feature=+crt-static -C link-args=-lc";
                CARGO_TARGET_AARCH64_UNKNOWN_LINUX_MUSL_LINKER = "aarch64-unknown-linux-musl-cc";
                CARGO_TARGET_AARCH64_UNKNOWN_LINUX_MUSL_RUNNER = "qemu-aarch64";
              });
          in {
            mithril-signer-static = pkgs.packaging.asZip { name = "${mithril-signer.stdenv.hostPlatform.system}-mithril-signer-${mithril-signer.version}-${inputs.mithril.shortRev}"; } mithril-signer;
            mithril-signer-static-arm64 = pkgs.packaging.asZip { name = "${mithril-signer-arm.stdenv.hostPlatform.system}-mithril-signer-${mithril-signer-arm.version}-${inputs.mithril.shortRev}"; } mithril-signer-arm;
        });
        # ; in nix-tools-pkgs // { default-nix = (import nixpkgs { system = "x86_64-linux" }).runCommand "default.nix" { buildInputs = [] } (''
        #     mkdir -p $out/nix-support
        #     echo "pkgs: baseurl: {" > $out/default.nix
        #     echo "  x86_64-linux = pkgs.fetchzip { name = \"nix-tools\"; url = \"''${baseurl}/nix-tools-static.zip\"; sha256 = $(sha256sum ${nix-tools-static})" >> $out/default.nix
        #     echo "}" > $out/default.nix
        #     cp ${self.writeText "default.nix" ''pkgs: baseurl: {
        #       x86_64-linux = pkgs.fetchzip { name = "nix-tools"; url = "${baseurl}/nix-tools-static.zip"; sha256 =
        #     }''} $out/default.nix
        #     echo "report build-products $out default.nix" > $out/nix-support/hydra-build-products
        # '')};

        # helper function to add `hydraJobs` to the flake output.
        addHydraJobs = pkgs: pkgs // { hydraJobs = pkgs.packages; };
      # turn them into a merged flake output.
      in addHydraJobs (
        pkgs.lib.foldl' (pkg: acc: pkgs.lib.recursiveUpdate acc pkg)
          nativePackages
          [ linuxCrossPackages kupoPackages hydraPackages
            #dbSyncPackages
            #ogmiosPackages  -- ogmios is pinned to GHC 8.10.7 which haskell.nix no longer supports
            #encoinsPackages -- encoins is pinned to GHC 8.10.7 which haskell.nix no longer supports
            cardanoNodePackages cardanoNodePrePackages prunedCardanoNodePackages
            # nixToolsPackages nixToolsPackagesNoIfd
            mithrilPackages cabalInstallPackages ]
      )
    ); in with (import nixpkgs { system = "x86_64-linux"; overlays = [(import ./download.nix)]; });
          lib.recursiveUpdate flake {
            hydraJobs.index = hydra-utils.mkIndex flake;
            # hydraJobs.nix-tools = pkgs.releaseTools.aggregate {
            #   name = "nix-tools";
            #   constituents = [
            #     "aarch64-darwin.nix-tools-static"
            #     "x86_64-darwin.nix-tools-static"
            #     "x86_64-linux.nix-tools-static"
            #     "x86_64-linux.nix-tools-static-arm64"
            #     "aarch64-darwin.nix-tools-static-no-ifd"
            #     "x86_64-darwin.nix-tools-static-no-ifd"
            #     "x86_64-linux.nix-tools-static-no-ifd"
            #     "x86_64-linux.nix-tools-static-arm64-no-ifd"
            #     (writeText "gitrev" (self.rev or "0000000000000000000000000000000000000000"))
            #   ];
            # };
            # hydraJobs.all-nix-tools = runCommand "all-nix-tools" {
            #   } ''
            #   mkdir $out
            #   cp ${flake.hydraJobs.aarch64-darwin.nix-tools-static-no-ifd}/*.zip $out/
            #   cp ${flake.hydraJobs.x86_64-darwin.nix-tools-static-no-ifd}/*.zip $out/
            #   cp ${flake.hydraJobs.x86_64-linux.nix-tools-static-no-ifd}/*.zip $out/
            #   cp ${flake.hydraJobs.x86_64-linux.nix-tools-static-arm64-no-ifd}/*.zip $out/
            # '';
          };
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
