{
  description = "A simple flake for building the hello package with haskell.nix";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix?ref=angerman/fix-aarch64-musl";
    iserv-proxy = {
      url = "github:stable-haskell/iserv-proxy?ref=iserv-syms";
      flake = false;
    };
    haskellNix.inputs.iserv-proxy.follows = "iserv-proxy";
    # for caching you want to follow haskell.nix's nixpkgs-unstable pins.
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    kupo.url = "github:CardanoSolutions/kupo?ref=v2.7";
    kupo.flake = false;

    ogmios.url = "github:CardanoSolutions/ogmios?rev=199daf67062e7c9efa735a0ba7d80d49108a56a0";
    ogmios.flake = false;

    hydra.url = "github:input-output-hk/hydra";
    hydra.flake = false;

    db-sync.url = "github:input-output-hk/cardano-db-sync";
    db-sync.flake = false;

    encoins.url = "github:encryptedcoins/encoins-relay";
    encoins.flake = false;

    cardano-node.url = "github:input-output-hk/cardano-node?ref=8.7.2";
    cardano-node.flake = false;

    nix-tools.url = "github:input-output-hk/haskell.nix?dir=nix-tools";
    nix-tools.flake = false;

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
              dyn-static-secp256k1 = final.secp256k1.overrideDerivation (old: {
                configureFlags = old.configureFlags ++ ["--enable-static" "--enable-shared" ];
              });
              static-gmp = (final.gmp.override { withStatic = true; }).overrideDerivation (old: {
                configureFlags = old.configureFlags ++ ["--enable-static" "--disable-shared" ];
              });
              static-libblst = (final.libblst.override { enableShared = false; }).overrideDerivation (old: {
                postFixup = "";
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
          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };
          sha256map = {
            "https://github.com/CardanoSolutions/ogmios"."01f7787216e7ceb8e39c8c6807f7ae53fc14ab9e" = "18wxmz3452lwnd169r408b54h5grjws3q25i30nkl425sl4k8p87";
            "https://github.com/CardanoSolutions/direct-sqlite"."82c5ab46715ecd51901256144f1411b480e2cb8b" = "1r1g6nf65d9n436ppcjky3gkywpnx4y0a3v88ddngchmf8za3qky";
            "https://github.com/CardanoSolutions/text-ansi"."dd81fe6b30e78e95589b29fd1b7be1c18bd6e700" = "0c9ckqcl4lahmkkfhj95bwwj4l2w8hlw0429gi1yly2mbdb688cq";
            "https://github.com/CardanoSolutions/text-ansi"."e204822d2f343b2d393170a2ec46ee935571345c" = "16ki7wsf7wivxn65acv4hxwfrzmphq4zp61lpxwzqkgrg8shi8bv";
          };
          modules = [{
            packages.double-conversion.ghcOptions = [
              # stop putting U __gxx_personality_v0 into the library!
              "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
              # stop putting U __cxa_guard_release into the library!
              "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
            ];
          }
          (pkgs.lib.mkIf (pkgs.hostPlatform.isMusl && pkgs.hostPlatform.isAarch64) {
            # this will disable --split-sections. Having --split-sections on will
            # break the linker in GHC. And thus make iserv (e.g. aarch64-linux-musl)
            # break during cross compilation. The UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts
            # will produce an almost 1MB object file, with an unholy amount of sections. The linker is sadly
            # quite stupid and maps each section into a page (4k on linux, 16k on darwin), and this quickly
            # leads to sections being so far apart, that the linker can't relocate the entries properly
            # anymore. To work around this, we disable --split-sections for plutus-core.
            # TODO: fix GHC's linker properly.
            packages.plutus-core.components.library.enableDeadCodeElimination = false;

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
            # this will disable --split-sections. Having --split-sections on will
            # break the linker in GHC. And thus make iserv (e.g. aarch64-linux-musl)
            # break during cross compilation. The UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts
            # will produce an almost 1MB object file, with an unholy amount of sections. The linker is sadly
            # quite stupid and maps each section into a page (4k on linux, 16k on darwin), and this quickly
            # leads to sections being so far apart, that the linker can't relocate the entries properly
            # anymore. To work around this, we disable --split-sections for plutus-core.
            # TODO: fix GHC's linker properly.
            packages.plutus-core.components.library.enableDeadCodeElimination = false;
            packages.plutus-tx.components.library.enableDeadCodeElimination = false;

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
        dbSyncPkg = pkgs: pkgs.haskell-nix.project' {
          compiler-nix-name = "ghc963";
          src = inputs.db-sync;

          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };
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
              packages.double-conversion.ghcOptions = [
                # stop putting U __gxx_personality_v0 into the library!
                "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
                # stop putting U __cxa_guard_release into the library!
                "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
              ];
              # Just say no to systemd.
              packages.cardano-config.flags.systemd = false;
              packages.cardano-node.flags.systemd = false;
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
        nixToolsPkg = pkgs: pkgs.haskell-nix.project' {
          # cabalProjectLocal = ''
          # allow-newer: *:base, *:ghc-prim, *:template-haskell
          # '';
          compiler-nix-name = "ghc928";
          src = "${inputs.nix-tools}/nix-tools";

          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };
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
          (pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.hydra-node.ghcOptions = with pkgs; [
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

          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };

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
        cardanoNodePkg = pkgs: pkgs.haskell-nix.project' {
          compiler-nix-name = "ghc963";
          src = inputs.cardano-node;

          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };
          modules = [({
            packages.double-conversion.ghcOptions = [
              # stop putting U __gxx_personality_v0 into the library!
              "-optcxx-fno-rtti" "-optcxx-fno-exceptions"
              # stop putting U __cxa_guard_release into the library!
              "-optcxx-std=gnu++98" "-optcxx-fno-threadsafe-statics"
            ];
            # Just say no to systemd.
            packages.cardano-config.flags.systemd = false;
            packages.cardano-node.flags.systemd = false;
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
            packages.hydra-node.ghcOptions = with pkgs; [
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

        # --ghc-option='-fplugin-library=${plutus-tx-plugin}/libplutus-tx-plugin.a;plutus-tx-plugin;PlutusTx.Plugin;[]'
        hydraPackages.packages = {
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
          hydra-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-hydra-node";             } (hydraPkgs pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.hydra-node.components.exes.hydra-node;
        };

        dbSyncPackages.packages = {
          db-sync            = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-db-sync";                                                  } (dbSyncPkg pkgs                                     ).hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          db-sync-static-musl       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-db-sync-static";                     } (dbSyncPkg pkgs.pkgsCross.musl64                    ).hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
          db-sync-static-musl-arm64 = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-db-sync-static"; } (dbSyncPkg pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
          db-sync-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-db-sync";             } (dbSyncPkg pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.cardano-db-sync.components.exes.cardano-db-sync;
        };

        encoinsPackages.packages = {
          encoins-relay-server = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-encoins-relay-server"; } (encoinsPkg pkgs).hsPkgs.encoins-relay-server.components.exes.encoins-relay-server;
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          encoins-relay-server-static-musl       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-encoins-relay-server-static";                     } (encoinsPkg pkgs.pkgsCross.musl64                    ).hsPkgs.encoins-relay-server.components.exes.encoins-relay-server;
          encoins-relay-server-static-musl-arm64 = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-encoins-relay-server-static"; } (encoinsPkg pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.encoins-relay-server.components.exes.encoins-relay-server;
          encoins-relay-server-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-encoins-relay-server";             } (encoinsPkg pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.encoins-relay-server.components.exes.encoins-relay-server;
        };

        cardanoNodePackages.packages = {
          cardano-node = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-cardano-node"; } (cardanoNodePkg pkgs).hsPkgs.cardano-node.components.exes.cardano-node;
          cardano-cli  = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-cardano-cli";  } (cardanoNodePkg pkgs).hsPkgs.cardano-cli.components.exes.cardano-cli;
          cardano-submit-api = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-cardano-submit-api";  } (cardanoNodePkg pkgs).hsPkgs.cardano-submit-api.components.exes.cardano-submit-api;
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          cardano-node-static-musl       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-cardano-node-static";                     } (cardanoNodePkg pkgs.pkgsCross.musl64                    ).hsPkgs.cardano-node.components.exes.cardano-node;
          cardano-node-static-musl-arm64 = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-cardano-node-static"; } (cardanoNodePkg pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.cardano-node.components.exes.cardano-node;
          cardano-node-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-cardano-node";             } (cardanoNodePkg pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.cardano-node.components.exes.cardano-node;
          cardano-cli-static-musl        = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-cardano-cli-static";                      } (cardanoNodePkg pkgs.pkgsCross.musl64                    ).hsPkgs.cardano-cli.components.exes.cardano-cli;
          cardano-cli-static-musl-arm64  = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-cardano-cli-static";  } (cardanoNodePkg pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.cardano-cli.components.exes.cardano-cli;
          cardano-cli-dynamic-arm64      = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-cardano-cli";              } (cardanoNodePkg pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.cardano-cli.components.exes.cardano-cli;
          cardano-submit-api-static-musl = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-cardano-submit-api-static";               } (cardanoNodePkg pkgs.pkgsCross.musl64                    ).hsPkgs.cardano-submit-api.components.exes.cardano-submit-api;
          cardano-submit-api-static-musl-arm64 = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-cardano-submit-api-static";  } (cardanoNodePkg pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.cardano-submit-api.components.exes.cardano-submit-api;
          cardano-submit-api-dynamic-arm64     = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform.hostPlatform.system}-cardano-submit-api";              } (cardanoNodePkg pkgs.pkgsCross.aarch64-multiplatform     ).hsPkgs.cardano-submit-api.components.exes.cardano-submit-api;
        };
        nixToolsPackages.packages = let components = ["cabal-name" "cabal-to-nix" "hackage-to-nix" "hashes-to-nix" "lts-to-nix" "make-install-plan" "plan-to-nix" "stack-repos" "stack-to-nix" "truncate-index" ];
        in
        # This is commented out, we don't want each and every executable by itself. They should all be rolled up into
        # one nix-tools package per platform.


        # pkgs.lib.foldl' (pkg: acc: pkgs.lib.recursiveUpdate acc pkg) {} (map (exe: {
        #   "${exe}" = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-${exe}"; } (nixToolsPkg pkgs).hsPkgs.nix-tools.components.exes.${exe};
        # } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
        #   "${exe}-static-musl"       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-${exe}-static";                     } (nixToolsPkg pkgs.pkgsCross.musl64                    ).hsPkgs.nix-tools.components.exes.${exe};
        #   "${exe}-static-musl-arm64" = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-${exe}-static"; } (nixToolsPkg pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.nix-tools.components.exes.${exe};
        # }) components)
        # //
        {
          "nix-tools" = pkgs.packaging.asZip { name = "${pkgs.hostPlatform.system}-nix-tools"; } (map (exe: (nixToolsPkg pkgs).hsPkgs.nix-tools.components.exes.${exe}) components);
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          "nix-tools-static-musl"       = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.musl64.hostPlatform.system}-nix-tools-static";                     } (map (exe: (nixToolsPkg pkgs.pkgsCross.musl64                    ).hsPkgs.nix-tools.components.exes.${exe}) components);
          "nix-tools-static-musl-arm64" = pkgs.packaging.asZip { name = "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-nix-tools-static"; } (map (exe: (nixToolsPkg pkgs.pkgsCross.aarch64-multiplatform-musl).hsPkgs.nix-tools.components.exes.${exe}) components);
        };

        # helper function to add `hydraJobs` to the flake output.
        addHydraJobs = pkgs: pkgs // { hydraJobs = pkgs.packages; };
      # turn them into a merged flake output.
      in addHydraJobs (
        pkgs.lib.foldl' (pkg: acc: pkgs.lib.recursiveUpdate acc pkg)
          nativePackages
          [linuxCrossPackages kupoPackages ogmiosPackages hydraPackages dbSyncPackages encoinsPackages cardanoNodePackages nixToolsPackages ]
      )
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