super: self: {
    packaging = {
        asTarball = drv:
        let name = drv.pname or drv.name;
            pkgs = self;
            targetPlatform = drv.stdenv.targetPlatform;
            nativePackages = self.buildPackages;
            interpForSystem = sys: let s = {
                "i686-linux"    = "/lib/ld-linux.so.2";
                "x86_64-linux"  = "/lib64/ld-linux-x86-64.so.2";
                "aarch64-linux" = "/lib/ld-linux-aarch64.so.1";
                "armv7l-linux"  = "/lib/ld-linux-armhf.so.3";
                "armv7a-linux"  = "/lib/ld-linux-armhf.so.3";
            }; in s.${sys} or (builtins.abort "Unsupported system ${sys}. Supported systms are: ${builtins.concatStringsSep ", " (builtins.attrNames s)}.");
        in nativePackages.stdenv.mkDerivation {
            name = "${drv.name}-tarball";
            buildInputs = with nativePackages; [ patchelf zip ];

            phases = [ "buildPhase" "installPhase" ];

            buildPhase = ''
                mkdir -p ${name}
                cp ${drv.out}/bin/* ${name}/
            ''
            # set the interpreter to the default expected location on linux. (See interpForSystem above)
            + pkgs.lib.optionalString (targetPlatform.isLinux && targetPlatform.isGnu) ''
                for bin in ${name}/*; do
                mode=$(stat -c%a $bin)
                chmod +w $bin
                patchelf --set-interpreter ${interpForSystem targetPlatform.system} $bin
                chmod $mode $bin
                done
            '' + pkgs.lib.optionalString (targetPlatform.isWindows) ''
                # may need to copy dlls
            '' + pkgs.lib.optionalString (targetPlatform.isLinux && targetPlatform.isGnu) ''
                # need to copy referenced *.so* files.
            '';

            # compress and put into hydra products
            installPhase = ''
                mkdir -p $out/
                zip -r -9 $out/${drv.name}.zip ${name}

                mkdir -p $out/nix-support
                echo "file binary-dist \"$(echo $out/*.zip)\"" \
                > $out/nix-support/hydra-build-products
            '';
            passthru = {
                inherit drv;
            };
        };
    };
}