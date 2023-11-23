super: self: {
    packaging = {
        asZip = { name ? null }: drvs:
        let pkgs = self;
            drv = if builtins.isList drvs then builtins.head drvs else drvs;
            name' = if name == null then drv.pname or drv.name else name;
            targetPlatform = drv.stdenv.targetPlatform;
            nativePackages = self.buildPackages;
            interpForSystem = sys: let s = {
                "i686-linux"    = "/lib/ld-linux.so.2";
                "x86_64-linux"  = "/lib64/ld-linux-x86-64.so.2";
                "aarch64-linux" = "/lib/ld-linux-aarch64.so.1";
                "armv7l-linux"  = "/lib/ld-linux-armhf.so.3";
                "armv7a-linux"  = "/lib/ld-linux-armhf.so.3";
            }; in s.${sys} or (builtins.abort "Unsupported system ${sys}. Supported systms are: ${builtins.concatStringsSep ", " (builtins.attrNames s)}.");
        fixup-nix-deps = pkgs.writeShellApplication {
            name = "fixup-nix-deps";
            text = ''
            for nixlib in $(otool -L "$1" |awk '/nix\/store/{ print $1 }'); do
                case "$nixlib" in
                *libiconv.dylib) install_name_tool -change "$nixlib" /usr/lib/libiconv.dylib "$1" ;;
                *libffi.*.dylib) install_name_tool -change "$nixlib" /usr/lib/libffi.dylib   "$1" ;;
                *libc++.*.dylib) install_name_tool -change "$nixlib" /usr/lib/libc++.dylib   "$1" ;;
                *libz.dylib)     install_name_tool -change "$nixlib" /usr/lib/libz.dylib     "$1" ;;
                *) ;;
                esac
            done
            '';
        };
        in nativePackages.stdenv.mkDerivation {
            name = "${name'}.zip";
            buildInputs = with nativePackages; [ patchelf zip ];

            phases = [ "buildPhase" "installPhase" ];

            buildPhase = ''
                mkdir -p ${name'}
                cp ${drv.out}/bin/* ${name'}/
            ''
            # set the interpreter to the default expected location on linux. (See interpForSystem above)
            + pkgs.lib.optionalString (targetPlatform.isLinux && targetPlatform.isGnu) ''
                for bin in ${name'}/*; do
                mode=$(stat -c%a $bin)
                chmod +w $bin
                patchelf --set-interpreter ${interpForSystem targetPlatform.system} $bin
                chmod $mode $bin
                done
            '' + pkgs.lib.optionalString (targetPlatform.isWindows) ''
                # may need to copy dlls
            '' + pkgs.lib.optionalString (targetPlatform.isLinux && targetPlatform.isGnu) ''
                # need to copy referenced *.so* files.
            '' + pkgs.lib.optionalString (targetPlatform.isDarwin) ''
                for bin in ${name'}/*; do
                mode=$(stat -f%Lp $bin)
                chmod +w $bin
                fixup-nix-deps $bin
                chmod $mode $bin
                done
            '';

            # compress and put into hydra products
            installPhase = ''
                mkdir -p $out/
                zip -r -9 $out/${drv.name}.zip ${name'}

                mkdir -p $out/nix-support
                echo "file binary-dist \"$(echo $out/*.zip)\"" \
                > $out/nix-support/hydra-build-products
            '';
            passthru = {
                inherit drv;
                isPackage = true;
                packageName = "${drv.name}.zip";
            };
        };
    };
}