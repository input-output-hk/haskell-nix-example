let mkHTML = flake: ''
<!DOCTYPE html>
<html>
    <head>
        <!-- Import element definition -->
        <script type="module" src="https://cdn.jsdelivr.net/gh/zerodevx/zero-md@2/dist/zero-md.min.js"></script>
    </head>
    <body>
        <zero-md style="width: 600pt; margin: auto">
            <script type="text/markdown" data-dedent>
            # Build artifacts

            ## Linux builds
            | Attribute | Architecture | Link |
            | --------- | ------------ | ---- |
            ${
            builtins.concatStringsSep "\n"
              (map (p: "| ${p.drv.pname} | ${p.drv.stdenv.hostPlatform.system} | <a href=\"./${p.drv.stdenv.hostPlatform.system}/${p.packageName}\">${p.name}</a> |")
                (builtins.filter (x: x ? isPackage && x.isPackage)
                  (builtins.attrValues flake.hydraJobs.x86_64-linux ++ builtins.attrValues flake.hydraJobs.aarch64-linux)))
            }

            ## macOS builds
            | Attribute | Architecture | Link |
            | --------- | ------------ | ---- |
            ${
            builtins.concatStringsSep "\n"
              (map (p: "| ${p.drv.pname} | ${p.drv.stdenv.hostPlatform.system} | <a href=\"./${p.packageName}\">${p.name}</a> |")
                (builtins.filter (x: x ? isPackage && x.isPackage)
                  (builtins.attrValues flake.hydraJobs.x86_64-darwin ++ builtins.attrValues flake.hydraJobs.aarch64-darwin)))
            }
            </script>
        </zero-md>
    </body>
</html>
''; in
super: self: {
    hydra-utils = {
        mkIndex = flake:
            self.runCommand "index-html" { buildInputs = [  ]; } (''
            mkdir -p $out/nix-support
            cp ${self.writeText "index.html" (mkHTML flake)} $out/index.html
            ${builtins.concatStringsSep "\n"
                (map (p: "mkdir -p $out/${p.drv.stdenv.hostPlatform.system} && cp ${p}/${p.packageName} $out/${p.drv.stdenv.hostPlatform.system}/")
                    (builtins.filter (x: x ? isPackage && x.isPackage)
                        (builtins.concatMap builtins.attrValues (builtins.attrValues flake.hydraJobs))))
            }
            echo "report build-products $out index.html" > $out/nix-support/hydra-build-products
            '');
    };
}