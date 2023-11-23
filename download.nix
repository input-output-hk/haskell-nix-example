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

            | Attribute | Link |
            | --------- | ---- |
            | blah      | blah |

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
            ''
            # TODO: copy stuff into $out/ that we want to reference from the index.html
            + ''
            echo "report cardano $out index.html" > $out/nix-support/hydra-build-products
            '');
    };
}