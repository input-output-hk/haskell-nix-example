pkgs: let baseurl = "https://github.com/input-output-hk/haskell-nix-example/releases/download/v0.1.0/"; in {
  aarch64-darwin = pkgs.fetchurl { 
     name = "aarch64-darwin-nix-tools-static";
     url = "${baseurl}aarch64-darwin-nix-tools-static.zip";
     sha256 = "sha256-EyrzCwoZdlWIfm4fL72uaGgIbxtoCWURpoDRFQFdkR4=";
  };
  x86_64-darwin = pkgs.fetchurl { 
     name = "x86_64-darwin-nix-tools-static";
     url = "${baseurl}x86_64-darwin-nix-tools-static.zip";
     sha256 = "sha256-cuVYBLMemVIDTmKXWm839umN+AXOoLc8IxDUWna5HSg=";
  };
  aarch64-linux = pkgs.fetchurl { 
     name = "aarch64-linux-nix-tools-static";
     url = "${baseurl}aarch64-linux-nix-tools-static.zip";
     sha256 = "sha256-dnTJhR1/YANwLL9YfWD3JBgfbA1v5TB+45wEhbESHnA=";
  };
  x86_64-linux = pkgs.fetchurl { 
     name = "x86_64-linux-nix-tools-static";
     url = "${baseurl}x86_64-linux-nix-tools-static.zip";
     sha256 = "sha256-OGN+m5URbzzv3HvPec176ZIrR+trhub5HbMS7fiFd/0=";
  };
}
