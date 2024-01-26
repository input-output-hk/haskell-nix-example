pkgs: let baseurl = "https://github.com/input-output-hk/haskell-nix-example/releases/download/v0.1.1/"; in {
  aarch64-darwin = pkgs.fetchurl { 
     name = "aarch64-darwin-nix-tools-static";
     url = "${baseurl}aarch64-darwin-nix-tools-static.zip";
     sha256 = "sha256-uudswo1R3ed/C5Dvo7hzdMmSZDkkWwvXBXOVejkNDv8=";
  };
  x86_64-darwin = pkgs.fetchurl { 
     name = "x86_64-darwin-nix-tools-static";
     url = "${baseurl}x86_64-darwin-nix-tools-static.zip";
     sha256 = "sha256-2j4xH8CMLYRrtGu4BRX6F0Uqe1V97kFfDcTDGW09h+c=";
  };
  aarch64-linux = pkgs.fetchurl { 
     name = "aarch64-linux-nix-tools-static";
     url = "${baseurl}aarch64-linux-nix-tools-static.zip";
     sha256 = "sha256-FYMu4sxs37zRyqYAX6M/XBDmFmH321xm6CfOihaOogQ=";
  };
  x86_64-linux = pkgs.fetchurl { 
     name = "x86_64-linux-nix-tools-static";
     url = "${baseurl}x86_64-linux-nix-tools-static.zip";
     sha256 = "sha256-WKnMaOnA1dpFGj/82fdJuiUFYUArQ7EyZxwL1YFQx2o=";
  };
}
