pkgs: let baseurl = "https://github.com/input-output-hk/haskell-nix-example/releases/download/v0.1.0/"; in {
  aarch64-darwin = pkgs.fetchurl { 
     name = "aarch64-darwin-nix-tools-static";
     url = "${baseurl}aarch64-darwin-nix-tools-static.zip";
     sha256 = "sha256-G4soHORNQvuevhvginu/GTW2auostwFfH4jYcXJCffc=";
  };
  x86_64-darwin = pkgs.fetchurl { 
     name = "x86_64-darwin-nix-tools-static";
     url = "${baseurl}x86_64-darwin-nix-tools-static.zip";
     sha256 = "sha256-C/B+nYjyzW5IxXKjDvW3LPIXJ62+5JXDvwtiDJDo38U=";
  };
  aarch64-linux = pkgs.fetchurl { 
     name = "aarch64-linux-nix-tools-static";
     url = "${baseurl}aarch64-linux-nix-tools-static.zip";
     sha256 = "sha256-hAa69L2x+QAmlaoZWaZJKSoJDQW79242TYmm1oDyKVM=";
  };
  x86_64-linux = pkgs.fetchurl { 
     name = "x86_64-linux-nix-tools-static";
     url = "${baseurl}x86_64-linux-nix-tools-static.zip";
     sha256 = "sha256-Z8F6TN1VARd/eaQxwukO0YcE/iq2Zs2lBLvsvIho2Rw=";
  };
}
