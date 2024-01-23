pkgs: let baseurl = "https://github.com/input-output-hk/haskell-nix-example/releases/download/v0.1.0/"; in {
  aarch64-darwin = pkgs.fetchurl { 
     name = "aarch64-darwin-nix-tools-static";
     url = "${baseurl}aarch64-darwin-nix-tools-static.zip";
     sha256 = "sha256-J3DZc5mHHk/1MFzhJPRue8aAXxkzg0eYRQ0LS2JzhmQ=";
  };
  x86_64-darwin = pkgs.fetchurl { 
     name = "x86_64-darwin-nix-tools-static";
     url = "${baseurl}x86_64-darwin-nix-tools-static.zip";
     sha256 = "sha256-zj9VHoiN62BNo8Vve2xnx8cawzJpp4gvhep6wPioaps=";
  };
  aarch64-linux = pkgs.fetchurl { 
     name = "aarch64-linux-nix-tools-static";
     url = "${baseurl}aarch64-linux-nix-tools-static.zip";
     sha256 = "sha256-U9tXa4EwlosEKsVKiSdDrwpte4/izpGBf9ldvurIIDE=";
  };
  x86_64-linux = pkgs.fetchurl { 
     name = "x86_64-linux-nix-tools-static";
     url = "${baseurl}x86_64-linux-nix-tools-static.zip";
     sha256 = "sha256-mKlFqOzrOiBK0PlYQWcd8LM2UoIP4gn4ITyX6T+0ZZc=";
  };
}
