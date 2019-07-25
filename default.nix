with import (fetchTarball {
  url = https://github.com/NixOS/nixpkgs/archive/88d9f776091.tar.gz;
  sha256 = "0z8a0g69fmbbzi77jhvhwafv73dn5fg3gsr0q828lss6j5qpx995";
  }) { config = {}; };
haskellPackages.developPackage { root = ./.; }
