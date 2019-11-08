{ pkgs, lib, ... }:

let
  xrandr = pkgs.fetchzip
    { url = https://github.com/theNerd247/xmonad-contrib-xrandr/archive/v0.3.0-apha.2.tar.gz;
      sha256 = "1n0vqh6frapycwb7qc23182q7wfl83fc1d2pbm1biqax1qz5nva6";
    };
in
{
  services.xserver.windowManager = 
  {
    default = "xmonad";
    xmonad = {
      config = ./xmonad.hs;
      enable = true;
      enableContribAndExtras = true;
      extraPackages = (hpkgs: [ hpkgs.xrandr ]);
      haskellPackages = pkgs.haskellPackages.override 
        {
          overrides = new: old:
          {
            xrandr = new.callPackage xrandr {};
          };
        };
    };
  };
}
