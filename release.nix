{ config, pkgs, lib, ... }:

let
  xrandr = pkgs.fetchzip
    { url = https://github.com/theNerd247/xmonad-contrib-xrandr/archive/v0.4.0-alpha.tar.gz;
      sha256 = "1w50zyfi7dfp3x669967i53y13ywzzfpqn2rd0nfxrybnaakxfr0";
    };

  xmonad = (pkgs.callPackage ./default.nix {});
  
  xmonadConfig = 
    builtins.replaceStrings 
      ["./xmobarcc"] 
      ["${xmonad}/xmobarcc"] 
      (builtins.readFile "${xmonad}/xmonad.hs");
in
{
  environment.systemPackages = with pkgs;
    [ haskellPackages.xmobar
      dmenu
      xsecurelock
      xorg.xbacklight
      termite
    ];

  services.xserver.windowManager = 
  {
    default = "xmonad";
    xmonad = {
      config = xmonadConfig;
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
