{ ... }: 

{
  nixpkgs.config = (import /home/noah/src/com/xrandr/release.nix);

  services.xserver.windowManager = 
  {
    default = "xmonad";
    xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = (hpkgs: [ hpkgs.xrandr ]);
    };
  };
}
