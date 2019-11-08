{ stdenv, makeWrapper }:

stdenv.mkDerivation
{ name = "theNerd247-xmonad";
  src = ./.;

  installPhase = ''
    mkdir -p $out
    cp ./xmonad.hs $out/xmonad.hs
    cp ./xmobarcc $out/xmobarcc
  '';

}
