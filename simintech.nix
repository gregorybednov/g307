{ lib, stdenv, pkgs
, pango
, xorg
, autoPatchelfHook
}:

stdenv.mkDerivation rec {
  pname = "simintech";
  version = "2.23.11.14";

  src = ./simintech;

  nativeBuildInputs = [ autoPatchelfHook ];  

  buildInputs = with pkgs; [
    at-spi2-atk
    gdk-pixbuf.out
    glamoroustoolkit
    glib
    gtk2
    libGLU
    pango
    xorg.libX11
    zlib
  ];
  
  sourceRoot = ".";
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
#	runHook preInstall
	mkdir -p $out/opt
#	install -m755 -D simintech/bin/mmain $out/bin/mmain
#	runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://simintech.ru";
    description = "Simulation In Technics";
    license = licenses.unfree;
    platforms = platforms.linux;
    maintainers = [ greg ];
  };
}
