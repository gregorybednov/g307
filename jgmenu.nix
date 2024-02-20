{ lib, stdenv, pkgs
, fetchFromGitHub
, pkg-config
, python3Packages
, pango
, librsvg
, libxml2
, menu-cache
, xorg
, makeWrapper
, enableXfcePanelApplet ? false
, xfce
, gtk3
, gitUpdater
}:

stdenv.mkDerivation rec {
  pname = "jgmenu";
  version = "4.4.1";

  src = fetchFromGitHub {
    owner = "johanmalm";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-UC92zyuMVjyMLNEOBMElO8wCWYgwWRZAGLEOdTPNMak=";
  };

  nativeBuildInputs = [
    pkg-config
    makeWrapper
    python3Packages.wrapPython
  ];
  patches = [(pkgs.writeText "xdgdirs.diff"
''
--- a/src/xdgdirs.c	2024-02-20 12:45:56.103021545 +0300
+++ b/src/xdgdirs.c	2024-02-20 12:47:32.225652432 +0300
@@ -18,8 +18,6 @@
 static char *xdg_data_dirs[] = { "$XDG_DATA_HOME",
 				 "$HOME/.local/share",
 				 "$XDG_DATA_DIRS",
-				 "/usr/share",
-				 "/usr/local/share",
 				 "/opt/share",
 				 NULL };
 
@@ -27,7 +25,6 @@
 static char *xdg_config_dirs[] = { "$XDG_CONFIG_HOME",
 				   "$HOME/.config",
 				   "$XDG_CONFIG_DIRS",
-				   "/etc/xdg",
 				   NULL };
 /* clang-format on */
 
'')];
  buildInputs = [
    pango
    librsvg
    libxml2
    menu-cache
    xorg.libXinerama
    xorg.libXrandr
    python3Packages.python
  ] ++ lib.optionals enableXfcePanelApplet [
    gtk3
    xfce.libxfce4util
    xfce.xfce4-panel
  ];

  configureFlags = [
  ]
  ++ lib.optionals enableXfcePanelApplet [
    "--with-xfce4-panel-applet"
  ];

  postFixup = ''
    wrapPythonProgramsIn "$out/lib/jgmenu"
    for f in $out/bin/jgmenu{,_run}; do
      wrapProgram $f --prefix PATH : $out/bin
    done
  '';

  passthru.updateScript = gitUpdater { rev-prefix = "v"; };

  meta = with lib; {
    homepage = "https://github.com/johanmalm/jgmenu";
    description = "Small X11 menu intended to be used with openbox and tint2";
    license = licenses.gpl2Plus;
    platforms = platforms.linux;
    maintainers = [ maintainers.romildo ];
  };
}
