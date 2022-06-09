{ mkDerivation, base, containers, lens, lib, linear, process, random,
  sdl2, sdl2-gfx, sdl2-ttf, split, text, transformers
}:
mkDerivation {
  pname = "sdl-simple";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers lens linear sdl2 sdl2-gfx sdl2-ttf text
    transformers process
  ];
  executableHaskellDepends = [ base containers random split ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
