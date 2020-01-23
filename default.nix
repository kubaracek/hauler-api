{ mkDerivation, aeson, base, bytestring, containers, ekg, ekg-core
, fast-logger, foreign-store, katip, microlens, monad-control
, monad-logger, monad-metrics, mtl, persistent
, persistent-postgresql, persistent-template, resource-pool, safe
, servant, servant-auth, servant-auth-server, servant-server
, stdenv, text, transformers, unordered-containers, wai, wai-extra
, wai-middleware-metrics, warp
}:
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers ekg ekg-core fast-logger
    foreign-store katip microlens monad-control monad-logger
    monad-metrics mtl persistent persistent-postgresql
    persistent-template resource-pool safe servant servant-server text
    transformers unordered-containers wai wai-extra
    wai-middleware-metrics warp
  ];
  executableHaskellDepends = [
    aeson base ekg ekg-core microlens monad-logger monad-metrics mtl
    persistent-postgresql safe servant servant-auth servant-auth-server
    servant-server wai-middleware-metrics warp
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
