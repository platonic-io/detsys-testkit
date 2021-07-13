{ sources ? import ./../../nix/sources.nix
, compiler ? "ghc8104"
}:

(import ./default.nix { inherit sources compiler; }).env
