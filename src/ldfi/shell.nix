{ sources ? import ./../../nix/sources.nix
, compiler ? "ghc8103"
}:

(import ./default.nix { inherit sources compiler; }).env
