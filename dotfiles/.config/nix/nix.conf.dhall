let substituters = [
  "https://hydra.iohk.io",
  "https://iohk.cachix.org",
  "https://cache.nixos.org",

  "https://hercules-ci.cachix.org",
  "https://nix-community.cachix.org",
  "https://pre-commit-hooks.cachix.org",

  "https://nixcache.reflex-frp.org",

  "https://all-hies.cachix.org",
  "https://ghcide-nix.cachix.org",
]

let trusted-public-keys = [
  "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=",
  "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=",
  "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=",

  "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0=",
  "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=",
  "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc=",

  "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=",

  "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k=",
  "ghcide-nix.cachix.org-1:ibAY5FD+XWLzbLr8fxK6n8fL9zZe7jS+gYeyxyWYK5c=",
]

let Text/concatSep = https://prelude.dhall-lang.org/Text/concatSep

let conf_line = \(name: Text) -> \(things: List Text) ->
  ''
  ${name} = ${Text/concatSep " " things}
  ''

in
  ''
  # Warning: this file is generated from nix.conf.dhall"

  '' ++
  conf_line "substituters" substituters ++
  conf_line "trusted-public-keys" trusted-public-keys ++
  conf_line "experimental-features" ["nix-command flakes"]
