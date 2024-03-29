let substituters = [
  "https://cache.nixos.org",
  "https://cache.iog.io",
  "https://iog.cachix.org",
  "https://iohk.cachix.org",
]

let trusted-substituters = [
  "https://all-hies.cachix.org",
  "https://ghc-nix.cachix.org",
  "https://ghcide-nix.cachix.org",
  "https://hercules-ci.cachix.org",
  "https://iohk.cachix.org",
  "https://nickel.cachix.org",
  "https://niv.cachix.org",
  "https://nix-community.cachix.org",
  "https://nixcache.reflex-frp.org",
  "https://pre-commit-hooks.cachix.org",
  "https://steshaw.cachix.org",
]

let trusted-public-keys = [
  "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=",
--  "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=",
--  "niv.cachix.org-1:X32PCg2e/zAm3/uD1ScqW2z/K0LtDyNV7RdaxIuLgQM=",
--  "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0=",
  "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=",
  "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=",
  "iog.cachix.org-1:nYO0M9xTk/s5t1Bs9asZ/Sww/1Kt/hRhkLP0Hhv/ctY=",
--  "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc=",
--  "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k=",
--  "ghcide-nix.cachix.org-1:ibAY5FD+XWLzbLr8fxK6n8fL9zZe7jS+gYeyxyWYK5c=",
--  "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=",
--  "steshaw.cachix.org-1:l/+4nVnZ9ldk0kCKphhmSsJ5wI3Wr5p4S+c/3mJnPi0=",
--  "ghc-nix.cachix.org-1:wI8l3tirheIpjRnr2OZh6YXXNdK2fVQeOI4SVz/X8nA=",
--  "nickel.cachix.org-1:ABoCOGpTJbAum7U6c+04VbjvLxG9f0gJP5kYihRRdQs=",
]

let Text/concatSep = https://prelude.dhall-lang.org/Text/concatSep

let conf_line = \(name: Text) -> \(things: List Text) ->
  ''
  ${name} = ${Text/concatSep " " things}
  ''

in
  ''
  # Warning: this file is generated from nix.conf.dhall!

  # Unfortunately, an absolute path to netrc is required meaning that we
  # need different locations for linux and macOS.
  netrc-file = ${env:PWD as Text}/netrc

  keep-derivations = true
  keep-outputs = true

  extra-experimental-features = nix-command flakes

  '' ++
  conf_line "substituters" substituters ++
  conf_line "trusted-substituters" trusted-substituters ++
  conf_line "trusted-public-keys" trusted-public-keys
