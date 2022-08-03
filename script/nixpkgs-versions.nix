{ pkg }:
let floop = (builtins.getAttr pkg (import ./pkg-versions.nix { }));
in
map
  (version:
    let
      info = builtins.getAttr version floop;
    in
    {
      version = version;
      channel = info.channel;
      rev = info.rev;
    })
  (builtins.attrNames floop)
