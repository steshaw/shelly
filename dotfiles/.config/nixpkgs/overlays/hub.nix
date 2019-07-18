# https://github.com/github/hub/archive/v2.12.2.tar.gz
self: super:
with builtins;
with super;
rec {
  hub = buildGoModule rec {
    name = "hub-${version}";
    version = "2.12.2";

    src = fetchFromGitHub {
      owner = "github";
      repo = "hub";
      rev = "v${version}";
      sha256 = "0sxfmjg26s86m5xa9nbj8287kg12kygxw6gggahal6v7zjhwcvaz";
    };

    modSha256 = "05pkcm68i6ig4jhz70sj3gq1vk7xp27cvl0sixys3dsg9krrm0y3";

    subPackages = [ "." ];

    meta = with lib; {
      description = "A command-line tool that makes git easier to use with GitHub";
      homepage = https://hub.github.com/;
      license = licenses.mit;
#      maintainers = with maintainers; [ steshaw ];
      platforms = platforms.linux ++ platforms.darwin;
    };
  };
}
