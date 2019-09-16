#
# Dropbox CLI.
#
# https://github.com/dropbox/dbxcli
#
# FIXME: Clashes with installation of hub binary from github/hub.
#
self: super:
with builtins;
with super;
rec {
  dbxcli = buildGoModule rec {
    name = "dbxcli-${version}";
    version = "3.0.0";

    src = fetchFromGitHub {
      owner = "dropbox";
      repo = "dbxcli";
      rev = "v${version}";
      sha256 = "0sxfmjg26s86m5xa9nbj8287kg12kygxw6gggahal6v7zjhwcvaz";
    };

    modSha256 = "05pkcm68i6ig4jhz70sj3gq1vk7xp27cvl0sixys3dsg9krrm0y3";

    subPackages = [ "." ];

    meta = with lib; {
      description = "A command line client for Dropbox built using the Go SDK";
      homepage = https://github.com/dropbox/dbxcli;
      license = licenses.asl20;
      maintainers = with maintainers; [ steshaw ];
      platforms = platforms.linux ++ platforms.darwin;
    };
  };
}
