#
# Buildkite CLI.
#
# https://github.com/buildkite/cli
#
# FIXME: Unfortunately, currently fails with the following error:
#
# Verifying github.com/keybase/go-keychain@v0.0.0-20180801170200-15d3657f24fc: checksum mismatch
#         downloaded: h1:l3WyMrNRQatEqPYGtCv3RaPJ+oQaFRWKZmX7GXTtiyw=
#         go.sum:     h1:hsMxTKUbDWam6afrf6TFFBUCCGejgYQzIpwSe14WI4c=
#
# SECURITY ERROR
# This download does NOT match an earlier download recorded in go.sum.
# The bits may have been replaced on the origin server, or an attacker may
# have intercepted the download attempt.
#
# For more information, see 'go help module-auth'.
#
let enable = false; in
self: super:
with builtins;
with super;
rec {
  buildkite-cli = if enable then buildGoModule rec {
    name = "buildkite-cli-${version}";
    version = "1.0.0";

    src = fetchGit {
      url = https://github.com/buildkite/cli;
      rev = "0d216574257dd4e2063d7a2393fa9d7f6de72272";
    };

    modSha256 = "05pkcm68i6ig4jhz70sj3gq1vk7xp27cvl0sixys3dsg9krrm0y3";

    subPackages = [ "." ];

    meta = with lib; {
      description = "A command line interface for Buildkite";
      homepage = https://github.com/buildkite/cli;
      license = licenses.mit;
      maintainers = with maintainers; [ steshaw ];
      platforms = platforms.linux ++ platforms.darwin;
    };
  } else {};
}
