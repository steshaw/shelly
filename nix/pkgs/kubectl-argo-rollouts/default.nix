{ stdenv, fetchurl }:
stdenv.mkDerivation rec {
  name = "kubectl-argo-rollouts";
  version = "v1.0.2";
  src = fetchurl {
    url = "https://github.com/argoproj/argo-rollouts/releases/download/${version}/kubectl-argo-rollouts-linux-amd64";
    sha256 = "1y9f35qixw602bp55r6zyq2djnl6718yzix54fq8gq3xbcllmd62";
  };
  phases = ["installPhase" "patchPhase"];
  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/kubectl-argo-rollouts
    chmod +x $out/bin/kubectl-argo-rollouts
  '';
}
