{ config, ... }:
{
  services.dnsmasq = {
    enable = true;
    servers = [
      # OpenDNS
      "208.67.222.222"
      "208.67.220.220"

      # CloudFlare
      "1.1.1.1"

      # Quad9
      "9.9.9.9"

      # Google.
      "8.8.8.8"
      "8.8.4.4"
    ];
    extraConfig = ''
      address=/.localhost/127.0.0.1
    '';
  };
}
