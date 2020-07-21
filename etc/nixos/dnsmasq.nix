{ config, ... }:
{
  services.dnsmasq = {
    enable = true;
    servers = [
      # LAN
      "192.168.0.1"

      # CloudFlare
      "1.1.1.1"
      "1.0.0.1"
    ];
    extraConfig = ''
      address=/.localhost/127.0.0.1
    '';
  };
}
