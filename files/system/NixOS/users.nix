{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.steshaw = {
    isNormalUser = true;
    home = "/home/steshaw";
    description = "Steven Shaw";
    extraGroups = [
      "docker"
      "networkmanager"
      "wheel"
    ];
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQM+CdduNpScMe8Uucb5TCVLx3HrXUrTJO8hBkOF8Dy4+IYLFxo6teT8XT4X0+SLJ6gdxPjRPfqmcZwdg051BkaPAh6TkX0zqAeaBFSh3rVmNWV1mxDxYl9X5yWXkaj/kCOpJccz2NrNINYvv4wYHVoVRDg97+RMCdLyzXV2W0sf+J1Mozpj05AAgo6iqUNwo8bHJtekD4UZ6L1Zql3QSwBZvIo2eK9Ir6DPhDMRD0YHLUnFfdLYbGhlqi3qPu8CbEbu+4ptyhlePqvIymdmVwd2VL44SBr5KvlmFuZmhm/LL2b89tb2a2X3RW8do4y1W/wIOwfSeUfXf83zUftTyR steven@steshaw.org" ];
  };
  users.users.debbie = {
    isNormalUser = true;
    home = "/home/debbie";
    description = "Deborah Shaw";
    extraGroups = [ ];
  };
}
