{
  services.borgmatic.enable = true;

  services.borgmatic.configurations.mail = {
    source_directories = [ "/data/mail" ];
    repositories = [{
      label = "borgbase-mail";
      path = "ssh://smsua417@smsua417.repo.borgbase.com/./repo";
    }];
    # TODO: use agenix
    # https://github.com/sinavir/sinavir/blob/52b7677ece30d6fd3f7f0ecd894dbf6b3c9f6da0/config/machines/schedar/backups.nix#L21
    ssh_command = "ssh -i /root/keys/borg/id_ed25519";
    encryption_passcommand = "cat /root/keys/borg/mail.pass";
    source_directories_must_exist = true;
    keep_daily = 7; # Keep 7 daily archives
    keep_weekly = 4; # Keep 4 weekly archives
    keep_monthly = -1; # Keep at least one archive for each month
  };

  services.borgmatic.configurations.nextcloud = {
    source_directories = [
      "/data/nextcloud/config"
      "/data/nextcloud/data"
    ];
    repositories = [{
      label = "borgbase-nextcloud";
      path = "ssh://n7ixpw3b@n7ixpw3b.repo.borgbase.com/./repo";
    }];
    ssh_command = "ssh -i /root/keys/borg/id_ed25519";
    encryption_passcommand = "cat /root/keys/borg/nextcloud.pass";
    source_directories_must_exist = true;
    keep_daily = 7; # Keep 7 daily archives
    keep_weekly = 4; # Keep 4 weekly archives
    keep_monthly = -1; # Keep at least one archive for each month

    # TODO: backup database
  };
}
