{ pkgs, ... }:
{
  services.borgmatic.enable = true;

  # TODO: convert back to services.borgbackup.jobs and move to mailserver.nix
  services.borgmatic.configurations.mail = {
    source_directories = [ "/data/mail" ];
    repositories = [{
      label = "borgbase-mail";
      path = "ssh://smsua417@smsua417.repo.borgbase.com/./repo";
    }];
    # TODO: use agenix
    # https://github.com/sinavir/sinavir/blob/52b7677ece30d6fd3f7f0ecd894dbf6b3c9f6da0/config/machines/schedar/backups.nix#L21
    ssh_command = "${pkgs.openssh}/bin/ssh -i /root/keys/borg/id_ed25519";
    encryption_passcommand = "${pkgs.coreutils}/bin/cat /root/keys/borg/mail.pass";
    source_directories_must_exist = true;
    # Backed up daily
    archive_name_format = "{hostname}-nextcloud-{now:%Y-%m-%dT%H:%M:%S}";
    keep_daily = 7; # Keep 7 daily archives
    keep_weekly = 4; # Keep 4 weekly archives
    keep_monthly = -1; # Keep at least one archive for each month
  };
}
