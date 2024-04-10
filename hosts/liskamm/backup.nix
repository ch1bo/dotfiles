{
  services.borgbackup.jobs = {
    mail = {
      paths = [ "/data/mail" ];
      doInit = true;
      repo = "smsua417@smsua417.repo.borgbase.com:repo";
      encryption = {
        mode = "repokey-blake2";
        passCommand = "cat /root/keys/borg/mail.pass";
      };
      environment = { BORG_RSH = "ssh -i /root/keys/borg/id_ed25519"; };
      compression = "auto,lzma";
      startAt = "daily";
      prune.keep = {
        daily = 7; # Keep 7 daily archives
        weekly = 4; # Keep 4 weekly archives
        monthly = -1; # Keep at least one archive for each month
      };
    };

    nextcloud-ch1bo = {
      paths = [ "/data/nextcloud/data/ch1bo/files" ];
      doInit = true;
      repo = "n7ixpw3b@n7ixpw3b.repo.borgbase.com:repo";
      encryption = {
        mode = "repokey-blake2";
        passCommand = "cat /root/keys/borg/nextcloud-ch1bo.pass";
      };
      environment = { BORG_RSH = "ssh -i /root/keys/borg/id_ed25519"; };
      compression = "auto,lzma";
      startAt = "daily";
      prune.keep = {
        daily = 7; # Keep 7 daily archives
        weekly = 4; # Keep 4 weekly archives
        monthly = -1; # Keep at least one archive for each month
      };
    };

    nextcloud-veronika = {
      paths = [ "/data/nextcloud/data/veronika/files" ];
      doInit = true;
      repo = "i287j8j3@i287j8j3.repo.borgbase.com:repo";
      encryption = {
        mode = "repokey-blake2";
        passCommand = "cat /root/keys/borg/nextcloud-veronika.pass";
      };
      environment = { BORG_RSH = "ssh -i /root/keys/borg/id_ed25519"; };
      compression = "auto,lzma";
      startAt = "daily";
      prune.keep = {
        daily = 7; # Keep 7 daily archives
        weekly = 4; # Keep 4 weekly archives
        monthly = -1; # Keep at least one archive for each month
      };
    };
  };
}
