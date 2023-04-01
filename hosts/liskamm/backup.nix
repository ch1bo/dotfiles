{
  services.borgbackup.jobs = {
    pictures = {
      paths = [ "/home/ch1bo/pictures" ];
      doInit = true;
      repo = "zp1865b5@zp1865b5.repo.borgbase.com:repo";
      encryption = {
        mode = "repokey-blake2";
        passCommand = "cat /run/keys/borg/pictures.pass";
      };
      environment = { BORG_RSH = "ssh -i /run/keys/borg/id_ed25519"; };
      compression = "auto,lzma";
      startAt = "daily";
    };

    nextcloud-ch1bo = {
      paths = [ "/data/nextcloud/data/ch1bo/files" ];
      doInit = true;
      repo = "n7ixpw3b@n7ixpw3b.repo.borgbase.com:repo";
      encryption = {
        mode = "repokey-blake2";
        passCommand = "cat /run/keys/borg/nextcloud-ch1bo.pass";
      };
      environment = { BORG_RSH = "ssh -i /run/keys/borg/id_ed25519"; };
      compression = "auto,lzma";
      startAt = "daily";
    };
  };
}
