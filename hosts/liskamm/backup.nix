{
  services.borgbackup.jobs = {
    pictures = {
      paths = [ "/home/ch1bo/pictures" ];
      doInit = true;
      repo = "zp1865b5@zp1865b5.repo.borgbase.com:repo";
      encryption = {
        mode = "repokey-blake2";
        passCommand = "cat /run/keys/borg/pictures_passphrase";
      };
      environment = { BORG_RSH = "ssh -i /run/keys/borg/id_ed25519_repo_pictures"; };
      compression = "auto,lzma";
      startAt = "daily";
    };
  };
}
