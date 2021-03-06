{
  accounts.email.accounts.franka = {
    primary = true;
    address = "sebastian.nagel@franka.de";
    realName = "Sebastian Nagel";
    userName = "nage_se";
    # secret-tool store --label='Mail FE' email sebastian.nagel@franka.de
    passwordCommand = "secret-tool lookup email sebastian.nagel@franka.de";
    maildir = { path = "franka.de"; };
    imap = {
      host = "mail.franka.de";
      tls.enable = false; # only starttls
    };
    smtp = {
      host = "mail.franka.de";
      tls.enable = true;
    };
    offlineimap = {
      enable = true;
      extraConfig.remote = {
        starttls = true;
        folderfilter = ''
          lambda fn: not(any(
            [ fn.startswith('Public')
            , fn.startswith('KBee')
            , fn.startswith('Kastanienbaum')
            ]))
        '';
      };
    };
  };
}
