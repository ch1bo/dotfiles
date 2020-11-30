{
  accounts.email.accounts.ncoding = {
    primary = true;
    address = "sebastian.nagel@ncoding.at";
    aliases = ".*@ncoding.at";
    realName = "Sebastian Nagel";
    userName = "sebastian.nagel@ncoding.at";
    # secret-tool store --label='Mail' email sebastian.nagel@ncoding.at
    passwordCommand = "secret-tool lookup email sebastian.nagel@ncoding.at";
    maildir = { path = "ncoding.at"; };
    imap = {
      host = "mail.ncoding.at";
      tls.enable = true;
    };
    smtp = {
      host = "mail.ncoding.at";
      port = 587; # TODO switch to 465
      tls.enable = true;
    };
    # TODO gpg
    offlineimap.enable = true;
    msmtp.enable = true;
  };
}
