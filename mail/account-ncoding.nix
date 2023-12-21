{
  # NOTE: Corresponding secrets API entry (as also used by emacs)
  # secret-tool store --label='Mail ncoding.at' sebastian.nagel@ncoding.at host mail.ncoding.at port 465
  accounts.email.accounts.ncoding = {
    primary = true;
    address = "sebastian.nagel@ncoding.at";
    aliases = [ ".*@ncoding.at" ];
    realName = "Sebastian Nagel";
    userName = "sebastian.nagel@ncoding.at";
    passwordCommand = "secret-tool lookup user sebastian.nagel@ncoding.at host mail.ncoding.at";
    maildir = { path = "ncoding.at"; };
    imap = {
      host = "mail.ncoding.at";
      tls.enable = true;
    };
    smtp = {
      host = "mail.ncoding.at";
      port = 465;
      tls.enable = true;
    };
    offlineimap.enable = true;
    msmtp.enable = true;
  };
}
