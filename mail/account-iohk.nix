{
  # Corresponding secrets API entry (as also used by emacs)
  # secret-tool store --label='Mail iohk.io' user sebastian.nagel@iohk.io host smtp.gmail.com port 465
  #
  # NOTE: use a Google "app password"
  accounts.email.accounts.iohk = {
    primary = false;
    flavor = "gmail.com";
    address = "sebastian.nagel@iohk.io";
    realName = "Sebastian Nagel";
    userName = "sebastian.nagel@iohk.io";
    passwordCommand = "secret-tool lookup user sebastian.nagel@iohk.io host smtp.gmail.com";
    maildir.path = "iohk.io";
    imap.tls.enable = true;
    smtp.tls.enable = true;
    offlineimap.enable = true;
    msmtp.enable = true;
  };
}
