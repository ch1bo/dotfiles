{
  # NOTE: Uses OAUTH2 credentials through https://github.com/pdobsan/oama
  # oama authorize book sebastian@book.io --nohint
  # NOTE:                                    ^^^^^^^^ is important!
  accounts.email.accounts.book = {
    primary = false;
    flavor = "gmail.com";
    address = "sebastian@book.io";
    realName = "Sebastian Nagel";
    userName = "sebastian@book.io";
    maildir.path = "book.io";
    imap.tls.enable = true;
    smtp.tls.enable = true;
    offlineimap = {
      enable = true;
      extraConfig.remote = {
        folderfilter = "lambda name: name not in ['[Gmail]/All Mail']";
        oauth2_client_id = "220236565462-v12v9gsp4t7redjdc39ebstfij51gu35.apps.googleusercontent.com";
        oauth2_access_token_eval = "get_token('sebastian@book.io')";
      };
    };
    msmtp = {
      enable = true;
      extraConfig = {
        auth = "oauthbearer";
        passwordeval = "oama access sebastian@book.io";
      };
    };
  };
}
