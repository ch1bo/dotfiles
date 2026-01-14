{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    git
    vim
    # printing & scanning
    cups-filters
    simple-scan
    system-config-printer
    # system monitoring
    lm_sensors
    btop
    powertop
    gnome-disk-utility
    fio
    # random tools
    atool
    bind.dnsutils
    dconf
    graphviz
    gnumake
    nautilus
    nmap
    ntfsprogs
    pavucontrol
    patchelf
    pdftk
    tree
    unzip
    usbutils
    websocat
    # desktop applications
    discord
    eva
    ffmpeg
    gimp
    imagemagick
    inkscape
    ledger-live-desktop
    libreoffice
    mplayer
    obs-studio
    unstable.claude-code
    unstable.portfolio
    xournalpp
  ];
}
