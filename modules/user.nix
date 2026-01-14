# Main user configuration
{ config, options, lib, inputs, ... }:
{
  options.user = lib.mkOption {
    type = lib.types.attrs;
    default = { name = "ch1bo"; };
    description = "Main user name to set up things for; also, all attrs will be used on users.users.$${user.name}";
  };

  config.assertions = [{
    assertion = config.user ? name;
    message = "User name must be set";
  }];

  config.user = {
    description = lib.mkDefault "The primary user";
    isNormalUser = true;
    # TODO: move all except "wheel"
    extraGroups = [ "wheel" "networkmanager" "docker" "scanner" "adbusers" ];
    uid = 1000;
    home = "/home/${config.user.name}";
  };

  # Define config.users.users.$USER using all values from options.user
  config.users.users.${config.user.name} = lib.mkAliasDefinitions options.user;

  config.nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
      allow-import-from-derivation = true
      accept-flake-config = true
    '';
    # Prime nix registry with same nixpkgs as system built from
    registry.nixpkgs.flake = inputs.nixpkgs;

    settings = {
      trusted-users = [ "root" config.user.name ];
      allowed-users = [ "root" config.user.name ];
    };
  };
}
