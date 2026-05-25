{ pkgs, inputs, ... }:
let
  jail = inputs.jail-nix.lib.init pkgs;

  claude-code = pkgs.unstable.claude-code;

  # Tools the jailed agent should find on $PATH inside the sandbox.
  agentPackages = with pkgs; [
    bash
    coreutils
    findutils
    gnugrep
    gnumake
    gnused
    git
    curl
    jq
    fd
    ripgrep
    nodejs
    python3
    vim
    gh
    openssh
    direnv
    playwright-mcp
  ];

  # Inside the jail it's safe to skip the per-tool permission prompts and
  # let claude-code run autonomously — escapes are bounded by bwrap.
  dangerousClaude = pkgs.writeShellScriptBin "claude" ''
    exec ${claude-code}/bin/claude --dangerously-skip-permissions --enable-auto-mode "$@"
  '';

  claude = jail "claude" dangerousClaude (
    with jail.combinators;
    [
      network
      (try-fwd-env "TERM")
      mount-cwd
      # Additional tools
      (add-pkg-deps agentPackages)
      # Access the nix store to load cached direnv nix shells
      (readonly "/nix/store")
      # Access to various dotfiles
      (try-readonly (noescape "~/.config"))
      (try-readwrite (noescape "~/.cache"))
      (try-readwrite (noescape "~/.claude"))
      (try-readwrite (noescape "~/.claude.json"))
      (try-readwrite (noescape "~/.cabal"))
    ]
  );

  unsafe-claude = pkgs.writeShellScriptBin "unsafe-claude" ''
    exec ${claude-code}/bin/claude "$@"
  '';
in
{
  environment.systemPackages = [
    claude
    unsafe-claude
  ];
}
