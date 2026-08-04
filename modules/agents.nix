{ pkgs, inputs, ... }:
let
  jail = inputs.jail-nix.lib.init pkgs;

  claude-code = pkgs.unstable.claude-code;

  # https://github.com/ahujasid/blender-mcp — MCP server exposing a running
  # Blender to the agent. Not in nixpkgs, so packaged from PyPI here rather than
  # fetched at runtime with uvx (which the read-only-store jail can't do). The
  # server talks to Blender over a TCP socket (localhost:9876 by default); the
  # matching Blender add-on has to be installed and enabled in Blender itself.
  blender-mcp = pkgs.python3Packages.buildPythonApplication rec {
    pname = "blender-mcp";
    version = "1.6.5";
    pyproject = true;

    src = pkgs.fetchPypi {
      pname = "blender_mcp";
      inherit version;
      hash = "sha256-cFSkHX8gEx/O3tLeUelywxzvuvwo6qtYZCFFkGZJWJY=";
    };

    build-system = with pkgs.python3Packages; [ setuptools ];

    dependencies =
      with pkgs.python3Packages;
      [
        mcp
        httpx
      ]
      ++ mcp.optional-dependencies.cli;

    # The sdist ships no tests; a bare import is enough of a smoke test.
    pythonImportsCheck = [ "blender_mcp" ];
  };

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
    blender-mcp
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
