# Zen Browser Keyboard Shortcuts Configuration

This module extends the [zen-browser-flake](https://github.com/0xc000022070/zen-browser-flake) to add declarative keyboard shortcuts configuration.

## Features

- Declarative keyboard shortcuts via Nix configuration
- Override only the shortcuts you want to customize
- All other shortcuts keep their Zen Browser defaults
- Automatic synchronization on system rebuild
- Type-safe configuration with validation
- Optional version pinning to detect breaking changes after Zen Browser updates

## Usage

### Basic Configuration

Modify specific keyboard shortcuts in your Zen Browser profile. When you declare a shortcut override:
- Specify the `id` to identify which shortcut to modify
- Provide the complete key binding (key/keycode, modifiers)
- Identity fields (`action`, `group`, etc.) are preserved from Zen's defaults

```nix
programs.zen-browser.profiles.default = {
  # Other profile settings...

  keyboardShortcuts = [
    # Change workspace 1 shortcut to use Meta+1 (Super/Windows/Command key)
    {
      id = "zen-workspace-switch-1";
      key = "1";
      modifiers.meta = true;
    }

    # Disable the quit shortcut to prevent accidental closes
    {
      id = "key_quitApplication";
      disabled = true;
    }
  ];
};
```

**Important**: When you override a shortcut, you completely replace its key binding. The `action`, `group`, and other metadata are preserved from Zen's defaults, but the `key`, `keycode`, `modifiers`, and `disabled` fields are fully replaced with your declaration.

### Version Pinning (Recommended)

To protect against silent breakage when Zen Browser updates its shortcuts, pin to a specific version:

```nix
programs.zen-browser.profiles.default = {
  keyboardShortcutsVersion = 1;  # Current version from about:config

  keyboardShortcuts = [
    # Your overrides...
  ];
};
```

How it works:
1. Open `about:config` in Zen Browser and search for `zen.keyboard.shortcuts.version`
2. Set `keyboardShortcutsVersion` to that number in your config
3. If Zen Browser updates and changes the shortcuts version, activation will fail with a clear error
4. You can then review the new shortcuts and update your configuration accordingly

**Benefits:**
- Prevents silent breakage when Zen Browser changes default shortcuts
- Forces you to review changes before they're applied
- Avoids scenarios where your overrides reference removed shortcut IDs

**Note:** This is optional but highly recommended if you rely on specific keyboard shortcuts.

## Finding Existing Shortcuts

Your current shortcuts are stored in `~/.zen/<profile>/zen-keyboard-shortcuts.json`. You can:

1. View all shortcuts:
   ```bash
   jq '.shortcuts[] | {id, key, keycode, modifiers, action, group}' ~/.zen/default/zen-keyboard-shortcuts.json
   ```

2. Find shortcuts for a specific action:
   ```bash
   jq '.shortcuts[] | select(.action == "Browser:Back")' ~/.zen/default/zen-keyboard-shortcuts.json
   ```

3. Find shortcuts by group:
   ```bash
   jq '.shortcuts[] | select(.group == "zen-workspace")' ~/.zen/default/zen-keyboard-shortcuts.json
   ```

## Shortcut Configuration

Each shortcut override has the following options:

### Required Fields

- `id` (string): Unique identifier for the shortcut to modify

### Key Binding Fields (fully replaces the binding)

When you specify an override, provide the complete key binding:

- `key` (string): Character key (e.g., "a", "1", "+"). Default: `""` (empty)
- `keycode` (string): Virtual key code for special keys (e.g., "VK_F1", "VK_DELETE"). Default: `null`
- `disabled` (bool): Set to true to disable the shortcut. Default: `false`
- `modifiers` (attrset): Modifier keys configuration. Default: all `false`
  - `control` (bool): Ctrl key
  - `alt` (bool): Alt key
  - `shift` (bool): Shift key
  - `meta` (bool): Meta/Windows/Command key
  - `accel` (bool): Accelerator (Ctrl on Linux/Windows, Cmd on macOS)

### Preserved Fields

These fields are automatically preserved from Zen Browser's default definition:
- `action`, `group`, `l10nId`, `reserved`, `internal`

**Note**: If you don't specify `key`, `keycode`, or `modifiers`, they default to empty/false/null. To disable a shortcut without changing its binding, just set `disabled = true`.

## Finding Shortcut IDs

To modify a shortcut, you need to know its `id`. You can find all shortcut IDs in `~/.zen/default/zen-keyboard-shortcuts.json` after running Zen Browser at least once.

Common shortcut IDs:
- `zen-workspace-switch-1` through `zen-workspace-switch-10`: Workspace switching
- `zen-workspace-forward`, `zen-workspace-backward`: Workspace navigation
- `zen-split-view-horizontal`, `zen-split-view-vertical`, `zen-split-view-grid`: Split view
- `zen-compact-mode-toggle`: Compact mode
- `key_quitApplication`: Quit application
- `key_close`: Close tab
- `key_newNavigatorTab`: New tab
- `key_reload`: Reload page
- `goBackKb`, `goForwardKb`: Browser navigation

## Examples

### Workspace Switching with Super+Number

```nix
keyboardShortcuts = [
  {
    id = "zen-workspace-switch-1";
    key = "1";
    modifiers.meta = true;  # Super/Windows/Command key
  }
  {
    id = "zen-workspace-switch-2";
    key = "2";
    modifiers.meta = true;
  }
];
```

### Disable a Default Shortcut

```nix
keyboardShortcuts = [
  {
    id = "key_quitApplication";
    disabled = true;  # Disables Ctrl+Q/Cmd+Q to prevent accidental quit
  }
];
```

### Change Key Binding

```nix
keyboardShortcuts = [
  {
    id = "key_reload";
    keycode = "VK_F5";  # Change reload to F5
  }
];
```

### Change Modifiers

```nix
keyboardShortcuts = [
  {
    id = "key_newNavigatorTab";
    # Change new tab from Ctrl+T to Alt+T
    key = "t";  # Must re-specify the key
    modifiers.alt = true;  # Other modifiers default to false
  }
];
```

## Activation

After modifying your configuration:

1. Run Zen Browser at least once to generate the shortcuts file (if you haven't already)
2. Apply the changes in one of two ways:
   - **Full rebuild**: `nixos-rebuild switch` or `home-manager switch`
   - **Quick re-apply** (without rebuild): `systemctl start home-manager-${USER}.service`
     - This re-runs the activation scripts including keyboard shortcuts updates
     - Useful if you only changed shortcuts in your already-built configuration
3. During activation:
   - The script runs automatically and updates the shortcuts JSON
   - If the shortcuts file doesn't exist yet, the script exits gracefully (shortcuts will be applied after first Zen Browser launch)
   - If version checking is enabled, activation will fail if versions don't match
4. Restart Zen Browser to load the changes

## Troubleshooting

### Shortcuts not applying
- Make sure you've run Zen Browser at least once to create the initial shortcuts file
- Check the JSON file exists: `cat ~/.zen/default/zen-keyboard-shortcuts.json`
- Verify JSON is valid: `jq empty ~/.zen/default/zen-keyboard-shortcuts.json`
- Make sure Zen Browser is fully closed when rebuilding

### Version mismatch error
If you see "Zen Browser keyboard shortcuts version mismatch!" during activation:

1. This means Zen Browser was updated and the shortcuts schema changed
2. Check the new version: `grep 'zen.keyboard.shortcuts.version' ~/.zen/default/prefs.js`
3. Review new shortcuts: `cat ~/.zen/default/zen-keyboard-shortcuts.json`
4. Update your config:
   ```nix
   keyboardShortcutsVersion = <new-version>;
   ```
5. Review and update your shortcuts overrides if needed
6. Rebuild

This is intentional protection against silent breakage!

### Checking your overrides
```bash
# View shortcuts for a specific ID
jq '.shortcuts[] | select(.id == "zen-workspace-switch-1")' ~/.zen/default/zen-keyboard-shortcuts.json
```

### Resetting to defaults
Remove your overrides from the Nix configuration and rebuild, or delete `~/.zen/default/zen-keyboard-shortcuts.json` and restart Zen Browser to regenerate defaults.
