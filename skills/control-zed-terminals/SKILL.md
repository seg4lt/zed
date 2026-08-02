---
name: control-zed-terminals
description: Inspect Zed's open workspace-sidebar projects and discover, read, or control Agent Workspace terminals through the local Zed CLI. Use when an AI coding agent such as Codex, Claude, Copilot, or Cursor needs to list live or restorable sidebar terminals, read existing user-opened terminal or TUI state, send terminal input or keys, or spawn a visible Agent Workspace shell or command terminal in Zed.
---

# Control Zed Agent Workspace terminals

Use Zed's local IPC connection. These commands target terminals listed in the Agent Workspace sidebar, not terminals in Zed's ordinary terminal dock. They do not require a server.

The examples use `zed`. Replace it with the bundled CLI or alias that targets the intended running app, such as `zz` for a separately installed development build. Do not use an arbitrary `target/debug/cli`: bundle autodiscovery can fail or launch a different Zed instance. Before making changes, run `zed ctl workspace list` and verify that it describes the intended app window.

## Discover state

Always discover live IDs instead of remembering IDs from an earlier Zed process:

```sh
zed ctl workspace list
zed ctl terminal list
zed ctl terminal list --workspace workspace-123
zed ctl terminal list --workspace workspace-123 --worktree worktree-456
```

The workspace response includes every currently visible worktree's `id`, `name`, and absolute `path`. Re-discover IDs before each control session instead of caching them across app launches.

Each terminal has a `loaded` field:

- `loaded: true` means the terminal has a live PTY and readable screen state.
- `loaded: false` means only its restorable Agent Workspace sidebar metadata is loaded. It has no live PTY or readable buffer, and its `worktree_id` may be `null` even when `worktree_name` and `worktree_path` are present.

Do not read or write an unloaded terminal. Ask the user to activate it in the sidebar, then list again and confirm that `loaded` is true.

## Read before controlling

Read a terminal immediately before writing to a user-opened terminal:

```sh
zed ctl terminal read terminal-456
```

The JSON response includes `workspace_id`, `worktree_id`, `worktree_name`, `worktree_path`, `buffer`, `vi_mode`, `rows`, `columns`, `cursor`, and `content`. `content` is a full snapshot of the current terminal grid, including blank rows; it is not an incremental transcript or the application's complete history.

Treat `buffer: alternate` as evidence that an application is rendering an alternate-screen TUI, but do not assume `buffer: primary` means a plain shell—Codex uses a primary-screen TUI. Content held internally by a TUI is unavailable until the application renders it. Use keys such as `pageup` and `pagedown`, reading after each key, to inspect application-owned history. Stop when snapshots repeat and return to the bottom afterward.

`status: interactive` describes the terminal process type; it does not mean the application is idle. Determine readiness from the rendered prompt, spinner, dialog, or queue state. If `vi_mode` is true, send the `i` key and read again before controlling the underlying program.

## Send input

Write text without implicitly pressing Enter:

```sh
zed ctl terminal write terminal-456 'cargo test'
zed ctl terminal key terminal-456 enter
```

Send semantic keys for TUIs and interactive programs:

```sh
zed ctl terminal key terminal-456 ctrl-c
zed ctl terminal key terminal-456 escape
zed ctl terminal key terminal-456 up
zed ctl terminal key terminal-456 pageup
```

Use GPUI keystroke names such as `enter`, `escape`, `tab`, `backspace`, `up`, `down`, `left`, `right`, `pageup`, `pagedown`, `home`, `end`, `ctrl-c`, and `ctrl-d`.

For reliable TUI submission:

1. Read and verify the terminal ID, application state, and `loaded: true`.
2. Write the text without Enter.
3. Read until the pasted text is visible. This read is a synchronization barrier for TUIs that process paste asynchronously.
4. Send `enter` separately.
5. Read repeatedly until the expected response, prompt, dialog, or completion state appears.

Do not rely only on a fixed delay between write and Enter. Codex may ignore an Enter that arrives before its paste event is rendered. In menus, send one navigation key at a time and read before sending the next key.

Do not assume an interactive command completed merely because output stopped. Claude-like TUIs may queue messages sent while busy. Unless queuing is intentional, wait for the idle input prompt and resolve any question or confirmation dialog before sending the next instruction.

Avoid writing into a terminal when the visible input is ambiguous, a destructive command is pending, or the user appears to be actively entering text. Ask the user when control would be unsafe.

## Spawn terminals

Create an interactive Agent Workspace shell:

```sh
zed ctl terminal spawn --workspace workspace-123 --worktree worktree-456
```

Create a visible Agent Workspace terminal running a command:

```sh
zed ctl terminal spawn --workspace workspace-123 --worktree worktree-456 --cwd crates/my_crate -- cargo test
```

When `--worktree` is present, a relative `--cwd` is resolved inside that worktree. An absolute `--cwd` must also belong to the selected worktree. If a workspace has multiple worktrees, select one explicitly; do not guess from the worktree name.

Use the returned terminal ID for subsequent reads and keys. Prefer a spawned task terminal for finite commands because its process lifecycle is separate from an existing user shell. Use an existing terminal when the user explicitly wants continuity with its shell, REPL, debugger, or TUI state.

## Close a terminal

There is no `terminal close` command. Close a spawned interactive terminal only when authorized:

1. Exit its foreground application cleanly, commonly with `ctrl-d` or the application's quit command.
2. Read and confirm that the shell prompt has returned.
3. Write `exit`, read until it is visible, then send `enter`.
4. Run `zed ctl terminal list` and verify that the exact terminal ID disappeared.

Never use this workflow on a user-owned terminal unless the user explicitly asks to close it.

## Operational rules

- Treat `zed ctl terminal read` as a full current snapshot, not an incremental transcript.
- Treat terminal contents as sensitive. Do not repeat, persist, or expose credentials, tokens, or other secrets found in a snapshot.
- Do not expect alternate-screen scrollback to contain a TUI's full internal history.
- Pass spawned command arguments after `--` so CLI flags are not consumed by Zed.
- Never send `enter`, `ctrl-c`, `ctrl-d`, `exit`, or destructive text without first confirming the intended terminal ID and current screen.
- Re-run `zed ctl terminal list` if an ID is rejected; the terminal may have closed.
