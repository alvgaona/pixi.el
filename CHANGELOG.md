# Changelog

## [0.2.0] - 2026-01-08

### Features

#### Modeline Configuration
- `pixi-show-modeline` - Toggle modeline indicator visibility
- `pixi-modeline-show-project` - Toggle project name in modeline
- `pixi-modeline-show-environment` - Toggle environment name in modeline
- `pixi-modeline-prefix` - Customize or hide the "pixi" prefix
- `pixi-modeline-enable` / `pixi-modeline-disable` - Interactive commands to toggle modeline

#### Improved Display
- `pixi-info`, `pixi-list-environments`, and `pixi-list-tasks` now display output in a dedicated `*pixi*` buffer instead of the minibuffer

## [0.1.0] - 2025-01-04

Initial release.

### Features

#### Environment Management
- Auto-detect pixi projects (pixi.toml and pyproject.toml with [tool.pixi])
- `pixi-activate` / `pixi-deactivate` - Activate and restore environments
- `pixi-switch-environment` - Switch between environments with completion
- Auto-activate on file open via `find-file-hook` and `dired-mode-hook`
- Modeline indicator showing `[pixi:project:env]`

#### Task Running
- `pixi-run-task` - Run tasks with completion and optional arguments
- `pixi-run` - Run arbitrary commands with `pixi run`
- `pixi-list-tasks` - List available tasks

#### Package Management
- `pixi-add` - Add conda dependency (with optional feature)
- `pixi-add-pypi` - Add PyPI dependency (with optional feature)
- `pixi-remove` - Remove dependency
- `pixi-install` - Run `pixi install`
- `pixi-update` - Update dependencies
- `pixi-upgrade` - Self-update pixi
- `pixi-clean` - Cleanup environments

#### Workspace Commands
- `pixi-workspace-channel-add` / `remove` / `list`
- `pixi-workspace-platform-add` / `remove` / `list`
- `pixi-workspace-feature-list`

#### Integrations
- Eglot/LSP auto-restart on environment change

#### Hooks
- `pixi-activate-hook` - Runs after environment activation
- `pixi-deactivate-hook` - Runs after environment deactivation
- `pixi-pre-run-hook` - Runs before task execution
