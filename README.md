# pixi.el

Emacs integration for [Pixi](https://pixi.sh), the fast cross-platform package manager built on the Conda ecosystem.

## Features

- **Auto-activation**: Automatically activate pixi environments when opening files
- **Environment switching**: Switch between environments with completion
- **Modeline indicator**: Shows `[pixi:project:env]` in the modeline
- **Task running**: Run tasks with completion and arguments
- **Package management**: Add, remove, install, update packages
- **Workspace commands**: Manage channels, platforms, and features
- **Eglot integration**: Auto-restart LSP when switching environments
- **Hooks**: Customize behavior with `pixi-activate-hook`, `pixi-deactivate-hook`, `pixi-pre-run-hook`

## Installation

### Manual

Clone the repository and add to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/pixi.el")
(require 'pixi)
(setq pixi-auto-activate t)
```

### use-package

```elisp
(use-package pixi
  :load-path "/path/to/pixi.el"
  :config
  (setq pixi-auto-activate t))
```

### MELPA (coming soon)

```elisp
(use-package pixi
  :ensure t
  :config
  (setq pixi-auto-activate t))
```

## Usage

### Environment Management

| Command                    | Description                          |
|----------------------------|--------------------------------------|
| `M-x pixi-activate`        | Activate pixi environment            |
| `M-x pixi-deactivate`      | Deactivate and restore original env  |
| `M-x pixi-switch-environment` | Switch to a different environment |
| `M-x pixi-list-environments`  | List available environments       |

### Task Running

| Command              | Description                              |
|----------------------|------------------------------------------|
| `M-x pixi-run-task`  | Run a task with completion and args      |
| `M-x pixi-run`       | Run arbitrary command with `pixi run`    |
| `M-x pixi-list-tasks`| List available tasks                     |

### Package Management

| Command              | Description                              |
|----------------------|------------------------------------------|
| `M-x pixi-add`       | Add a dependency (with optional feature) |
| `M-x pixi-add-pypi`  | Add a PyPI dependency                    |
| `M-x pixi-remove`    | Remove a dependency                      |
| `M-x pixi-install`   | Run `pixi install`                       |
| `M-x pixi-update`    | Update dependencies                      |
| `M-x pixi-upgrade`   | Update pixi itself                       |
| `M-x pixi-clean`     | Cleanup environments                     |

### Workspace Commands

| Command                           | Description              |
|-----------------------------------|--------------------------|
| `M-x pixi-workspace-channel-list` | List channels            |
| `M-x pixi-workspace-channel-add`  | Add a channel            |
| `M-x pixi-workspace-channel-remove` | Remove a channel       |
| `M-x pixi-workspace-platform-list`| List platforms           |
| `M-x pixi-workspace-platform-add` | Add a platform           |
| `M-x pixi-workspace-platform-remove` | Remove a platform     |
| `M-x pixi-workspace-feature-list` | List features            |

## Configuration

```elisp
;; Auto-activate pixi when opening files in a pixi project
(setq pixi-auto-activate t)

;; Custom pixi executable path (auto-detected by default)
(setq pixi-executable "/path/to/pixi")
```

## Hooks

```elisp
;; Save buffers before running tasks
(add-hook 'pixi-pre-run-hook #'save-some-buffers)

;; Custom action after activation
(add-hook 'pixi-activate-hook
          (lambda ()
            (message "Pixi environment activated!")))
```

## Requirements

- Emacs 28.1+
- [Pixi](https://pixi.sh) installed and in PATH

## License

GPL-3.0-or-later
