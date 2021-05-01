# PlatformIO Mode
[![MELPA](https://melpa.org/packages/platformio-mode-badge.svg)](https://melpa.org/#/platformio-mode)
[![MELPA Stable](https://stable.melpa.org/packages/platformio-mode-badge.svg)](https://stable.melpa.org/#/platformio-mode)


`platformio-mode` is an Emacs minor mode which allows quick building and uploading of
PlatformIO projects with a few short key sequences.

Code completion can be provided by installing any package compatible with `.ccls` files,
such as [ccls](https://github.com/MaskRay/emacs-ccls). To keep the index up to date, run
`platformio-init-update-workspace` (`C-c i i`) after installing any libraries.


## Dependencies

Currently the only dependency is [Projectile](https://github.com/bbatsov/projectile)
to facilitate running commands in the project root.


## Keymap

The default keymap prefix is `C-c i`.

The following keybindings are currently available.

| Function                | Keymap    |
| --------                | :-------: |
| Build                   | `C-c i b` |
| Upload                  | `C-c i u` |
| Upload using Programmer | `C-c i p` |
| Upload SPIFFS           | `C-c i s` |
| Monitor device          | `C-c i m` |
| Clean                   | `C-c i c` |
| Update                  | `C-c i d` |
| Update Workspace        | `C-c i i` |
| Boards List             | `C-c i l` |


## Installation

The recommended way to install PlatformIO-Mode is using [MELPA](https://melpa.org/).

### Configuration

Here is a sample config using PlatformIO-Mode in conjunction with [ccls](https://github.com/MaskRay/emacs-ccls).

```elisp
(require 'platformio-mode)

;; Enable ccls for all c++ files, and platformio-mode only
;; when needed (platformio.ini present in project root).
(add-hook 'c++-mode-hook (lambda ()
                           (lsp-deferred)
                           (platformio-conditionally-enable)))
```
