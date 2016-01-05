# PlatformIO Mode
[![MELPA](https://melpa.org/packages/platformio-mode-badge.svg)](https://melpa.org/#/platformio-mode)
[![MELPA Stable](https://stable.melpa.org/packages/platformio-mode-badge.svg)](https://stable.melpa.org/#/platformio-mode)


`platformio-mode` is an Emacs minor mode which allows quick building and uploading of
PlatformIO projects with a few short key sequences.

Code completion can be provided by installing any package compatible with `.clang_complete` files,
such as [irony-mode](https://github.com/Sarcasm/irony-mode).


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
| Clean                   | `C-c i c` |
| Update                  | `C-c i d` |


## Installation

The recommended way to install PlatformIO-Mode is using [MELPA](https://melpa.org/).

### Configuration

`platformio-setup-compile-buffer` is provided to simplify setting up the compilation
buffer. It enables scrolling and ansi-colors.


Here is a sample config using PlatformIO-Mode in conjuction with [company](http://company-mode.github.io/), [irony](https://github.com/Sarcasm/irony-mode), and [flycheck](http://www.flycheck.org/).

```elisp
;; Add the required company backend.
(add-to-list 'company-backends 'company-irony)

;; Enable irony and platformio for all c++ files.
(add-hook 'c++-mode-hook (lambda ()
                           (irony-mode)
                           (irony-eldoc)
                           (platformio-mode)))

;; Use irony's completion functions.
(add-hook 'irony-mode-hook
          (lambda ()
            (define-key irony-mode-map [remap completion-at-point]
              'irony-completion-at-point-async)

            (define-key irony-mode-map [remap complete-symbol]
              'irony-completion-at-point-async)

            (irony-cdb-autosetup-compile-options)))
            
;; Setup irony for flycheck.
(add-hook 'flycheck-mode-hook 'flycheck-irony-setup)

;; Enable scrolling and colours in the compile buffer.
(platformio-setup-compile-buffer)
```






