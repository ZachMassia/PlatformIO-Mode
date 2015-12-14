;;; platformio-mode.el --- PlatformIO integration for Emacs

;; Author: Zach Massia <zmassia@gmail.com>
;; URL: https://github.com/zachmassia/platformio-mode
;; Version: 0.1.0
;; Package-Requires: ((projectile "20151130.1039"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; TODO Add commentary.
;;
;;; Code:

(require 'projectile)

;;; Customization
(defgroup platformio nil
  "PlatformIO integration for Emacs"
  :prefix "platformio-" :group 'tools
  :link '(url-link :tag "PlatformIO Documentation" "docs.platformio.org/en/latest/")
  :link '(url-link :tag "Submit PlatformIO Issue" "https://github.com/platformio/platformio/issues/"))

(defcustom platformio-mode-keymap-prefix (kbd "C-c i")
  "PlatformIO-mode keymap prefix."
  :group 'platformio
  :type 'string)


;;; User setup functions
(defun platformio-setup-compile-buffer ()
  "Enables ansi-colors and scrolling in the compilation buffer."
  (require 'ansi-color)

  (add-hook 'compilation-filter-hook
            (lambda ()
              (when (eq major-mode 'compilation-mode)
                (ansi-color-apply-on-region compilation-filter-start (point-max)))))

  (setq compilation-scroll-output t))


;;; Internal functions
(defun platformio--run-make-target (target)
  "Call `make TARGET' in the root of the project."
  (let* ((default-directory (projectile-project-root))
         (cmd (concat "make " target)))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (projectile-project-buffer-p (current-buffer)
                                                      default-directory)))
    (compilation-start cmd)))


;;; User commands
(defun platformio-build ()
  "Build PlatformIO project."
  (interactive)
  (platformio--run-make-target "platformio_build"))

(defun platformio-upload ()
  "Upload PlatformIO project to device."
  (interactive)
  (platformio--run-make-target "platformio_upload"))

(defun platformio-clean ()
  "Clean PlatformIO project."
  (interactive)
  (platformio--run-make-target "platformio_clean"))

(defun platformio-update ()
  "Update installed PlatformIO libraries."
  (interactive)
  (platformio--run-make-target "platformio_update"))


;;; Minor mode
(defvar platformio-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") #'platformio-build)
    (define-key map (kbd "u") #'platformio-upload)
    (define-key map (kbd "c") #'platformio-clean)
    (define-key map (kbd "d") #'platformio-update)
    map)
  "Keymap for PlatformIO mode commands after `platformio-mode-keymap-prefix'.")
(fset 'platformio-command-map platformio-command-map)

(defvar platformio-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map platformio-mode-keymap-prefix 'platformio-command-map)
    map)
  "Keymap for PlatformIO mode.")

(easy-menu-change
 '("Tools") "PlatformIO"
 '(["Build Project" platformio-build]
   ["Upload Project" platformio-upload]
   "--"
   ["Clean Project" platformio-clean]
   ["Update Project Libraries" platformio-update]))


;;;###autoload
(define-minor-mode platformio-mode
  "PlatformIO integration for Emacs."
  :lighter "PlatformIO"
  :keymap platformio-mode-map
  :group 'platformio
  :require 'platformio)

(provide 'platformio-mode)
;;; platformio-mode.el ends here
