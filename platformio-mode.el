;;; platformio-mode.el --- PlatformIO integration

;; Author: Zach Massia <zmassia@gmail.com>
;; URL: https://github.com/zachmassia/platformio-mode
;; Version: 0.1.0
;; Package-Requires: ((projectile "0.13.0"))

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
;; A minor mode which allows quick building and uploading of PlatformIO
;; projects with a few short key sequences.
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


(defun platformio-conditionally-enable ()
  "Enable `platformio-mode' only when a `platformio.ini' file is present in project root."
  (condition-case nil
      (when (projectile-verify-file "platformio.ini")
        (platformio-mode 1))
    (error nil)))


;;; Internal functions
(defun platformio--run-cmd (target)
  "Call `platformio ... TARGET' in the root of the project."
  (let ((default-directory (projectile-project-root))
        (cmd (concat "platformio -f -c emacs " target)))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (projectile-project-buffer-p (current-buffer)
                                                      default-directory)))
    (compilation-start cmd)))


;;; User commands
(defun platformio-build ()
  "Build PlatformIO project."
  (interactive)
  (platformio--run-cmd "run"))

(defun platformio-upload ()
  "Upload PlatformIO project to device."
  (interactive)
  (platformio--run-cmd "run -t upload"))

(defun platformio-programmer-upload ()
  "Upload PlatformIO project to device using external programmer."
  (interactive)
  (platformio--run-cmd "run -t program"))

(defun platformio-spiffs-upload ()
  "Upload SPIFFS to device."
  (interactive)
  (platformio--run-cmd "run -t uploadfs"))

(defun platformio-clean ()
  "Clean PlatformIO project."
  (interactive)
  (platformio--run-cmd "run -t clean"))

(defun platformio-update ()
  "Update installed PlatformIO libraries."
  (interactive)
  (platformio--run-cmd "update"))


;;; Minor mode
(defvar platformio-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") #'platformio-build)
    (define-key map (kbd "u") #'platformio-upload)
    (define-key map (kbd "p") #'platformio-programmer-upload)
    (define-key map (kbd "s") #'platformio-spiffs-upload)
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
   ["Upload using External Programmer" platformio-programmer-upload]
   ["Upload SPIFFS" platformio-spiffs-upload]
   "--"
   ["Clean Project" platformio-clean]
   ["Update Project Libraries" platformio-update]))


;;;###autoload
(define-minor-mode platformio-mode
  "PlatformIO integration for Emacs."
  :lighter " PlatformIO"
  :keymap platformio-mode-map
  :group 'platformio
  :require 'platformio)

(provide 'platformio-mode)
;;; platformio-mode.el ends here
