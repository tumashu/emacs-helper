;;; eh-guix.el --- Tumashu's emacs configuation    -*- lexical-binding: t; -*-

;; * Header
;; Copyright (c) 2011-2019, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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

;; This emacs config will set a basic guix develop environment for new
;; guix developer, it deal with guile %load-path with the help of
;; GUIX_PACKAGE_PATH environment and 'guix repl' command.

;; ** When it will useful?

;; 1. You just use guile to develop guix and do not develop other
;;    projects.
;; 2. You use 'guix pull' to update guix and other channels
;;    frequently, and do not like to deal with guile %load-path manually.
;; 3. You just edit your own guix config or change ONE package file at
;;    a time in guix.git, this package file is relatively independent.
;; 4. You do not ADD a new scm file to guix.git.

;; ** How to start?

;; 1. Load this config to emacs
;; 2. Open guix file, for example: emacs-xyz.scm in guix.git.
;; 3. M-x: geiser
;; 4. Type 'C-c . u' key to use current guix module.
;; 5. Type 'C-c C-b' to geiser-eval-buffer current buffer.
;; 6. Edit... type 'C-M-x' or 'C-x C-e' to see result... edit again ...

;;; Code:
(require 'geiser)
(require 'geiser-guile)
(require 'guix)
(require 'magit)
(require 'guix-devel)

;; ** Enable guix-devel-mode
(add-hook 'scheme-mode-hook #'guix-devel-mode)

;; ** Get guix checkout directory.
(defun eh-guix-dir ()
  "Get guix checkout directory created by guix pull."
  (when-let* ((dir (expand-file-name "~/.cache/guix/checkouts/"))
              (directory-p (file-directory-p dir)))
    (file-name-as-directory
     (cl-find-if
      (lambda (dir)
        (file-exists-p
         (expand-file-name "guix.scm" dir)))
      (directory-files dir t)))))

;; ** Load guix copyright.el
(when-let* ((dir (eh-guix-dir))
            (file (expand-file-name "etc/copyright.el" dir))
            (exists-p (file-exists-p file)))
  (load-file file)
  ;; (add-hook 'after-save-hook 'copyright-update)
  (defvar copyright-names-regexp)
  (setq copyright-names-regexp
        (format "%s <%s>" user-full-name user-mail-address)))

;; ** Load guix tempel snippets.
(when-let* ((dir (eh-guix-dir))
            (path (expand-file-name "etc/snippets/tempel/*" dir)))
  (defvar tempel-path)
  (add-to-list 'tempel-path path))

;; * Footer
(provide 'eh-guix)

;;; eh-guix.el ends here
