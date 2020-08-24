;;; eh-installer.el --- Emacs-helper installer   -*- lexical-binding: t; -*-

;; * Header
;; Copyright (c) 2011-2019, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.3

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

;; * 简介                                                  :README:
;;  这个文件是tumashu个人专用的emacs配置文件，emacs中文用户可以参考。

;;; Code:

;; * 代码                                                                 :code:
(require 'package)

(defun eh-current-directory ()
  (file-name-directory
   (buffer-file-name)))

(setq package-archives
      `(("eh-elpa" . ,(concat (eh-current-directory) "elpa/"))))

(package-initialize)

(defun eh-packages-install (packages)
  (let ((refreshed nil))
    (when (not package-archive-contents)
      (package-refresh-contents)
      (setq refreshed t))
    (dolist (pkg packages)
      (when (and (not (package-installed-p pkg))
                 (assoc pkg package-archive-contents))
        (unless refreshed
          (package-refresh-contents)
          (setq refreshed t))
        (ignore-errors
          (package-install pkg))))))

(defun eh-get-mirror-packages (mirror-directory)
  "Return all package's name in a mirror directory: MIRROR-DIRECTROY."
  (when (and mirror-directory (stringp mirror-directory))
    (let ((file (concat (file-name-as-directory mirror-directory)
                        "archive-contents")))
      (when (file-exists-p file)
        (mapcar #'car (cdr (read (with-temp-buffer
                                   (insert-file-contents file)
                                   (buffer-string)))))))))

(defvar eh-config-template
  "
%% ------------------------------------
%% Add by emacs-helper installer

(add-to-list 'load-path %S)
(require 'emacs-helper)

%% ------------------------------------
")

(defun eh-installer ()
  (interactive)
  ;; 安装 elpa 目录下的所有包
  (eh-packages-install
   (eh-get-mirror-packages
    (concat (eh-current-directory) "elpa/")))
  ;; 在 "~/.emacs" 文件中插入配置片断
  (unless (with-temp-buffer
            (insert-file-contents "~/.emacs")
            (goto-char (point-min))
            (search-forward "(require 'emacs-helper)" nil t))
    (append-to-file
     (format (replace-regexp-in-string "%%" ";;" eh-config-template)
             (eh-current-directory)) nil "~/.emacs"))
  (message "Emacs-helper 安装完成，请重新启动 Emacs"))

(eh-installer)

;; * Footer

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-installer.el ends here
