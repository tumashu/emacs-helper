;;; eh-termux.el --- Tumashu's emacs termux configuation   -*- lexical-binding: t; -*-

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
(require 'eh-basic)
(require 'eh-functions)
(require 'eh-org)
(require 'eh-theme)

(defun eh-termux-create-button (button)
  (when (or (not (nth 2 button))
            (and (functionp (nth 2 button))
                 (funcall (nth 2 button))))
    (if (nth 1 button)
        (list (propertize (car button)
                          'mouse-face 'mode-line-highlight
                          'keymap
                          (let ((map (make-sparse-keymap)))
                            (define-key map [mode-line mouse-1] (nth 1 button))
                            map))
              " ")
      (list (nth 0 button) " "))))

(defun eh-termux-create-buttons (buttons)
  (mapcar #'eh-termux-create-button buttons))

(defun eh-termux-ibuffer ()
  (interactive)
  (ibuffer nil "ibuffer-termux" nil nil nil nil '((" " name)))
  (with-current-buffer "ibuffer-termux"
    (setq-local ibuffer-display-summary nil)
    (setq-local ibuffer-use-header-line nil))
  (ibuffer-update nil)
  (message ""))

(defun eh-termux-default-mode-line ()
  (eh-termux-create-buttons
   '(("[M-x]" counsel-M-x)
     ("[切]" eh-termux-ibuffer)
     ("[大]" delete-other-windows)
     ("[存]" save-buffer
      (lambda () (and (buffer-file-name)
                      (buffer-modified-p))))
     ("[C-c C-c]" org-ctrl-c-ctrl-c
      (lambda () (eq major-mode 'org-mode)))
     ("%b"))))

(defun eh-termux-capture-mode-line ()
  (eh-termux-create-buttons
   '(("Capture:")
     ("[完成]" org-capture-finalize)
     ("[取消]" org-capture-kill)
     ("[归整]" org-capture-refile)
     ("[CC键]" org-ctrl-c-ctrl-c))))

(defun eh-termux-ivy-mode-line ()
  (eh-termux-create-buttons
   '(("[C-g]" minibuffer-keyboard-quit)
     ("[C-']" ivy-avy))))

(defun eh-termux-create-mode-line ()
  (cond ((and (boundp 'org-capture-mode)
              org-capture-mode)
         (eh-termux-capture-mode-line))
        ((active-minibuffer-window)
         (eh-termux-ivy-mode-line))
        (t (eh-termux-default-mode-line))))

(defun eh-termux-enable ()
  (interactive)
  (setq-default truncate-lines t)
  (setq-default header-line-format nil)
  (setq-default mode-line-format
                '(:eval (eh-termux-create-mode-line)))
  (add-hook 'buffer-list-update-hook
            #'(lambda ()
                (setq header-line-format nil)
                (setq mode-line-format
                      '(:eval (eh-termux-create-mode-line)))))
  (add-hook 'org-capture-mode-hook
            #'(lambda ()
                (setq-local header-line-format nil))))

;; * Footer
(provide 'eh-termux)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-termux.el ends here
