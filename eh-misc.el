;;; eh-misc.el --- Tumashu's emacs configuation    -*- lexical-binding: t; -*-

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

;; * 简介                                                  :README:
;;  这个文件是tumashu个人专用的emacs配置文件，emacs中文用户可以参考。

;;; Code:

;; * 代码                                                      :code:

;; ** emacs-eat
(require 'eat)
(eat-eshell-mode 1)

(defun eh-eat--set-cmd (_ cmd)
  (when (cl-some (lambda (x)
                   (string-search x cmd))
                 '("htop" "nmtui" "mutt" "aptitude"))
    ;; * Color number:
    ;; 0. black 1. red     2. green 3. yellow
    ;; 4. blue  5. magenta 6. cyan  7. white
    (dolist (x '((1 . 124)
                 (4 . 20)))
      (eh-eat--change-color (car x) (cdr x)))))

(defun eh-eat--change-color (old-color-number new-color-number)
  (let ((old-face (intern (format "color-%s-face" old-color-number)))
        (new-face (intern (format "eat-term-color-%s" new-color-number))))
    (setf (eat-term-parameter eat--terminal old-face) new-face)))

(defun eh-eat--eshell-adjust-make-process-args (_ command _args)
  (when eat--terminal
    (eh-eat--set-cmd nil command)))

(defun eh-eat--set-cmd-status (&rest _)
  (dotimes (i 8)
    (eh-eat--change-color i i)))

(advice-add 'eat--set-cmd :after #'eh-eat--set-cmd)
(advice-add 'eat--set-cmd-status :after #'eh-eat--set-cmd-status)
(advice-add 'eat--eshell-adjust-make-process-args :after #'eh-eat--eshell-adjust-make-process-args)

;; ** projectile
(require 'projectile)
(global-set-key (kbd "C-x F") 'projectile-find-file)
(global-set-key (kbd "C-S-s") 'projectile-grep)
(setq projectile-enable-caching nil)
(projectile-mode 1)

;; ** tempel
(require 'tempel)
(setq tempel-path (list (format "%s/*" (expand-file-name "tempel" (eh-directory)))))
(global-set-key (kbd "<f4>") 'tempel-insert)
(define-key tempel-map (kbd "<tab>") #'tempel-next)
(define-key tempel-map (kbd "C-<tab>") #'tempel-previous)
(add-hook 'write-file-functions #'tempel-done)

;; ** syncthing
(require 'eh-basic)
(require 'eh-org)
(defvar eh-org-syncthing-dir "~/syncthing")
(defun eh-org-open-syncthing-dir ()
  (interactive)
  (let ((dir (file-name-as-directory
              (expand-file-name eh-org-syncthing-dir))))
    (when (file-directory-p dir)
      (if (or (derived-mode-p 'org-mode)
              (derived-mode-p 'org-agenda-mode))
          (eh-org-attach-reveal)
        (eh-system-open dir)))))

(global-set-key (kbd "<f1>") 'eh-org-open-syncthing-dir)

;; ** cnfonts
(require 'cnfonts)
(setq-default line-spacing 2)
(setq cnfonts-use-face-font-rescale
      (eq system-type 'gnu/linux))
(setq cnfonts-personal-fontnames
      '(("PragmataPro Mono")))
(cnfonts-mode 1)
(define-key cnfonts-mode-map (kbd "C--") 'cnfonts-decrease-fontsize)
(define-key cnfonts-mode-map (kbd "C-=") 'cnfonts-increase-fontsize)

;; ** el2org
(require 'el2org)

;; ** EPG
(require 'epg)
;; 1. Put the below to your ~/.gnupg/gpg-agent.conf:
;;       allow-emacs-pinentry
;;       allow-loopback-pinentry
;; 2. gpgconf --reload gpg-agent
;; 3. (setq epa-pinentry-mode 'loopback)
;; 4. (pinentry-start)
(setq epg-pinentry-mode 'loopback)

;; ** emms
(require 'eh-emms)

;; ** aggressive-indent
(require 'aggressive-indent)

(defun eh-elisp-setup ()
  ;; 跟踪行尾空格
  (setq show-trailing-whitespace t)
  ;; 自动缩进
  (aggressive-indent-mode))

(add-hook 'emacs-lisp-mode-hook
          #'eh-elisp-setup)

;; ** magit
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)
(define-key magit-status-mode-map (kbd "C-c f") 'magit-format-patch)

;; ** vundo
(require 'vundo)

;; * Footer
(provide 'eh-misc)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-misc.el ends here
