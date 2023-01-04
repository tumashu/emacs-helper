;;; eh-complete.el --- Tumashu's emacs complete configuation    -*- lexical-binding: t; -*-

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
;; ** company-mode
(require 'company)
(global-set-key (kbd "M-/") 'company-complete)
(define-key company-active-map (kbd "M-i") 'company-complete-selection)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "M-n") 'company-select-next)
(define-key company-active-map (kbd "M-p") 'company-select-previous)

(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(setq company-show-numbers t)
(setq company-tooltip-limit 10)
(setq company-tooltip-minimum-width 40)
(setq company-echo-delay 0)
(setq company-global-modes
      '(not message-mode git-commit-mode eshell-mode))

(setq company-dabbrev-char-regexp "[[:word:]_:@.-]+")
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)
(setq company-require-match nil)
(setq company-dabbrev-minimum-length 2)

(setq company-backends
      '((company-capf company-files) ;company-dabbrev 经常让 emacs 卡死
        (company-dabbrev-code company-gtags company-etags
                              company-keywords)))
(setq company-transformers
      '(company-sort-by-occurrence))

(setq company-frontends
      '(company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              (lambda (_)
                (global-company-mode)))
  (global-company-mode))

(defun eh-company-dabbrev--prefix (orig-fun)
  "取消中文补全"
  (let ((string (pyim-char-before-to-string 0)))
    (if (pyim-string-match-p "\\cc" string)
        nil
      (funcall orig-fun))))

(advice-add 'company-dabbrev--prefix
            :around #'eh-company-dabbrev--prefix)

;; ** company-posframe
(require 'company-posframe)
(setq company-posframe-show-indicator nil)
(setq company-posframe-show-metadata nil)
(setq company-posframe-quickhelp-delay 1)
(company-posframe-mode 1)

;; * Footer
(provide 'eh-complete)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-complete.el ends here
