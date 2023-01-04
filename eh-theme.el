;;; eh-theme.el --- Tumashu's emacs configuation   -*- lexical-binding: t; -*-

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

;; ** modus
(require 'modus-themes)

(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs t)
(setq modus-themes-headings '((t . (1.0))))

(setq modus-themes-common-palette-overrides
      '((fg-heading-1 blue-warmer)
        (fg-heading-2 yellow-cooler)
        (fg-heading-3 cyan-cooler)
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))

(load-theme 'modus-vivendi :no-confim)

;; Tab-line
(dolist (face '(tab-line
                tab-line-tab
                tab-line-highlight
                tab-line-tab-special
                tab-line-tab-inactive
                tab-line-tab-modified
                tab-line-close-highlight
                tab-line-tab-inactive-alternate))
  (custom-set-faces
   `(,face ((,c :weight bold :box nil :height 1.0)))))

;; * Footer
(provide 'eh-theme)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-theme.el ends here
