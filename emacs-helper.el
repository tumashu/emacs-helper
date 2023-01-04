;;; emacs-helper.el --- An emacs config collection for Non programmers    -*- lexical-binding: t; -*-

;; * Header
;; Copyright 2016 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.1
;; Package-Requires: ((adaptive-wrap "0.0.1")(aggressive-indent "0.0.1")(cal-china-x "0.0.1")(citre "0.0.1")(cnfonts "0.0.1")(company "0.0.1")(company-posframe "0.0.1")(consult "0.0.1")(eat "0.0.1")(ebdb "0.0.1")(ebdb-i18n-chn "0.0.1")(el2org "0.0.1")(emms "0.0.1")(flycheck "0.0.1")(geiser-guile "0.0.1")(guix "0.0.1")(magit "0.0.1")(marginalia "0.0.1")(markdown-mode "0.0.1")(modus-themes "0.0.1")(orderless "0.0.1")(org-contrib "0.0.1")(org-download "0.0.1")(org-ql "0.0.1")(org-super-agenda "0.0.1")(ox-gfm "0.0.1")(paredit "0.0.1")(pos-tip "0.0.1")(projectile "0.0.1")(pyim "0.0.1")(pyim-basedict "0.0.1")(rainbow-delimiters "0.0.1")(rainbow-mode "0.0.1")(switch-window "0.0.1")(tempel "0.0.1")(vertico "0.0.1")(vundo "0.0.1")(wgrep "0.0.1")(xmlgen "0.0.1")(xr "0.0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; * Emacs-helper 使用说明                                          :README:doc:
;; ** 简介
;; Emacs-helper 是 [[https://github.com/tumashu][Tumashu]] 同学的个人配置，
;; 可以做为 Emacs 中文用户的一个参考。

;; ** 使用方法（以 window 系统为例）
;; 1. 下载 [[https://ftp.gnu.org/gnu/emacs/windows/][Emacs]] .
;; 1. 下载 [[https://github.com/tumashu/emacs-helper/archive/master.zip][emacs-helper 压缩包]] .
;; 2. 将压缩包解压缩到任意一个目录，比如："d:/emacs-helper"
;; 3. 用 emacs 打开 installer 文件： "d:/emacs-helper/eh-installer.el"
;; 4. 运行下面的命令来安装 "d:/emacs-helper/elpa" 目录下所有的包，
;;    并在 "~/.emacs" 文件中插入相应的配置片断。
;;    #+BEGIN_EXAMPLE
;;    M-x eval-buffer
;;    #+END_EXAMPLE
;; 5. 重启 Emacs

;; * 代码                                                                 :code:
(require 'eh-basic)
(require 'eh-theme)
(require 'eh-functions)
(require 'eh-org)
(require 'eh-complete)
(require 'eh-gnus-common)
(require 'eh-misc)
(require 'eh-guix)

;; * Footer
(provide 'emacs-helper)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; emacs-helper.el ends here
