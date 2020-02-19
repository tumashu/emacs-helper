;;; emacs-helper.el --- An emacs config collection for Non programmers

;; * Header
;; Copyright 2016 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5"))

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
(require 'eh-functions)
(require 'eh-org)
(require 'eh-complete)
(require 'eh-theme)
(require 'eh-misc)

;; * Footer
(provide 'emacs-helper)

;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:

;;; emacs-helper.el ends here
