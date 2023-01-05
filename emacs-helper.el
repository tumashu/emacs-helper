;;; emacs-helper.el --- An emacs config collection for Non programmers    -*- lexical-binding: t; -*-

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
;; Emacs-helper 是 [[https://github.com/tumashu][Tumashu]] 同学的个人配置.

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
