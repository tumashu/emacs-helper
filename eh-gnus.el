;;; eh-gnus.el --- Tumashu's gnus configuation file    -*- lexical-binding: t; -*-

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

;; 在 ~/.gnus.el 文件中插入一行：

;; #+BEGIN_EXAMPLE
;; (require 'eh-gnus)
;; (eh-gnus-load)
;; #+END_EXAMPLE

;;; Code:

;; * 代码                                                      :code:
(defcustom eh-gnus-personal-file "~/Gnus/eh-gnus-personal.el"
  "eh-gnus用于存储个人帐号信息的文件"
  :group 'emacs-helper
  :type 'file)

(defun eh-gnus-load ()
  (interactive)
  (if (file-exists-p (expand-file-name eh-gnus-personal-file))
      (progn
        ;; 加载个人帐号信息。
        (load eh-gnus-personal-file)
        ;; 加载 gnus 可共享的配置
        (use-package eh-gnus-common :ensure nil))
    (message "eh-gnus个人帐号文件不存在，eh-gnus启动失败!!!")))

(global-set-key (kbd "C-x m") #'(lambda ()
                                  (interactive)
                                  (unless (gnus-alive-p)
                                    (gnus))
                                  (gnus-msg-mail)))

;; * Footer
(provide 'eh-gnus)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-gnus.el ends here
