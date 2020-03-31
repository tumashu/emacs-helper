;;; eh-functions.el --- Tumashu's  emacs functions   -*- lexical-binding: t; -*-


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
(defun eh-wash-text (text &optional fill-width indent)
  "Insert text into a temp buffer and wash it,
if `fill-width' is a number, the temp buffer will be filled to the number,
if `indent' is a number ,the temp buffer will be indent the number,
then the formated buffer will be exported with `buffer-string',
this function  derived from `article-strip-multiple-blank-lines' in
`gnus-art.el'."
  (interactive)
  (with-temp-buffer
    (goto-char (point-min))
    (insert text)
    ;; Make all blank lines empty.
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]	\t]+$" nil t)
      (replace-match "" nil t))

    ;; Replace multiple empty lines with a single empty line.
    (goto-char (point-min))
    (while (re-search-forward "^\n\\(\n+\\)" nil t)
      (delete-region (match-beginning 1) (match-end 1)))

    ;; Remove a leading blank line.
    (goto-char (point-min))
    (if (looking-at "\n")
        (delete-region (match-beginning 0) (match-end 0)))

    ;; Remove a trailing blank line.
    (goto-char (point-max))
    (if (looking-at "\n")
        (delete-region (match-beginning 0) (match-end 0)))

    ;; remove "{"
    (goto-char (point-min))
    (if (looking-at "^[[:space:]	\t]*{")
        (delete-region (match-beginning 0) (match-end 0)))

    ;; remove "}"
    (goto-char (point-max))
    (if (looking-at "}^[[:space:]	\t]*")
        (delete-region (match-beginning 0) (match-end 0)))

    ;; fill buffer
    (when fill-width
      ;; unindent the buffer
      (indent-region (point-min) (point-max) 0)
      ;; unfill the buffer
      (let ((fill-column 100000))
        (fill-region (point-min) (point-max)))
      ;; fill the buffer to fill-width
      (let ((fill-column fill-width))
        (fill-region (point-min) (point-max))))

    ;;indent buffer
    (when indent
      (indent-region (point-min) (point-max) indent))
    (buffer-string)))

(defun eh-dos2unix ()
  "将dos换行方式转换为unix的换行方式,用于去除^M"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun eh-unix2dos ()
  "将unix换行方式转换为dos的换行方式"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

(defun eh-revert-buffer-with-gbk ()
  (interactive)
  (revert-buffer-with-coding-system 'gbk-dos))

(defun eh-revert-buffer-with-utf8 ()
  (interactive)
  (revert-buffer-with-coding-system 'utf-8-unix))

(defun eh-save-buffer-with-gbk (&optional arg)
  (interactive)
  (set-buffer-file-coding-system 'gbk-dos)
  (save-buffer arg))

(defun eh-save-buffer-with-utf8 (&optional arg)
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix)
  (save-buffer arg))

(defun eh-utf8-language-environment ()
  "设置utf-8语言环境"
  (interactive)
  (set-language-environment "UTF-8")
  (set-buffer-file-coding-system 'utf-8-unix)
  (set-clipboard-coding-system 'utf-8-unix)
  (set-file-name-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-next-selection-coding-system 'utf-8-unix)
  (set-selection-coding-system 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix))

(defun eh-gbk-language-environment ()
  "设置gbk语言环境"
  (interactive)
  (set-language-environment "Chinese-GBK")
  (set-buffer-file-coding-system 'gbk-dos)
  (set-clipboard-coding-system 'gbk-dos)
  (set-file-name-coding-system 'gbk-dos)
  (set-keyboard-coding-system 'gbk-dos)
  (set-next-selection-coding-system 'gbk-dos)
  (set-selection-coding-system 'gbk-dos)
  (set-terminal-coding-system 'gbk-dos))

;; * Footer
(provide 'eh-functions)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-functions.el ends here
