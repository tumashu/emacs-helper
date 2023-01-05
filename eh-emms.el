;;; eh-emms.el --- Tumashu's emacs configuation   -*- lexical-binding: t; -*-

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
(require 'emms)
(require 'emms-setup)
(require 'emms-info-exiftool)
(require 'emms-info-native)
(require 'emms-browser)

(emms-all)
(emms-default-players)
(emms-cache 1)

(global-set-key (kbd "<f12>") 'emms-smart-browse)
(global-set-key (kbd "<f11>") 'emms-next)
(global-set-key (kbd "<f10>") 'emms-pause)

;; "+" 按起来麻烦，用 = 省事。
(define-key emms-playlist-mode-map (kbd "=") 'emms-volume-raise)
(define-key emms-browser-mode-map (kbd "=") 'emms-volume-raise)

(setq emms-directory (concat user-emacs-directory "emms"))

(setq emms-source-file-default-directory
      (or emms-source-file-default-directory
          (concat (file-name-as-directory emms-directory) "music")))

(unless (file-directory-p emms-directory)
  (make-directory
   (file-name-as-directory emms-directory) t))

(unless (file-directory-p emms-source-file-default-directory)
  (make-directory
   (file-name-as-directory
    emms-source-file-default-directory)
   t))

;; 设定EMMS主模式为 Playlist 模式
(setq emms-playlist-default-major-mode 'emms-playlist-mode)

;; 修复播放完后的 BUG
(setq emms-player-next-function 'emms-next-noerror)

;; 设定音轨初始化信息
(add-to-list 'emms-track-initialize-functions
             'emms-info-initialize-track)

;; 关闭 EMMS 信息异步模式
(setq emms-info-asynchronously nil)

;; 设定 EMMS 启动列表循环播放
(setq emms-repeat-playlist t)

;; 排序方法: 艺术家 -> 专辑 -> 序号
(setq emms-playlist-sort-function
      'emms-playlist-sort-by-natural-order)

;; 在 minibuffer 中显示播放信息 (emms-show)
(setq emms-show-format "正在播放:%s")
(add-hook 'emms-player-started-hook #'emms-show)

;; 不在 mode-line 中显示播放信息
(emms-mode-line-mode -1)

;; 如何显示 track
(setq emms-track-description-function
      #'eh-emms-track-simple-description)

(defun eh-emms-track-simple-description (track)
  (let* ((type (emms-track-type track))
         (string (cond ((eq 'file type)
                        (emms-track-name track))
                       ((eq 'url type)
                        (emms-format-url-track-name (emms-track-name track)))
                       (t (concat (symbol-name type)
                                  ": " (emms-track-name track))))))
    (with-temp-buffer
      (insert (concat "  ♪ " string))
      (eh-emms-wash-buffer)
      (buffer-string))))

;; Playlist 清洗功能
(defvar eh-emms-wash-config nil)

(defun eh-emms-wash-buffer ()
  (dolist (x eh-emms-wash-config)
    (goto-char (point-min))
    (while (re-search-forward
            (if (nth 2 x)
                (nth 0 x)
              (regexp-quote (nth 0 x)))
            nil t)
      (replace-match (propertize (nth 1 x) 'face (text-properties-at (point))) nil t))))

(defun eh-emms-wash-config ()
  (interactive)
  (let ((str1 (read-string "请输入需清洗的字符串："))
        (str2 (read-string "请输入清洗成的字符串："))
        (regexp-p (y-or-n-p "是否将待清洗的字符串作为 regexp 处理? ")))
    (when (> (length str1) 0)
      (push (list str1 str2 regexp-p) eh-emms-wash-config))
    (customize-save-variable
     'eh-emms-wash-config
     (delete-dups eh-emms-wash-config))
    (with-current-emms-playlist
      (eh-emms-wash-buffer))))

;; 显示歌词
(emms-lyrics 1)

;; Track information
;; 1. emms-info-exiftool 使用 perl 程序 exiftool, 速度比较慢但兼容性很好。
;; 2. emms-info-native 是用 elisp 实现的，安装比较省事，速度还可以。

;; (setq emms-info-functions '(emms-info-exiftool))
(setq emms-info-functions '(eh-emms-info-native))

(defun eh-emms-info-native (track)
  (let ((file (emms-track-name track)))
    (condition-case nil
        (emms-info-native track)
      (error (message "EMMS: 无法从 %S 中读取音乐信息." file)))))

;; 设置 Playlist 的显示方式
(setq emms-last-played-format-alist
      '(((emms-last-played-seconds-today) . "%H:%M")
        (604800                           . "%H:%M")
        ((emms-last-played-seconds-month) . "%d")
        ((emms-last-played-seconds-year)  . "%m-%d")
        (t                                . "%Y")))

;; 设置 EMMS 浏览器, 默认显示方式为: 显示所有
(emms-browser-set-filter (assoc "EVERYTHING" emms-browser-filters))

;; filter: 显示所有
(emms-browser-make-filter "EVERYTHING" 'ignore)

;; filter: 只显示文件
(emms-browser-make-filter
 "ALL-FILES"
 (emms-browser-filter-only-type 'file))

;; filter: 最近一个星期播放的
(emms-browser-make-filter
 "LAST-WEEK"
 (emms-browser-filter-only-recent 7))

;; filter: 最近一个月都没有播放的
(emms-browser-make-filter
 "LAST-MONTH-NOT-PLAYED"
 (lambda (track)
   (not (funcall (emms-browser-filter-only-recent 30) track))))

;; 设置 emms browser 和 playlist 中音乐的显示格式

(defun eh-emms-browser-format-line (orig_func bdata &optional target)
  (let* ((type (emms-browser-bdata-type bdata))
         (level (emms-browser-bdata-level bdata))
         (indent (emms-browser-make-indent level))
         (str (funcall orig_func bdata target)))
    ;; 删除原来函数添加的 " ", 不然 playlist 对齐不好处理。
    (when (and (eq type 'info-title)
               (not (string= indent "")))
      (setq str (string-remove-prefix " " str)))
    str))

(advice-add 'emms-browser-format-line :around #'eh-emms-browser-format-line)

(setq emms-browser-info-year-format      "%i+ %n")
(setq emms-browser-info-genre-format     "%i+ %n")
(setq emms-browser-info-performer-format "%i+ %n")
(setq emms-browser-info-composer-format  "%i+ %n")
(setq emms-browser-info-artist-format    "%i* %n")
(setq emms-browser-info-album-format     "%i- %n")
(setq emms-browser-info-title-format     "%i♪ %n")
(setq emms-browser-playlist-info-year-format      "%i%n")
(setq emms-browser-playlist-info-genre-format     "%i%n")
(setq emms-browser-playlist-info-performer-format "%i%n")
(setq emms-browser-playlist-info-composer-format  "%i%n")
(setq emms-browser-playlist-info-artist-format    "%i%n")
(setq emms-browser-playlist-info-album-format     "%i%n")
(setq emms-browser-playlist-info-title-format     "  ♪ %n")

(defun eh-emms-browser-wash-playlist (&optional _)
  "简化 playlist, emms-browser 默认生成的 playlist 有缩进，看起来太花。"
  (with-current-emms-playlist
    (eh-emms-wash-buffer)
    (goto-char (point-max))))

(add-hook 'emms-browser-tracks-added-hook #'eh-emms-browser-wash-playlist)

;; 使用类似 org-mode 的快捷键
(define-key emms-browser-mode-map (kbd "<tab>") 'emms-browser-toggle-subitems-recursively)
(define-key emms-browser-mode-map (kbd "<backtab>") 'emms-browser-toggle-subitems-recursively)
(define-key emms-browser-mode-map (kbd "C-c C-c") 'emms-browser-add-tracks-and-play)

;; 加载 playlist 历史
(add-hook 'after-init-hook #'emms-history-load)

(setq emms-tag-editor-pipe-config
      '(("处理MP3中文标签乱码 (mid3iconv -e gbk <file>)"
         :command "mid3iconv"
         :arguments ("-e" "gbk" name))
        ("Test1 (echo -a <name> -c <name>)"
         :command "echo"
         :arguments (("-a" name) ("-b" name1) ("-c" name)))
        ("Test2 (echo <artist>-<title>)"
         :command "echo"
         :arguments
         (lambda (track)
           (list (format "%s-%s"
                         (emms-track-get track 'info-artist)
                         (emms-track-get track 'info-title)))))))

;; * Footer
(provide 'eh-emms)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-emms.el ends here
