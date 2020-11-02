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
(require 'emms-info-tinytag)

(emms-all)
(emms-default-players)
(emms-cache 1)

(global-set-key (kbd "<f12>") 'emms-smart-browse)
;; "+" 按起来麻烦，用 = 省事。
(define-key emms-playlist-mode-map (kbd "=") 'emms-volume-raise)

(setq emms-directory (concat user-emacs-directory "emms"))

(unless (file-directory-p emms-directory)
  (make-directory
   (file-name-as-directory emms-directory) t))

(setq emms-source-file-default-directory
      (or emms-source-file-default-directory
          (concat (file-name-as-directory emms-directory) "music")))

(unless (file-directory-p emms-source-file-default-directory)
  (make-directory
   (file-name-as-directory
    emms-source-file-default-directory)
   t))

;; 设定EMMS主模式为 Playlist 模式
(setq emms-playlist-default-major-mode 'emms-playlist-mode)

;; 修复播放完后的 BUG
(setq emms-player-next-function 'emms-next)

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
(emms-mode-line -1)

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
(setq emms-info-tinytag-python-name "python3")
(setq emms-info-functions '(emms-info-tinytag))

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

;; 设置 emms-tag-editor
(setq emms-tag-editor-tags
      '((info-artist      . "a")
        (info-albumartist . "A")
        (info-composer    . "C")
        (info-performer   . "p")
        (info-title       . "t")
        (info-album       . "l")
        (info-tracknumber . "n")
        (info-year        . "y")
        (info-genre       . "g")
        (info-date        . "d")
        (info-note        . "c")))

(setq emms-tag-editor-formats
      (let* ((tags (mapcar 'car emms-tag-editor-tags))
             (default (emms-tag-editor-make-format (remove 'info-date tags))))
        `(("mp3" . ,default)
          ("ogg" . ,(emms-tag-editor-make-format (remove 'info-year tags)))
          ("flac" . ,(emms-tag-editor-make-format (remove 'info-year tags)))
          ("default" . ,default))))

(setq emms-tag-editor-tagfile-functions
      '(("mp3" "mid3v2"
         ((info-artist      . "a")
          (info-title       . "t")
          (info-album       . "A")
          (info-tracknumber . "T")
          (info-year        . "y")
          (info-genre       . "g")
          (info-note        . "c")
          ;;--------------------------
          (info-albumartist . "-TPE2")
          (info-composer    . "-TCOM")
          (info-performer   . "-TOPE")
          (info-date        . "-TDAT")))
        ("ogg" . emms-tag-editor-tag-ogg)
        ("flac" . emms-tag-editor-tag-flac)))

(defvar eh-emms-tag-charset "gbk")

(defun eh-emms-fix-tag-charset ()
  (interactive)
  (if (emms-mark-has-markedp)
      (eh-emms-fix-marked-track-tag-charset)
    (eh-emms-fix-track-tag-charset (emms-tag-editor-track-at))))

(defun eh-emms-fix-track-tag-charset (track)
  (if (eq (emms-track-get track 'type) 'file)
      (let ((coding-system-for-read 'utf-8)
            (file (emms-track-name track)))
        (when (zerop
               (call-process
                "mid3iconv" nil nil nil "-e" eh-emms-tag-charset file))
          (message "Update: %s" file)
          (run-hook-with-args 'emms-info-functions track)))
    (message "Only support files.")))

(defun eh-emms-fix-marked-track-tag-charset ()
  (let ((tracks (emms-mark-mapcar-marked-track
                 'emms-tag-editor-track-at t)))
    (if (null tracks)
        (message "No track marked!")
      (dolist (track tracks)
        (eh-emms-fix-track-tag-charset track)))))

;; * Footer
(provide 'eh-emms)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-emms.el ends here
