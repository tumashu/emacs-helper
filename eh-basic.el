;;; eh-basic.el --- Tumashu's basic emacs configuation

;; * Header
;; Copyright (c) 2011-2019, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.3

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

;; * 代码                                                                 :code:
(require 'cl-lib)

(defun eh-system-open (path &rest args)
  (cond ((string-equal system-type "windows-nt")
         (w32-shell-execute "open" path))
        ((string-equal system-type "darwin")
         (concat "open " (shell-quote-argument path)))
        ((string-equal system-type "gnu/linux")
         (let ((process-connection-type nil))
           (start-process "" nil "xdg-open" path)))))

(defun eh-emacs-open (path &rest args)
  (if (and (string-equal system-type "gnu/linux")
           (functionp 'eaf-open)
           (not (file-directory-p path)))
      (funcall 'eaf-open path)
    (eh-system-open path)))

(defun eh-fileext-match-p (filename exts)
  (cl-find-if
   (lambda (ext)
     (string-match-p (format "\\.%s\\'" ext) filename))
   exts))

(defun eh-find-file (orig-fun &rest args)
  (let ((filename (car args))
        (cmd (symbol-name this-command)))
    (cond ((eh-fileext-match-p
            filename
            '("pdf" "xps" "oxps" "cbz" "epub" "fb2" "fbz" "djvu"
              "jpg" "jpeg" "png" "bmp" "gif" "svg" "webp"
              "avi" "rmvb" "ogg" "mp4" "mkv"))
           (eh-emacs-open filename))
          ((eh-fileext-match-p
            filename
            '("doc" "docx" "xls" "xlsx" "ppt" "pptx" "wps"))
           (eh-system-open filename))
          ((and (or (string-match "^org-" cmd)
                    (string-match "^eh-org-" cmd))
                (file-directory-p filename))
           (eh-system-open filename))
          (t (apply orig-fun args)))))

(dolist (f '(find-file
             find-file-read-only
             find-file-other-window
             find-file-other-frame))
  (advice-add f :around 'eh-find-file))

;; ** 设置 load-path
(defun eh-update-load-path ()
  (interactive)
  (let (dirs)
    (dolist (x '("~" "c:" "d:" "e:" "f:"
                 "/c" "/d" "/e" "/f"
                 "~/storage/shared/"))
      (push (file-name-as-directory
             (concat x "/projects/emacs-packages")) dirs)
      (push (file-name-as-directory
             (concat x "/project/emacs-packages")) dirs))
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (x (directory-files dir t))
          (when (and (file-directory-p x)
                     (not (string-match-p "/\\.$" x))
                     (not (string-match-p "/\\.\\.$" x)))
            (add-to-list 'load-path x)))))))

(eh-update-load-path)

;; ** Full name and Email
(setq user-full-name "Feng Shu")
(setq user-mail-address "tumashu@163.com")

;; ** 启动时默认打开的 buffer.
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
(setq initial-major-mode 'emacs-lisp-mode)
(setq initial-scratch-message
      ";; This is *scratch* buffer.\n\n")

;; ** 使用空格缩进
(setq-default indent-tabs-mode nil)

;; ** 关闭 beep
(setq visible-bell t)

;; ** 让 *scratch* buffer 无法删除
(defun eh-unkillable-scratch-buffer ()
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        (insert initial-scratch-message)
        nil)
    t))

(add-hook 'kill-buffer-query-functions
          #'eh-unkillable-scratch-buffer)

(defun eh-termux-p ()
  "Test termux environment."
  (and (equal system-configuration "aarch64-unknown-linux-android")
       (not (display-graphic-p))))

;; ** 设置 emacs 包管理器
(require 'package)
(unless package--initialized
  (package-initialize))

(defun eh-elpa-directory ()
  "返回 emacs-helper 内置 elpa 镜像的目录。"
  (file-name-as-directory
   (concat (file-name-directory
            (locate-library "eh-basic.el"))
           "elpa/")))

(setq package-archives
      `(("eh-elpa" . ,(eh-elpa-directory))
        ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
        ("org-cn"   . "http://elpa.emacs-china.org/org/")
        ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))

;; ** 设置 elpa-mirror
(require 'elpa-mirror)

(defun eh-elpa-mirror ()
  (interactive)
  (let* ((directory (file-name-as-directory (eh-elpa-directory)))
         (recreate-directory
          (yes-or-no-p (format "重新创建目录：%S ? " directory))))
    (elpamr-create-mirror-for-installed directory recreate-directory)))

;; ** 设置 Charset
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(when (eq system-type 'windows-nt)
  (set-language-environment "Chinese-GBK")
  (set-selection-coding-system 'gbk-dos)
  (set-next-selection-coding-system 'gbk-dos)
  (set-clipboard-coding-system 'gbk-dos))

;; ** 保存文件之前，删除无用的空格

;; Window 系统下关闭 Emacs 时，强制确认，防止误操作。
(when (eq system-type 'windows-nt)
  (setq confirm-kill-emacs 'yes-or-no-p))

;; ** 关闭自动备份功能，我有 git :-)
(setq make-backup-files nil)

(require 'whitespace)
;; 使用下面这一行配置后，org-mode 的源代码总是莫名其妙的
;;     (add-hook 'before-save-hook #'whitespace-cleanup)
;; 更改，这会导致生成的 diff 相当乱。

;;   (add-hook 'before-save-hook
;;             #'(lambda ()
;;                 (delete-trailing-whitespace)))

;; ** 设置 recentf
(require 'recentf)
(global-set-key (kbd "C-x f") 'recentf-open-files)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-saved-items 99)
(setq recentf-max-menu-items 99)
(setq recentf-show-file-shortcuts-flag nil)
(setq recentf-exclude
      '("COMMIT" "autoloads" "archive-contents" "eld" "newsrc"
        ".recentf" "emacs-font-size.conf"
        "pyim-dcache-.*"))
;; 自动保存recentf文件。
(add-hook 'find-file-hook #'recentf-save-list)

;; ** 设置 ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x b") 'ibuffer)

;; ** 设置区域选择快捷键
(global-unset-key (kbd "C-x C-x"))
(global-set-key (kbd "C-x <SPC>") 'set-mark-command)
;; QQ 将会在 emacs 之前捕捉 M-w 快捷键，记得取消。
;; 另外绑定 C-c w 作为备用。
(global-set-key (kbd "C-c w") 'kill-ring-save)
(global-set-key (kbd "C-x C-x C-x") 'exchange-point-and-mark)
(global-set-key (kbd "C-x C-x <SPC>") 'rectangle-mark-mode)

;; ** kill 当前 buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; ** 关闭 tool-bar
(tool-bar-mode -1)

;; ** 关闭 menu-bar
(menu-bar-mode 0)

;; ** 关闭 scroll-bar
(scroll-bar-mode -1)

;; ** 配对括号高亮显示
(require 'paren)
(show-paren-mode 1)

;; ** 括号自动匹配
(require 'elec-pair)
(electric-pair-mode 1)

;; ** switch-window
(require 'switch-window)

(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

(unless (display-graphic-p)
  (setq switch-window-shortcut-appearance 'asciiart))
(setq switch-window-increase 6)
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-input-style 'minibuffer)

;; ** 设置 swiper , ivy-mode and counsel
(require 'counsel)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x f") 'counsel-recentf)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-c y") 'counsel-yank-pop)

;; Default setting is not suitable for GuixSD.
(setq counsel-linux-app-format-function
      #'counsel-linux-app-format-function-name-only)

(require 'swiper)
;; I use "C-x C-f" to open file, so bind "C-f" to
;; `ivy-immediate-done' is very useful.
(define-key ivy-minibuffer-map (kbd "C-f") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "S-<return>") 'ivy-immediate-done)
(define-key ivy-minibuffer-map [mouse-1] 'ignore)
(define-key ivy-minibuffer-map [mouse-3] 'ignore)
(define-key ivy-minibuffer-map [mouse-4] 'ivy-next-line)
(define-key ivy-minibuffer-map [mouse-5] 'ivy-previous-line)

(ivy-mode 1)
(setq ivy-count-format ""
      ivy-use-virtual-buffers t
      ivy-format-functions-alist
      '((t . ivy-format-function-arrow))
      ivy-display-style 'fancy
      ivy-use-selectable-prompt t)

(setq ivy-initial-inputs-alist
      '((org-refile . "")
        (org-agenda-refile . "")
        (org-capture-refile . "")
        (counsel-M-x . "")
        (counsel-describe-function . "")
        (counsel-describe-variable . "")
        (counsel-org-capture . "")
        (Man-completion-table . "")
        (woman . "")))

(require 'ivy-hydra)

;; ** 设置 amx
(require 'amx)
(amx-initialize)

;; ibuffer
(require 'ibuffer)
;; Make ibuffer work well with termux
(define-key ibuffer-name-map [(mouse-2)] 'ibuffer-mouse-toggle-mark)
(define-key ibuffer-name-map [(mouse-1)] 'ibuffer-mouse-visit-buffer)

;; ** VC
(require 'vc)
;; I use magit instead of vc :-)
(setq vc-handled-backends nil)
(setq vc-ignore-dir-regexp ".*")

;; ** elisp setting
(require 'elisp-mode)

;; ** wdired and dired-ranger
(require 'dired)
(require 'wdired)
(require 'dired-ranger)

;; ** 设置拼音输入法
(require 'pyim)
(global-set-key (kbd "M-j") 'pyim-convert-string-at-point)
(define-key minibuffer-local-map (kbd "C-<return>") 'pyim-convert-cregexp-at-point)

(setq default-input-method "pyim")

;; 只有在 termux 环境下用 emacs thread 生成 dcache。
(setq pyim-dcache-prefer-emacs-thread (eh-termux-p))

;; 使用全拼
(setq pyim-default-scheme 'quanpin)

;; pyim 探针设置
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-dynamic-english
                pyim-probe-isearch-mode
                pyim-probe-program-mode
                pyim-probe-org-structure-template))

(setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))

;; 开启拼音搜索功能
(pyim-isearch-mode 1)

;; 设置选词框的绘制方式
(if (and (display-graphic-p)
         (>= emacs-major-version 26))
    (setq pyim-page-tooltip 'posframe)
  (setq pyim-page-tooltip 'popup))

;; 显示5个候选词。
(setq pyim-page-length 5)

;; 添加 pinyin 搜索中文功能
(setq eh-org-minibuffer-local-map
      (let* ((map (make-sparse-keymap)))
        (set-keymap-parent map minibuffer-local-map)
        map))

;; emacs 启动时加载 pyim 词库
;; (add-hook 'emacs-startup-hook
;;           #'(lambda ()
;;               (pyim-restart-1 t)))

(require 'pyim-basedict)
(pyim-basedict-enable)

(defun eh-ivy-cregexp (str)
  (let ((a (ivy--regex-plus str))
        (b (let ((case-fold-search nil))
             (pyim-cregexp-build str))))
    (if (and a (stringp a) (string> a ""))
        (concat a "\\|" b)
      a)))

(setq ivy-re-builders-alist
      '((t . eh-ivy-cregexp)))

;; liberime
(when (eq system-type 'gnu/linux)
  (require 'liberime nil t)
  (with-eval-after-load "liberime"
    (require 'liberime-config)
    (add-hook 'after-liberime-load-hook
              (lambda ()
                (liberime-select-schema "luna_pinyin_simp")))
    (setq pyim-default-scheme 'rime)))

;; ** calendar
(require 'calendar)
(setq calendar-week-start-day 0) ; 一周第一天，0表示星期天, 1表示星期一

(setq calendar-month-name-array
      ["一月" "二月" "三月" "四月" "五月" "六月"
       "七月" "八月" "九月" "十月" "十一月" "十二月"])

(setq calendar-day-name-array
      ["周日" "周一" "周二" "周三" "周四" "周五" "周六"])

(defun eh-org-chinese-anniversary (year lunar-month lunar-day &optional mark)
  (if year
      (let* ((d-date (diary-make-date lunar-month lunar-day year))
             (a-date (calendar-absolute-from-gregorian d-date))
             (c-date (calendar-chinese-from-absolute a-date))
             (cycle (car c-date))
             (yy (cadr c-date))
             (y (+ (* 100 cycle) yy)))
        (diary-chinese-anniversary lunar-month lunar-day y mark))
    (diary-chinese-anniversary lunar-month lunar-day year mark)))

;; ** calendar 相关
;; 使用英文 day-name, 而不是中文： “星期XX”
(setq system-time-locale "C")

(require 'parse-time)
(setq parse-time-months
      (append '(("yi" . 1) ("er" . 2) ("san" . 3)
                ("si" . 4) ("wu" . 5) ("liu" . 6)
                ("qi" . 7) ("ba" . 8) ("jiu" . 9)
                ("shi" . 10) ("shiyi" . 11) ("shier" . 12))
              parse-time-months))

(setq parse-time-weekdays
      (append '(("zri" . 0) ("zyi" . 1) ("zer" . 2) ("zsan" . 3)
                ("zsi" . 4) ("zwu" . 5) ("zliu" . 6))
              parse-time-weekdays))

(require 'cal-china-x)
(defvar eh-calendar-holidays nil)
(setq eh-calendar-holidays
      '(;;公历节日
        (holiday-fixed 1 1 "元旦")
        (holiday-fixed 2 14 "情人节")
        (holiday-fixed 3 8 "妇女节")
        (holiday-fixed 3 14 "白色情人节")
        (holiday-fixed 4 1 "愚人节")
        (holiday-fixed 5 1 "劳动节")
        (holiday-fixed 5 4 "青年节")
        (holiday-float 5 0 2 "母亲节")
        (holiday-fixed 6 1 "儿童节")
        (holiday-float 6 0 3 "父亲节")
        (holiday-fixed 9 10 "教师节")
        (holiday-fixed 10 1 "国庆节")
        (holiday-fixed 10 24 "程序员节")
        (holiday-fixed 12 25 "圣诞节")
        ;; 农历节日
        (holiday-lunar 1 1 "春节" 0)
        (holiday-lunar 1 2 "春节" 0)
        (holiday-lunar 1 3 "春节" 0)
        (holiday-lunar 1 15 "元宵节" 0)
        (holiday-solar-term "清明" "清明节")
        (holiday-solar-term "小寒" "小寒")
        (holiday-solar-term "大寒" "大寒")
        (holiday-solar-term "立春" "立春")
        (holiday-solar-term "雨水" "雨水")
        (holiday-solar-term "惊蛰" "惊蛰")
        (holiday-solar-term "春分" "春分")
        (holiday-solar-term "谷雨" "谷雨")
        (holiday-solar-term "立夏" "立夏")
        (holiday-solar-term "小满" "小满")
        (holiday-solar-term "芒种" "芒种")
        (holiday-solar-term "夏至" "夏至")
        (holiday-solar-term "小暑" "小暑")
        (holiday-solar-term "大暑" "大暑")
        (holiday-solar-term "立秋" "立秋")
        (holiday-solar-term "处暑" "处暑")
        (holiday-solar-term "白露" "白露")
        (holiday-solar-term "秋分" "秋分")
        (holiday-solar-term "寒露" "寒露")
        (holiday-solar-term "霜降" "霜降")
        (holiday-solar-term "立冬" "立冬")
        (holiday-solar-term "小雪" "小雪")
        (holiday-solar-term "大雪" "大雪")
        (holiday-solar-term "冬至" "冬至")
        (holiday-lunar 5 5 "端午节" 0)
        (holiday-lunar 8 15 "中秋节" 0)
        (holiday-lunar 7 7 "七夕情人节" 0)
        (holiday-lunar 12 8 "腊八节" 0)
        (holiday-lunar 9 9 "重阳节" 0)))
(setq calendar-holidays eh-calendar-holidays)

;; * Footer
(provide 'eh-basic)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8-unix
;; End:

;;; eh-basic.el ends here
