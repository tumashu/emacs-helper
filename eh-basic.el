;;; eh-basic.el --- Tumashu's basic emacs configuation     -*- lexical-binding: t; -*-

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

(defun eh-directory ()
  "返回 emacs-helper 所在的目录。"
  (file-name-directory
   (locate-library "eh-basic.el")))

(defun eh-system-open (path &rest _args)
  (let ((path (expand-file-name path)))
    (cond ((string-equal system-type "windows-nt")
           (with-no-warnings (w32-shell-execute "open" path)))
          ((string-equal system-type "darwin")
           (concat "open " (shell-quote-argument path)))
          ((string-equal system-type "gnu/linux")
           (let ((process-connection-type nil))
             (start-process "" nil "xdg-open" path))))))

(defun eh-file-extension-match-p (file-name extensions)
  (cl-find-if
   (lambda (extension)
     (string-match-p (format "\\.%s\\'" extension) file-name))
   extensions))

(defun eh-find-file (orig-fun &rest args)
  (let ((file-name (car args))
        ;; this-command may be a lambda
        (cmd (ignore-errors (symbol-name this-command))))
    (cond ((and (file-exists-p file-name)
                (eh-file-extension-match-p
                 file-name
                 '("doc" "docx" "xls" "xlsx" "ppt" "pptx" "wps"
                   "pdf" "xps" "oxps" "cbz" "epub" "fb2" "fbz" "djvu"
                   "jpg" "jpeg" "png" "bmp" "gif" "svg" "webp"
                   "avi" "rmvb" "mp4" "mkv" "mp3" "ogg")))
           (eh-system-open file-name))
          ((and cmd
                (or (string-match "^org-" cmd)
                    (string-match "^eh-org-" cmd)
                    (string-match "^eh-share2computer-" cmd))
                (file-directory-p file-name))
           (eh-system-open file-name))
          (t (apply orig-fun args)))))

(dolist (f '(find-file
             find-file-read-only
             find-file-other-window
             find-file-other-frame))
  (advice-add f :around 'eh-find-file))

;; ** Full name and Email
(setq user-full-name "Feng Shu")
(setq user-mail-address "tumashu@163.com")

;; ** 启动时默认打开的 buffer.
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
(setq initial-major-mode 'emacs-lisp-mode)
(setq initial-scratch-message "")

;; ** yes-or-no-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; ** 使用空格缩进
(setq-default indent-tabs-mode nil)

;; ** 关闭 beep
(setq visible-bell t)

;; ** 不使用 dialog
(setq use-dialog-box nil)

;; ** 让全角空格不会显示下划线
(setq nobreak-char-display nil)
;; (setq eh-space (propertize " 　" 'face '(:weight 'bold)))
(defvar eh-space " 　")

;; ** 设置 mode-line
;; 在 mode-line 最后追加一个半角空格，一个全角空格，防止因为字体高度原
;; 因，导致 mode-line 抖动。
(setq mode-line-end-spaces
      '(:eval (if (display-graphic-p) eh-space "-%-")))

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

;; ** kill buffer
(global-set-key (kbd "C-x k") #'kill-current-buffer)

;; ** 设置 emacs 包管理器
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 10)
        ("nongnu" . 9)
        ("melpa" . 8)))

(unless package--initialized
  (package-initialize))

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

;; ** Window 系统下关闭 Emacs 时，强制确认，防止误操作。
(when (eq system-type 'windows-nt)
  (setq confirm-kill-emacs 'yes-or-no-p))

;; ** 关闭自动备份功能，我有 git :-)
(setq make-backup-files nil)

;; ** 保存文件之前，删除无用的空格
;; (require 'whitespace)
;; 使用下面这一行配置后，org-mode 的源代码总是莫名其妙的
;;     (add-hook 'before-save-hook #'whitespace-cleanup)
;; 更改，这会导致生成的 diff 相当乱。

;;   (add-hook 'before-save-hook
;;             #'(lambda ()
;;                 (delete-trailing-whitespace)))

;; ** 设置 recentf
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-saved-items 99)
(setq recentf-max-menu-items 99)
(setq recentf-show-file-shortcuts-flag nil)
(setq recentf-exclude
      '("COMMIT" "autoloads" "archive-contents" "eld" "newsrc"
        ".recentf" "emacs-font-size.conf" "eh-scratch"
        "pyim-dcache-.*"))
;; 自动保存recentf文件。
(add-hook 'find-file-hook #'recentf-save-list)

;; ** 设置区域选择快捷键
(global-unset-key (kbd "C-x C-x"))
(global-set-key (kbd "C-x <SPC>") 'set-mark-command)
;; QQ 将会在 emacs 之前捕捉 M-w 快捷键，记得取消。
;; 另外绑定 C-c w 作为备用。
(global-set-key (kbd "C-c w") 'kill-ring-save)
(global-set-key (kbd "C-x C-x C-x") 'exchange-point-and-mark)
(global-set-key (kbd "C-x C-x <SPC>") 'rectangle-mark-mode)

;; ** 保存 session
;; (desktop-save-mode 1)

;; ** 关闭 tool-bar
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))

;; ** 关闭 scroll-bar
(when (functionp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; ** 启用像素级 scroll
(when (functionp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; ** 解决最大化留缝隙的问题
(setq frame-resize-pixelwise t)

;; ** 加载 bookmark
(require 'bookmark)
(bookmark-maybe-load-default-file)

;; ** 配对括号高亮显示
(require 'paren)
(show-paren-mode 1)

;; ** 括号自动匹配
(require 'elec-pair)
(electric-pair-mode 1)

;; ** 处理长行
(global-so-long-mode 1)

;; ** 处理折行
(toggle-word-wrap 1)
(setq word-wrap-by-category t)

;; ** 启用 menu-bar
(if (> emacs-major-version 28)
    (menu-bar-mode -1)
  (menu-bar-mode 1))

;; ** 启用右键菜单
(when (functionp 'context-menu-mode)
  (context-menu-mode 1))

;; ** 减少 C-g 时的闪烁
(setq ring-bell-function 'ignore)

;; ** switch-window
(require 'switch-window)

(unless (display-graphic-p)
  (setq switch-window-shortcut-appearance 'asciiart))
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-input-style 'minibuffer)
(global-set-key (kbd "C-x o") 'switch-window)

;; ** tab-line
(require 'tab-line)
(global-tab-line-mode 1)
(setq tab-line-close-tab-function #'kill-buffer)
(setq tab-line-tab-name-truncated-max 22)
(setq tab-line-tab-name-ellipsis "…")
(setq tab-line-tab-name-function
      #'tab-line-tab-name-truncated-buffer)

(defun eh-tab-line-format (orig_func)
  "在 tab-line 的最后添加一个全角空格，防止 tab-line 抖动。"
  (list (funcall orig_func) eh-space))

(advice-add 'tab-line-format :around #'eh-tab-line-format)

;; ** save history
(require 'savehist)
(savehist-mode 1)

;; ** minibuffer and completing-read
(require 'simple)
(require 'minibuffer)
(setq completion-auto-help t)
(setq completion-auto-select nil)
(setq completion-show-help nil)
(setq completion-show-inline-help t)
(setq completions-max-height 15)
(setq completions-format 'one-column)
(setq enable-recursive-minibuffers t)
(setq history-delete-duplicates t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(defun eh-crm-indicator (args)
  (cons (concat "[CRM] " (car args))
        (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'eh-crm-indicator)

;; ** Vertico
(require 'vertico)
(require 'vertico-directory)
(vertico-mode 1)
(define-key vertico-map "\r" #'vertico-directory-enter)
(define-key vertico-map "\d" #'vertico-directory-delete-char)
(define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

;; ** orderless
(require 'orderless)
(require 'pyim-cregexp)
(setq completion-styles '(basic orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(defun eh-orderless-regexp (orig_func component)
  (let ((result (funcall orig_func component)))
    (pyim-cregexp-build result)))

(advice-add 'orderless-regexp :around #'eh-orderless-regexp)

;; ** consult
(require 'consult)
(setq consult-preview-key nil)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x f") 'consult-recent-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c y") 'consult-yank-pop)

;; ** VC
(require 'vc)
(require 'vc-hg)
(setq vc-handled-backends '(Hg Git))
(setq vc-hg-revert-switches '("--no-backup"))

;; ** ChangeLog 命令
(require 'add-log)
(setq add-log-dont-create-changelog-file t)
;; 大多数情况，我不会手动编辑 ChangeLog 文件，防止误操作。
(setq change-log-default-name "01-dont-edit-changelog")

;; ** elisp setting
(require 'elisp-mode)

;; ** Indent
(electric-indent-mode -1)

;; ** wdired
(require 'dired)
(require 'wdired)

;; ** 设置拼音输入法
(require 'pyim)
(require 'pyim-cregexp-utils)

(global-set-key (kbd "M-j") 'pyim-convert-string-at-point)
(define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)

(setq default-input-method "pyim")

;; 使用全拼
(pyim-default-scheme 'quanpin)

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

;; 显示5个候选词。
(setq pyim-page-length 5)

;; 全拼词库 basedict.
(require 'pyim-basedict)
(pyim-basedict-enable)

;; ;; liberime
;; (setq liberime-auto-build t)
;; (require 'liberime nil t)
;; (require 'pyim-liberime nil t)
;; (with-eval-after-load "liberime"
;;   (liberime-try-select-schema "luna_pinyin_simp")
;;   (pyim-default-scheme 'rime-quanpin))

;; ** calendar
(require 'calendar)
(require 'cal-china-x)

;; 使用英文 day-name, 而不是中文： “星期XX”
(setq system-time-locale "C")

;; 一周第一天，0表示星期天, 1表示星期一
(setq calendar-week-start-day 0)

(setq calendar-holidays
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

;; * Footer
(provide 'eh-basic)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-basic.el ends here
