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

;; ** 设置 load-path (Can not use use-package)
(defun eh-hack-load-path ()
  ;; Delete buildin org's PATH
  (setq load-path
        (cl-remove-if
         #'(lambda (path)
             (string-match "lisp/org$" path))
         load-path))
  ;; Demove property lists to defeat cus-load and remove autoloads
  (mapatoms
   #'(lambda (sym)
       (let ((sym-name (symbol-name sym)))
         (when (string-match "^\\(org\\|ob\\|ox\\)-?" sym-name)
           (setplist sym nil)
           (when (autoloadp sym)
             (unintern sym)))))))

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
            (add-to-list 'load-path x))))))
  (eh-hack-load-path))

(eh-update-load-path)

;; ** Full name and Email (Can not use use-package)
(setq user-full-name "Feng Shu")
(setq user-mail-address "tumashu@163.com")

;; ** 启动时默认打开的 buffer. (Can not use use-package)
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
(setq initial-major-mode 'emacs-lisp-mode)
(setq initial-scratch-message
      ";; This is *scratch* buffer.\n\n")

;; ** 使用空格缩进 (Can not use use-package)
(setq-default indent-tabs-mode nil)

;; ** 关闭 beep (Can not use use-package)
(setq visible-bell t)

;; ** 使用英文 day-name, 而不是中文： “星期XX”
(setq system-time-locale "C")

;; ** 让 *scratch* buffer 无法删除 (Can not use use-package)
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

;; ** 设置 emacs 包管理器 (Can not use use-package)
(require 'package)
(setq use-package-always-ensure nil)
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

;; ** 使用 use-package
(setq use-package-verbose 'errors)
(require 'use-package)

;; ** 设置 elpa-mirror
(use-package elpa-mirror
  :commands (eh-elpa-mirror eh-elpa-mirror-github)
  :config

  (defun eh-elpa-mirror-github ()
    (interactive)
    (let ((directory "~/.eh-elpa-mirror/"))
      (elpamr-create-mirror-for-installed directory t)
      (shell-command
       (concat "cd " directory " && "
               "git init &&"
               "git add -A && "
               "git commit -m \"Update elpa mirror\" && "
               "git push -f git@github.com:tumashu/elpa.git master &"))))

  (defun eh-elpa-mirror ()
    (interactive)
    (let* ((directory (file-name-as-directory (eh-elpa-directory)))
           (recreate-directory
            (yes-or-no-p (format "重新创建目录：%S ? " directory))))
      (elpamr-create-mirror-for-installed directory recreate-directory))))

;; ** 设置 Charset
(use-package mule
  :ensure nil
  :config

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
    (set-clipboard-coding-system 'gbk-dos)))

;; ** 保存文件之前，删除无用的空格
(use-package files
  :ensure nil
  :config

  ;; Window 系统下关闭 Emacs 时，强制确认，防止误操作。
  (when (eq system-type 'windows-nt)
    (setq confirm-kill-emacs 'yes-or-no-p))

  ;; ** 关闭自动备份功能，我有 git :-)
  (setq make-backup-files nil))

(use-package whitespace
  :ensure nil
  :config
  ;; 使用下面这一行配置后，org-mode 的源代码总是莫名其妙的
  ;;     (add-hook 'before-save-hook #'whitespace-cleanup)
  ;; 更改，这会导致生成的 diff 相当乱。
  )

;; (use-package simple
;;   :ensure nil
;;   :config
;;   (add-hook 'before-save-hook
;;             #'(lambda ()
;;                 (delete-trailing-whitespace))))

;; ** 设置 recentf
(use-package recentf
  :ensure nil
  :bind (("C-x f" . recentf-open-files))
  :config
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
  (add-hook 'find-file-hook #'recentf-save-list))

;; ** 设置 ibuffer
(use-package ibuffer
  :ensure nil
  :bind (("C-x b" . ibuffer)))

;; ** 设置区域选择快捷键
(use-package simple
  :ensure nil
  :init (global-unset-key (kbd "C-x C-x"))
  :bind
  (("C-x <SPC>" . set-mark-command)
   ;; QQ 将会在 emacs 之前捕捉 M-w 快捷键，记得取消。
   ;; 另外绑定 C-c w 作为备用。
   ("C-c w" . kill-ring-save)
   ("C-x C-x C-x" . exchange-point-and-mark)))

(use-package rect
  :ensure nil
  :bind (("C-x C-x <SPC>" . rectangle-mark-mode)))

;; ** 关闭 tool-bar
(use-package tool-bar
  :ensure nil
  :bind (("C-x k" . kill-this-buffer))
  :config
  (tool-bar-mode -1))

;; ** 关闭 menu-bar
(use-package menu-bar
  :ensure nil
  :bind (("C-x k" . kill-this-buffer))
  :config
  (menu-bar-mode 0))

;; ** 关闭 scroll-bar
(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

;; ** 配对括号高亮显示
(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1))

;; ** 括号自动匹配
(use-package elec-pair
  :ensure nil
  :config
  (electric-pair-mode 1))

;; ** switch-window
(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x 1" . switch-window-then-maximize)
         ("C-x 2" . switch-window-then-split-below)
         ("C-x 3" . switch-window-then-split-right)
         ("C-x 0" . switch-window-then-delete))
  :config
  (unless (display-graphic-p)
    (setq switch-window-shortcut-appearance 'asciiart))
  (setq switch-window-increase 6)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-input-style 'minibuffer))

;; ** 设置 swiper , ivy-mode and counsel
(use-package counsel
  :bind
  (("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x b" . ivy-switch-buffer)
   ("C-x f" . counsel-recentf)
   ("C-x C-b" . ivy-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-c y" . counsel-yank-pop))

  :config
  ;; Default setting is not suitable for GuixSD.
  (setq counsel-linux-app-format-function
        #'counsel-linux-app-format-function-name-only))

(use-package swiper
  :demand t
  :bind (
         :map ivy-minibuffer-map
         ;; I use "C-x C-f" to open file, so bind "C-f" to
         ;; `ivy-immediate-done' is very useful.
         ("C-f" . ivy-immediate-done)
         ("S-<return>" . ivy-immediate-done)
         ([mouse-1] . ignore)
         ([mouse-3] . ignore)
         ([mouse-4] . ivy-next-line)
         ([mouse-5] . ivy-previous-line))
  :config
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
          (woman . ""))))

(use-package ivy-hydra
  :after counsel
  :demand t)

;; ** 设置 amx
(use-package amx
  :after counsel
  :config
  (amx-initialize))

;; ibuffer
(use-package ibuffer
  :ensure nil
  :config
  ;; Make ibuffer work well with termux
  (define-key ibuffer-name-map [(mouse-2)] 'ibuffer-mouse-toggle-mark)
  (define-key ibuffer-name-map [(mouse-1)] 'ibuffer-mouse-visit-buffer))

;; ** VC
(use-package vc
  :ensure nil
  :config
  ;; I use magit instead of vc :-)
  (setq vc-handled-backends nil)
  (setq vc-ignore-dir-regexp ".*"))

;; ** elisp setting
(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :ensure nil)

;; ** wdired and dired-ranger
(use-package dired
  :commands dired
  :ensure nil)

(use-package wdired
  :after dired
  :ensure nil)

(use-package dired-ranger
  :after dired
  :ensure nil)

;; ** 设置拼音输入法
(use-package pyim
  :ensure nil
  :bind* (("M-j" . pyim-convert-string-at-point)
          :map minibuffer-local-map
          ("C-<return>" . pyim-convert-cregexp-at-point))
  :config

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

  )

(use-package pyim-basedict
  :after pyim
  :config (pyim-basedict-enable))

(use-package pyim
  :after ivy
  :config

  (defun eh-ivy-cregexp (str)
    (let ((a (ivy--regex-plus str))
          (b (let ((case-fold-search nil))
               (pyim-cregexp-build str))))
      (if (and a (stringp a))
          (concat a "\\|" b)
        a)))

  (setq ivy-re-builders-alist
        '((t . eh-ivy-cregexp))))

;; ** eh-function
(use-package eh-function
  :ensure nil
  :commands (eh-wash-text
             eh-dos2unix
             eh-unix2dos
             eh-revert-buffer-with-gbk
             eh-revert-buffer-with-utf8
             eh-save-buffer-with-gbk
             eh-save-buffer-with-utf8
             eh-utf8-language-environment
             eh-gbk-language-environment))

;; * Footer
(provide 'eh-basic)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8-unix
;; End:

;;; eh-basic.el ends here
