;;; eh-misc.el --- Tumashu's emacs configuation

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

;; ** Share to Computer

(defvar eh-sharetocomputer-url nil)
(defvar eh-sharetocomputer-default-path "~/ShareToComputer/")
(defvar eh-sharetocomputer-file-number 0)
(defvar eh-sharetocomputer-buffers nil)

(defun eh-sharetocomputer-write (status path n)
  (let* ((err (plist-get status :error))
         (disposition
          (mail-fetch-field "Content-Disposition"))
         (filename
          (when disposition
            (replace-regexp-in-string
             ".*filename=\"\\(.*\\)\"$" "\\1"
             (decode-coding-string disposition 'utf-8)))))
    (when (and filename (not err))
      (let ((file (concat (file-name-as-directory path) filename)))
        (delete-region
         (point-min)
         (progn
           (re-search-forward "\n\n" nil 'move)
           (point)))
        (if (file-exists-p file)
            (message "ShareToComputer: file %S is exist, do not override it." file)
          (let ((coding-system-for-write 'no-conversion))
            (write-region nil nil file)))
        (setq eh-sharetocomputer-file-number
              (+ eh-sharetocomputer-file-number 1))
        (if (= eh-sharetocomputer-file-number n)
            (progn
              (message "ShareToComputer: download finished!")
              (eh-system-open path))
          (message "ShareToComputer: downloading %s/%s files to %S ..." eh-sharetocomputer-file-number n path))))))

(defun eh-sharetocomputer-1 (path)
  (setq path (expand-file-name (file-name-as-directory path)))
  (make-directory path t)
  (while (< (length eh-sharetocomputer-url) 1)
    (setq eh-sharetocomputer-url
          (read-from-minibuffer "ShareToComputer url: " "http://192.168.0.X:8080"))
    (when (y-or-n-p "Save this url for future session? ")
      (customize-save-variable
       'eh-sharetocomputer-url
       eh-sharetocomputer-url)))
  (let* ((buf (url-retrieve-synchronously
               (concat (file-name-as-directory eh-sharetocomputer-url) "info")
               t nil 3))
         (n (with-current-buffer buf
              (goto-char (point-min))
              (re-search-forward "\n\n" nil 'move)
              (ignore-errors
                (cdr (assoc 'total
                            (json-read-from-string
                             (buffer-substring (point) (point-max)))))))))
    (when (and (numberp n)
               (> n 0))
      (dotimes (i n)
        (push (url-retrieve
               (format "%s%S" (file-name-as-directory eh-sharetocomputer-url) i)
               (lambda (status path n)
                 (eh-sharetocomputer-write status path n))
               (list path n)
               nil t)
              eh-sharetocomputer-buffers)))))

(defun eh-sharetocomputer ()
  (interactive)
  (let ((buff))
    (while (setq buff (pop eh-sharetocomputer-buffers))
      (if (buffer-live-p buff)
	  (kill-buffer buff))))
  (setq eh-sharetocomputer-file-number 0)
  (if (or (eq major-mode 'org-agenda-mode)
          (eq major-mode 'org-mode))
      (let (c marker)
        (when (eq major-mode 'org-agenda-mode)
          (setq marker (or (get-text-property (point) 'org-hd-marker)
		           (get-text-property (point) 'org-marker)))
          (unless marker
	    (error "No task in current line")))
        (save-excursion
          (when marker
	    (set-buffer (marker-buffer marker))
	    (goto-char marker))
          (org-back-to-heading t)
          (eh-sharetocomputer-1 (org-attach-dir t))))
    (eh-sharetocomputer-1 eh-sharetocomputer-default-path)))

;; ** EAF
(use-package eaf
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (setq eaf-buffer-title-format "EAF/%s")
  (setq browse-url-browser-function 'eaf-open-browser)
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding))

;; ** terminal here
(use-package terminal-here)

;; ** yasnippet
(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs "d:/org/snippets" t)
  (yas-global-mode 1))

;; ** Tramp (msys2's emacs) 和 termux 的 sshd 配合使用需要如下设置：
;;
;; *** 基本设置
;; 1. 将 "/data/data/com.termux/files/usr/bin" 添加到 tramp-remote-path
;; 2. 清理 termux 文件：/data/data/com.termux/files/usr/etc/motd ,
;;    去掉文件中的所有的 "<" 和 ">",因为这两个字符会影响 tramp 登录，
;;    具体细节见 Tramp FAQ： https://www.gnu.org/software/tramp/
;;
;; *** 使用 ssh 相关方法的设置
;; 使用 ssh 的相关 tramp 方法有： ssh, sshx, scp, scpx
;;
;; 1. ssh 和 scp 两种 tramp 方法会让 emacs 卡死，原因可能和 cygwin 遇到
;;    的情况类似：
;;
;;    #+BEGIN_EXAMPLE
;;    Pseudo-terminal will not be allocated because stdin is not a terminal.
;;    #+END_EXAMPLE
;;
;;    具体请参考 Tramp 文档的相关章节： "Issues with Cygwin ssh"
;;    https://www.gnu.org/software/tramp/
;;
;; 2. sshx 和 scpx 可以正常使用，大文件访问使用 scpx 方法速度比较快。
;; 3. 建议使用 key + ssh-agent 的方式登录，具体设置方法请自行搜索。
;;
;;    下面是一个启动 ssh-agent 的脚本，调整以下贴到 ~/.bashrc 文件就可以了。
;;
;;    #+BEGIN_SRC shell
;;    if [ -f ~/.agent.env ]; then
;;        . ~/.agent.env >/dev/null
;;        if ! kill -0 $SSH_AGENT_PID >/dev/null 2>&1; then
;;            echo "Stale agent file found. Spawning new agent..."
;;            eval `ssh-agent |tee ~/.agent.env`
;;            ssh-add
;;        fi
;;    else
;;        echo "Starting ssh-agent..."
;;        eval `ssh-agent |tee ~/.agent.env`
;;        ssh-add
;;    fi
;;    #+END_SRC
;;
;; *** 使用 Putty 相关方法的设置
;; 使用 putty 的 tramp 方法有四种： plink, plinks, pscp 和 psftp
;;
;; putty 在 window 平台下有图形界面，使用起来很方便，推荐使用，
;; 但 putty, msys2 和 termux 三者配合需要做一些配置。
;;
;; 1. 安装 putty, plink 和 ssh-pageant 三个外部程序：
;;
;;    #+BEGIN_EXAMPLE
;;    pacman -Ss mingw-w64-i686-putty mingw-w64-i686-putty-ssh ssh-pageant-git
;;    #+END_EXAMPLE
;;
;; 2. 在 msys2 的 .bashrc 中设置环境变量: MSYS2_ARG_CONV_EXCL, 比如：
;;
;;    #+BEGIN_EXAMPLE
;;    export MSYS2_ARG_CONV_EXCL="192.168.137.250:;feng@192.168.137.250:"
;;    #+END_EXAMPLE
;;
;;    这个设置的意思是：在调用命令时，所有以 "192.168.137.250:" 或者
;;    "feng@192.168.137.250:" 开头的命令参数，都保持原样，不要作 msys2
;;    文件路径转换，比如：
;;
;;    #+BEGIN_EXAMPLE
;;    pscp feng@192.168.137.250:/test.org ~/test.org
;;    #+END_EXAMPLE
;;
;;    这个命令中 "feng@192.168.137.250:/test.org" 这个参数实例将被保护，
;;    不作转换。
;;
;;    这个步骤对于 pscp 和 psftp 两个 tramp 方法非常重要， 不然 pscp 会报错：
;;    "ssh_init Host does not exit"
;;
;;    想了解具体细节同学可以阅读：
;;    1. https://github.com/msys2/msys2/wiki/Porting#user-content-filesystem-namespaces
;;    2. https://stackoverflow.com/questions/41789559/how-to-prevent-msys-from-converting-remote-file-path-for-pscp
;;
;; 3. 设置 putty, 最好设置为免密码登录，比如：key + ssh-pageant 的方式。
;;    具体方法请自行搜索。
;; 4. 将 "C:\msys32\mingw32\bin\pageant.exe" 的快捷方式添加到启动菜单。
;;    更改快捷方式的目标：
;;
;;    #+BEGIN_EXAMPLE
;;    C:\msys32\mingw32\bin\pageant.exe YOUR-PUTTY-KEY-PATH
;;    #+END_EXAMPLE
;;
;; 5. 如果用户使用 plinkx 方法, 还需要设置保存一个 putty session, 这个
;;    putty session 的名字与 host 的名字一致， 比如用 tramp 访问：
;;
;;    #+BEGIN_EXAMPLE
;;    /plinkx:192.168.1.101:/
;;    #+END_EXAMPLE
;;
;;    就需要设置保存一个名字为 "192.168.1.101" 的 putty session .
(use-package tramp
  :ensure nil
  :config
  (push "/data/data/com.termux/files/usr/bin" tramp-remote-path))

;; ** Eshell
(use-package eshell
  :bind (("C-x c" . eshell))
  :ensure nil
  :config
  (use-package em-term :ensure nil)
  (use-package em-unix :ensure nil)
  (setq eshell-visual-commands
        (append '("top" "htop" "aptitude" "mutt"
                  "nano" "crontab" "vim" "less" "zile")
                eshell-visual-commands))
  (setq eshell-visual-subcommands
        (list (append '("sudo") eshell-visual-commands)
              '("git" "log" "diff" "show" "grep"
                "commit" "rebase" "pull" "push")))
  (setq eshell-visual-options
        '(("git" "--help" "--paginate")))
  (defun eh-eshell (&optional arg)
    (interactive)
    ;; 使用eshell-exec-visual第一次打开term时，
    ;; 不能使用multi-term的键盘绑定，原因不知，
    ;; 首先运行一下less, 从而让multi-term的键盘绑定生效。
    (eshell-command "less")
    (eshell arg)))

;; ** EWW
(use-package eww
  :commands eww
  :ensure nil
  :config
  (setq shr-width 90)
  ;; 搜狗:  http://www.sogou.com/sogou?query=
  ;; 百度:  http://m.baidu.com/ssid=0/s?word=
  ;; 必应:  http://cn.bing.com/search?q=
  (setq eww-search-prefix "http://www.sogou.com/sogou?query="))


;; ** cnfonts
(use-package cnfonts
  :demand t
  :if (display-graphic-p)
  :init (setq cnfonts-verbose nil)
  :config
  (setq cnfonts-use-face-font-rescale
        (eq system-type 'gnu/linux))
  (cnfonts-enable)
  :bind (("C--" . cnfonts-decrease-fontsize)
         ("C-=" . cnfonts-increase-fontsize)
         ("C-+" . cnfonts-next-profile)))

;; ** eh-website
(use-package org2web
  :commands (org2web-publish org2web-new-post))

(use-package eh-website
  :after org2web
  :ensure nil)

(use-package org2web-devtools
  :after org2web
  :ensure nil)

(use-package pyim-devtools
  :after (:all org2web pyim)
  :ensure nil)

;; ** el2org
(use-package el2org
  :commands (el2org-generate-readme
             el2org-generate-org
             el2org-generate-html))

;; ** poporg
(use-package poporg
  :bind (("C-c \"" . poporg-dwim)))

;; EPG
(use-package epg
  :after gnus  ;; Only use it in gnus
  :config
  ;; 1. Put the below to your ~/.gnupg/gpg-agent.conf:
  ;;       allow-emacs-pinentry
  ;;       allow-loopback-pinentry
  ;; 2. gpgconf --reload gpg-agent
  ;; 3. (setq epa-pinentry-mode 'loopback)
  ;; 4. (pinentry-start)
  (setq epa-pinentry-mode 'loopback))

;; ** emms
(use-package eh-emms
  :commands (emms eh-emms emms-browser)
  :ensure nil)

;; ** aggressive-indent
(use-package aggressive-indent
  :config

  (defun eh-elisp-setup ()
    ;; 跟踪行尾空格
    (setq show-trailing-whitespace t)
    ;; 高亮TAB
    (setq highlight-tabs t)
    ;; 自动缩进
    (aggressive-indent-mode))

  (add-hook 'emacs-lisp-mode-hook
            #'eh-elisp-setup))

;; ** python
(use-package python
  :config
  (setq python-shell-interpreter "C:/ProgramData/Anaconda3/pythonw.exe"
        python-shell-interpreter-args
        "-i C:/ProgramData/Anaconda3/Scripts/ipython-script.py"
        )

  (add-hook 'inferior-python-mode-hook
            #'(lambda ()
                (setq comint-preoutput-filter-functions
                      '(eh-python-comint-preoutput-funtion))))

  (defun eh-python-comint-preoutput-funtion (output)
    (concat "\n" output))

  (defun eh-python-shell-send-region-or-line (&optional send-main msg)
    "Sends from python-mode buffer to a python shell, intelligently."
    (interactive current-prefix-arg t)
    (display-buffer
     (process-buffer (python-shell-get-process-or-error msg)) t t)
    (cond ((region-active-p)
           (python-shell-send-region (region-beginning) (region-end) send-main msg)
           (setq deactivate-mark t))
          (t (eh-python-shell-send-current-statement))))

  (defun eh-python-shell-send-current-statement ()
    "Send current statement to Python shell.
Taken from elpy-shell-send-current-statement"
    (interactive)
    (let ((beg (python-nav-beginning-of-statement))
          (end (python-nav-end-of-statement)))
      (python-shell-send-string (buffer-substring beg end)))
    (python-nav-forward-statement))

  (add-hook 'python-mode-hook
            #'(lambda ()
                (define-key python-mode-map "\C-c\C-c" 'eh-python-shell-send-region-or-line))))

;; ** ESS
(use-package ess
  :bind (:map
         ess-mode-map
         ("C-<return>" . eh-ess-eval-region-or-line-and-step)
         ("C-M-<return>" . eh-ess-eval-region-or-function-or-paragraph)
         ("C-c C-c" . eh-ess-eval-region-or-function-or-paragraph-and-step))
  :config
  ;; 这个变量设置为 nil 后 org-babel 会有奇怪的问题
  (setq ess-eval-visibly-p t)
  (setq ess-history-file nil)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-smart-S-assign-key nil)
  ;; (setq inferior-ess-r-program "R")

  (defun eh-ess-popup-ESS-buffer (eob-p)
    (interactive "P")
    (ess-force-buffer-current)
    (let ((buffer (current-buffer)))
      (ess-switch-to-ESS eob-p)
      (ess-show-buffer buffer t)))

  (defun eh-ess-eval-region-or-line-and-step (vis)
    (interactive "P")
    (ess-eval-region-or-line-and-step vis)
    (eh-ess-popup-ESS-buffer t))

  (defun eh-ess-eval-region-or-function-or-paragraph (vis)
    (interactive "P")
    (ess-eval-region-or-function-or-paragraph vis)
    (eh-ess-popup-ESS-buffer t))

  (defun eh-ess-eval-region-or-function-or-paragraph-and-step (vis)
    (interactive "P")
    (ess-eval-region-or-function-or-paragraph-and-step vis)
    (eh-ess-popup-ESS-buffer t))

  ;; (define-key ess-mode-map (kbd "C-<return>") #'eh-ess-eval-region-or-line-and-step)
  ;; (define-key ess-mode-map (kbd "C-M-<return>") #'eh-ess-eval-region-or-function-or-paragraph)
  ;; (define-key ess-mode-map (kbd "C-c C-c") #'eh-ess-eval-region-or-function-or-paragraph-and-step)
  )

;; ** aggressive-indent
(use-package aggressive-indent)

;; ** multi-term
(use-package multi-term
  :commands multi-term
  :ensure nil
  :config
  (setq multi-term-program "/bin/bash")
  (setq multi-term-buffer-name "term")
  (setq term-scroll-show-maximum-output nil)
  (setq term-scroll-to-bottom-on-output nil)
  (setq multi-term-dedicated-select-after-open-p t)
  (setq term-bind-key-alist
        (append '(("C-c C-x" . eh-term-send-ctrl-x)
                  ("C-c C-h" . eh-term-send-ctrl-h))
                term-bind-key-alist))

  (remove-hook 'term-mode-hook 'eh-term-setup)
  (remove-hook 'term-mode-hook 'multi-term-keystroke-setup)
  (remove-hook 'kill-buffer-hook 'multi-term-kill-buffer-hook)

  (add-hook 'term-mode-hook #'eh-term-setup)
  (add-hook 'term-mode-hook #'multi-term-keystroke-setup)
  (add-hook 'kill-buffer-hook #'multi-term-kill-buffer-hook)

  (defun eh-term-setup ()
    (setq truncate-lines t)
    (setq term-buffer-maximum-size 0)
    (setq show-trailing-whitespace nil)
    (multi-term-handle-close))

  (defun eh-term-send-ctrl-x ()
    "Send C-x in term mode."
    (interactive)
    (term-send-raw-string "\C-x"))

  (defun eh-term-send-ctrl-z ()
    "Send C-z in term mode."
    (interactive)
    (term-send-raw-string "\C-z"))

  (defun eh-term-send-ctrl-h ()
    "Send C-h in term mode."
    (interactive)
    (term-send-raw-string "\C-h")))

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

;; ** ace-jump
(use-package ace-jump-mode
  :ensure nil
  :bind (("C-j" . ace-jump-mode)))

;; ** gitpatch
(use-package gitpatch
  :bind (("C-c m" . eh-gitpatch-mail))
  :ensure nil
  :config
  (setq gitpatch-mail-function 'gnus-msg-mail)
  (setq gitpatch-mail-attach-patch-key "C-c i")
  (setq gitpatch-mail-database
        '("guix-patches@gnu.org"
          "emms-help@gnu.org"
          "emacs-orgmode@gnu.org"
          "emacs-devel@gnu.org"))
  (defun eh-gitpatch-mail ()
    (interactive)
    ;; 如果 gnus 没有开启，强制开启。
    (let ((buffer (current-buffer)))
      (unless (gnus-alive-p)
        (gnus)
        (switch-to-buffer buffer))
      (call-interactively 'gitpatch-mail))))

;; ** ebdb
(use-package ebdb
  :ensure nil)

(use-package ebdb-mua
  :after ebdb
  :ensure ebdb)

(use-package ebdb-gnus
  :after ebdb
  :ensure ebdb
  :config
  (setq ebdb-gnus-window-configuration
        '(article
          (vertical 1.0
		    (summary 0.25 point)
		    (horizontal 1.0
			        (article 1.0)
			        (ebdb-gnus 0.25))))))

(use-package ebdb-com
  :after ebdb
  :ensure ebdb)

(use-package ebdb-vcard
  :after ebdb
  :ensure ebdb)

(use-package ebdb-complete
  :after ebdb
  :ensure ebdb
  :config
  (ebdb-complete-enable))

(use-package ebdb-i18n-chn
  :after ebdb
  :ensure nil)

(use-package pyim
  :after ebdb-i18n-chn
  :config
  ;; (defun eh-ebdb-search-chinese (string)
  ;;   (if (functionp 'pyim-isearch-build-search-regexp)
  ;;       (pyim-isearch-build-search-regexp string)
  ;;     string))

  ;; (setq ebdb-search-transform-functions
  ;;       '(eh-ebdb-search-chinese))
  (cl-defmethod ebdb-field-search
    :around (field criterion)
    (or (cl-call-next-method)
        (when (stringp criterion)
          (let ((str (ebdb-string field)))
            (cl-some
             (lambda (pinyin)
               (string-match-p criterion pinyin))
             (append (pyim-hanzi2pinyin str nil "" t)
                     (pyim-hanzi2pinyin str t "" t))))))))

;; ** magit
(use-package magit
  :bind (("C-c g" . magit-status)
         :map magit-status-mode-map
         ("C-c f" . magit-format-patch)))

(use-package swiper
  :after magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package counsel
  :after magit
  :config
  (setq counsel-yank-pop-separator
        (concat "\n\n" (make-string 70 ?-) "\n"))

  (setq counsel-git-log-cmd
        "GIT_PAGER=cat git log --pretty='TUMASHU%%s%%n%%n%%b' --grep '%s'")
  (setq counsel-git-log-split-string-re "TUMASHU"))

;; ** projectile
(use-package projectile
  :bind (("C-x F" . projectile-find-file)
         ("C-S-s" . projectile-grep)))

(use-package swiper
  :after projectile
  :ensure nil
  :config (setq projectile-completion-system 'ivy))

(use-package wgrep
  :after projectile
  :config
  (projectile-global-mode 1)
  (setq projectile-enable-caching nil))

;; ** guix
(use-package guix
  :commands (guix-scheme-mode guix-devel-mode)
  :ensure nil
  :config
  (setq guix-directory "~/project/guix")
  (setq geiser-debug-jump-to-debug-p nil)
  (setq geiser-guile-binary
        (list (executable-find "guile")
              ;; Avoid auto-compilation as it is slow and error-prone:
              ;; <https://notabug.org/alezost/emacs-guix/issues/2>.
              "--no-auto-compile"))
  (add-hook 'scheme-mode-hook 'guix-devel-mode))

(use-package geiser-guile
  :commands run-guile
  :ensure geiser
  :config
  (add-to-list 'geiser-guile-load-path "~/.config/guix/latest"))

;; ** undo-tree
(use-package undo-tree
  :bind (("C-c /" . undo-tree-visualize))
  :config
  (global-undo-tree-mode)
  (add-hook 'undo-tree-visualizer-mode-hook
            #'eh-undo-tree-visualizer-settings)
  (defun eh-undo-tree-visualizer-settings ()
    (interactive)
    (define-key undo-tree-visualizer-mode-map (kbd "C-c C-k") #'undo-tree-visualizer-quit)
    (define-key undo-tree-visualizer-mode-map (kbd "C-k") #'undo-tree-visualizer-quit)
    (define-key undo-tree-visualizer-mode-map (kbd "k") #'undo-tree-visualizer-quit)
    (define-key undo-tree-visualizer-mode-map (kbd "C-g") #'undo-tree-visualizer-abort)))

;; * Footer
(provide 'eh-misc)

;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:

;;; eh-misc.el ends here
