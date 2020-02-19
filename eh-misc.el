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
(require 'share2computer)
(add-hook 'share2computer-finish-hook #'eh-system-open)

;; ** Emacs Application Framework (EAF)
(setq eaf-find-alternate-file-in-dired t)
(with-eval-after-load "eaf"
  (setq eaf-buffer-title-format "EAF/%s")
  (setq browse-url-browser-function 'eaf-open-browser)
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding))

;; ** terminal here
(require 'terminal-here)

;; ** yasnippet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "d:/org/snippets" t)
(add-to-list 'yas-snippet-dirs "e:/org/snippets" t)
(add-to-list 'yas-snippet-dirs "f:/org/snippets" t)
(add-to-list 'yas-snippet-dirs "~/org/snippets" t)
(yas-global-mode 1)

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
(require 'tramp)
(push "/data/data/com.termux/files/usr/bin" tramp-remote-path)

;; ** Eshell
(require 'eshell)
(require 'em-term)
(require 'em-unix)

(global-set-key (kbd "C-x c") 'eshell)

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
  (eshell arg))

;; ** EWW
(require 'eww)
(setq shr-width 90)
;; 搜狗:  http://www.sogou.com/sogou?query=
;; 百度:  http://m.baidu.com/ssid=0/s?word=
;; 必应:  http://cn.bing.com/search?q=
(setq eww-search-prefix "http://www.sogou.com/sogou?query=")

;; ** cnfonts
(setq cnfonts-verbose nil)
(require 'cnfonts)
(when (display-graphic-p)
  (setq cnfonts-use-face-font-rescale
        (eq system-type 'gnu/linux))
  (cnfonts-enable)
  (global-set-key (kbd "C--") 'cnfonts-decrease-fontsize)
  (global-set-key (kbd "C-=") 'cnfonts-increase-fontsize)
  (global-set-key (kbd "C-+") 'cnfonts-next-profile))

;; ** eh-website
(require 'org2web)
(require 'eh-website nil t)
(require 'org2web-devtools nil t)
(require 'pyim-devtools nil t)

;; ** el2org
(require 'el2org)

;; ** EPG
(require 'epg)
;; 1. Put the below to your ~/.gnupg/gpg-agent.conf:
;;       allow-emacs-pinentry
;;       allow-loopback-pinentry
;; 2. gpgconf --reload gpg-agent
;; 3. (setq epa-pinentry-mode 'loopback)
;; 4. (pinentry-start)
(setq epa-pinentry-mode 'loopback)

;; ** emms
(require 'eh-emms)

;; ** aggressive-indent
(require 'aggressive-indent)

(defun eh-elisp-setup ()
  ;; 跟踪行尾空格
  (setq show-trailing-whitespace t)
  ;; 高亮TAB
  (setq highlight-tabs t)
  ;; 自动缩进
  (aggressive-indent-mode))

(add-hook 'emacs-lisp-mode-hook
          #'eh-elisp-setup)

;; ** python
;; (require 'python)

;; (setq python-shell-interpreter "C:/ProgramData/Anaconda3/pythonw.exe"
;;       python-shell-interpreter-args "-i C:/ProgramData/Anaconda3/Scripts/ipython-script.py")

;; (add-hook 'inferior-python-mode-hook
;;           #'(lambda ()
;;               (setq comint-preoutput-filter-functions
;;                     '(eh-python-comint-preoutput-funtion))))

;; (defun eh-python-comint-preoutput-funtion (output)
;;   (concat "\n" output))

;; (defun eh-python-shell-send-region-or-line (&optional send-main msg)
;;   "Sends from python-mode buffer to a python shell, intelligently."
;;   (interactive current-prefix-arg t)
;;   (display-buffer
;;    (process-buffer (python-shell-get-process-or-error msg)) t t)
;;   (cond ((region-active-p)
;;          (python-shell-send-region (region-beginning) (region-end) send-main msg)
;;          (setq deactivate-mark t))
;;         (t (eh-python-shell-send-current-statement))))

;; (defun eh-python-shell-send-current-statement ()
;;   "Send current statement to Python shell.
;; Taken from elpy-shell-send-current-statement"
;;   (interactive)
;;   (let ((beg (python-nav-beginning-of-statement))
;;         (end (python-nav-end-of-statement)))
;;     (python-shell-send-string (buffer-substring beg end)))
;;   (python-nav-forward-statement))

;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;               (define-key python-mode-map "\C-c\C-c" 'eh-python-shell-send-region-or-line)))

;; ;; ** ESS
;; (require 'ess)
;; (require 'ess-mode)

;; ;; 这个变量设置为 nil 后 org-babel 会有奇怪的问题
;; (setq ess-eval-visibly-p t)
;; (setq ess-history-file nil)
;; (setq ess-ask-for-ess-directory nil)
;; (setq ess-smart-S-assign-key nil)
;; ;; (setq inferior-ess-r-program "R")

;; (defun eh-ess-popup-ESS-buffer (eob-p)
;;   (interactive "P")
;;   (ess-force-buffer-current)
;;   (let ((buffer (current-buffer)))
;;     (ess-switch-to-ESS eob-p)
;;     (ess-show-buffer buffer t)))

;; (defun eh-ess-eval-region-or-line-and-step (vis)
;;   (interactive "P")
;;   (ess-eval-region-or-line-and-step vis)
;;   (eh-ess-popup-ESS-buffer t))

;; (defun eh-ess-eval-region-or-function-or-paragraph (vis)
;;   (interactive "P")
;;   (ess-eval-region-or-function-or-paragraph vis)
;;   (eh-ess-popup-ESS-buffer t))

;; (defun eh-ess-eval-region-or-function-or-paragraph-and-step (vis)
;;   (interactive "P")
;;   (ess-eval-region-or-function-or-paragraph-and-step vis)
;;   (eh-ess-popup-ESS-buffer t))

;; (define-key ess-mode-map (kbd "C-<return>") #'eh-ess-eval-region-or-line-and-step)
;; (define-key ess-mode-map (kbd "C-M-<return>") #'eh-ess-eval-region-or-function-or-paragraph)
;; (define-key ess-mode-map (kbd "C-c C-c") #'eh-ess-eval-region-or-function-or-paragraph-and-step)

;; ** aggressive-indent
(require 'aggressive-indent)

;; ;; ** multi-term
;; (require 'multi-term)

;; (setq multi-term-program "/bin/bash")
;; (setq multi-term-buffer-name "term")
;; (setq term-scroll-show-maximum-output nil)
;; (setq term-scroll-to-bottom-on-output nil)
;; (setq multi-term-dedicated-select-after-open-p t)
;; (setq term-bind-key-alist
;;       (append '(("C-c C-x" . eh-term-send-ctrl-x)
;;                 ("C-c C-h" . eh-term-send-ctrl-h))
;;               term-bind-key-alist))

;; (add-hook 'term-mode-hook #'eh-term-setup)
;; (add-hook 'term-mode-hook #'multi-term-keystroke-setup)
;; (add-hook 'kill-buffer-hook #'multi-term-kill-buffer-hook)

;; (defun eh-term-setup ()
;;   (setq truncate-lines t)
;;   (setq term-buffer-maximum-size 0)
;;   (setq show-trailing-whitespace nil)
;;   (multi-term-handle-close))

;; (defun eh-term-send-ctrl-x ()
;;   "Send C-x in term mode."
;;   (interactive)
;;   (term-send-raw-string "\C-x"))

;; (defun eh-term-send-ctrl-z ()
;;   "Send C-z in term mode."
;;   (interactive)
;;   (term-send-raw-string "\C-z"))

;; (defun eh-term-send-ctrl-h ()
;;   "Send C-h in term mode."
;;   (interactive)
;;   (term-send-raw-string "\C-h"))

;; ** ace-jump
(require 'ace-jump-mode)
(global-set-key (kbd "C-j") 'ace-jump-mode)

;; ** gitpatch
(require 'gitpatch)
(global-set-key (kbd "C-c m") 'eh-gitpatch-mail)

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
    (call-interactively 'gitpatch-mail)))

;; ** ebdb
(require 'ebdb)
(require 'ebdb-mua)
(require 'ebdb-gnus)
(require 'ebdb-com)
(require 'ebdb-vcard)
(setq ebdb-gnus-window-configuration
      '(article
        (vertical 1.0
		  (summary 0.25 point)
		  (horizontal 1.0
			      (article 1.0)
			      (ebdb-gnus 0.25)))))
(require 'ebdb-complete)
(ebdb-complete-enable)

(require 'ebdb-i18n-chn)
(cl-defmethod ebdb-field-search
  :around (field criterion)
  (or (cl-call-next-method)
      (when (stringp criterion)
        (let ((str (ebdb-string field)))
          (cl-some
           (lambda (pinyin)
             (string-match-p criterion pinyin))
           (append (pyim-hanzi2pinyin str nil "" t)
                   (pyim-hanzi2pinyin str t "" t)))))))

;; ** magit
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)
(define-key magit-status-mode-map (kbd "C-c f") 'magit-format-patch)
(setq magit-completing-read-function 'ivy-completing-read)
(setq counsel-yank-pop-separator
      (concat "\n\n" (make-string 70 ?-) "\n"))
(setq counsel-git-log-cmd
      "GIT_PAGER=cat git log --pretty='TUMASHU%%s%%n%%n%%b' --grep '%s'")
(setq counsel-git-log-split-string-re "TUMASHU")

;; ** projectile
(require 'projectile)
(global-set-key (kbd "C-x F") 'projectile-find-file)
(global-set-key (kbd "C-S-s") 'projectile-grep)
(setq projectile-completion-system 'ivy)
(setq projectile-enable-caching nil)
(projectile-global-mode 1)

;; ** guix
(with-eval-after-load "guix"
  (setq guix-directory "~/project/guix")
  (setq geiser-debug-jump-to-debug-p nil)
  (setq geiser-guile-binary
        (list (executable-find "guile")
              ;; Avoid auto-compilation as it is slow and error-prone:
              ;; <https://notabug.org/alezost/emacs-guix/issues/2>.
              "--no-auto-compile"))
  (add-hook 'scheme-mode-hook 'guix-devel-mode))

(with-eval-after-load "geiser-guile"
  (add-to-list 'geiser-guile-load-path "~/.config/guix/latest"))

;; ** undo-tree
(require 'undo-tree)
(global-key-binding (kbd "C-c /") 'undo-tree-visualize)
(global-undo-tree-mode)
(add-hook 'undo-tree-visualizer-mode-hook
          #'eh-undo-tree-visualizer-settings)

(defun eh-undo-tree-visualizer-settings ()
  (interactive)
  (define-key undo-tree-visualizer-mode-map (kbd "C-c C-k") #'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map (kbd "C-k") #'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map (kbd "k") #'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map (kbd "C-g") #'undo-tree-visualizer-abort))

;; * Footer
(provide 'eh-misc)

;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:

;;; eh-misc.el ends here
