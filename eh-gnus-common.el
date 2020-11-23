;;; eh-gnus-common.el --- Tumashu's gnus configuation file    -*- lexical-binding: t; -*-

;; * Header
;; Copyright (c) 2008-2009, Andy Stewart
;;               2011-2019, Feng Shu

;; Author: Andy Stewartf <lazycat.manatee@gmail.com>
;;         Feng Shu <tumashu@163.com>
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
(require 'gnus)
(require 'gnus-cache)
(require 'gnus-cite)
(require 'gnus-agent)
(require 'nntp)
(require 'mm-encode)
(require 'mm-decode)
(require 'rfc2047)
(require 'gnus-demon)
(require 'eww)
(require 'gnus-select-account)
(gnus-select-account-enable)

;; 邮件分类设置
(setq nnmail-treat-duplicates 'delete
      nnmail-split-fancy-match-partial-words t
      nnmail-mail-splitting-decodes t
      nnmail-mail-splitting-charset 'utf-8)

;; 存储设置
(setq gnus-startup-file "~/Gnus/.newsrc")                  ;初始文件
(setq gnus-init-file "~/Gnus/.gnus")                       ;.gnus 位置
(setq gnus-default-directory nil)                          ;默认目录
(setq gnus-home-directory "~/")                            ;主目录
(setq gnus-dribble-directory "~/Gnus/")                    ;恢复目录
(setq gnus-directory "~/Gnus/News/")                       ;新闻组的存储目录
(setq gnus-article-save-directory "~/Gnus/News/")          ;文章保存目录
(setq gnus-kill-files-directory "~/Gnus/News/trash/")      ;文件删除目录
(setq gnus-agent-directory "~/Gnus/News/agent/")           ;代理目录
(setq gnus-cache-directory "~/Gnus/News/cache/")           ;缓存目录
(setq gnus-cache-active-file "~/Gnus/News/cache/active")   ;缓存激活文件
(setq message-directory "~/Gnus/Mail/")                    ;邮件的存储目录
(setq message-auto-save-directory "~/Gnus/Mail/drafts")    ;自动保存的目录
(setq mail-source-directory "~/Gnus/Mail/incoming")        ;邮件的源目录
(setq nnmail-message-id-cache-file "~/Gnus/.nnmail-cache") ;nnmail 的消息ID缓存
(setq nnml-newsgroups-file "~/Gnus/Mail/newsgroup")        ;邮件新闻组解释文件
(setq nntp-marks-directory "~/Gnus/News/marks")            ;nntp 组存储目录
(setq mml-default-directory "~/")                          ;附件的存储位置

;; 默认禁用 nnfolder
(setq gnus-message-archive-group nil)

;; 设置 message-mode 发信的方式，这里默认使用/usr/sbin/sendmail.
;; 在 `gnus-posting-styles' 中设置 "X-Message-SMTP-Method" 邮件头可以实现
;; 更为复杂的邮件发送方式。
(setq message-send-mail-function 'message-smtpmail-send-it)

;; 设置gnus默认编码: 如果常与国外联系，可以设置为utf-8
;; 如果只在本国使用，可以设置为本地编码，比如: gbk
(setq gnus-default-charset 'gbk)

;; 根据method来确定编码
(setq gnus-group-name-charset-method-alist
      '(((nntp "news.newsfan.net") . gbk)
        ((nntp "news.cn99.com") . gbk)))

;; 根据组名称来确定组名称解析使用的编码
(setq gnus-group-name-charset-group-alist
      '((".*" . gbk)))

;; 确定组默认使用的编码。
(setq gnus-group-charset-alist
      '((".*" . gbk)))

;; 如果还有乱码，手动调整
(setq gnus-summary-show-article-charset-alist
      '((1 . gbk)
        (2 . utf-8)
        (3 . big5)
        (4 . utf-7)))

;; 邮件MIME类型设置不正确时，gnus的处理方式。
(setq gnus-newsgroup-ignored-charsets
      '(unknown-8bit x-unknown x-gbk))

;; 设置邮件附件文件名的编码方式以及邮件subject的编码方式
(defalias 'mail-header-encode-parameter 'rfc2047-encode-parameter)
(add-to-list 'rfc2047-charset-encoding-alist '(gbk . B))
(add-to-list 'rfc2047-charset-encoding-alist '(gb18030 . B))

;; 常规设置
(setq gnus-agent t)                                 ; 开启agent
(setq read-mail-command 'gnus)                      ; 使用gnus阅读邮件
(setq mail-user-agent 'gnus-user-agent)             ; 使用gnus发送邮件
(setq gnus-inhibit-startup-message t)               ; 关闭启动时的画面
(setq gnus-novice-user nil)                         ; 关闭新手设置, 不进行确认
(setq gnus-expert-user t)                           ; 不询问用户
(setq gnus-show-threads t)                          ; 显示邮件threads
(setq gnus-interactive-exit t)                      ; 退出时进行交互式询问
(setq gnus-use-dribble-file t)                      ; 创建恢复文件
(setq gnus-always-read-dribble-file t)              ; 读取恢复文件
(setq gnus-asynchronous t)                          ; 异步操作
(setq gnus-large-newsgroup 2000)                    ; 设置大容量的新闻组默认显示的大小
(setq gnus-large-ephemeral-newsgroup nil)           ; 设置临时新闻组默认显示的大小
(setq gnus-read-active-file 'some)
(setq gnus-nov-is-evil nil)
(setq gnus-summary-ignore-duplicates t)             ; 忽略具有相同ID的消息
(setq gnus-treat-fill-long-lines t)                 ; 自动折行
(setq message-confirm-send t)                       ; 发邮件前需要确认（防止误发）
(setq message-kill-buffer-on-exit t)                ; 发送邮件后删除buffer
(setq message-syntax-checks '((sender . disabled))) ; 语法检查
(setq nnmail-expiry-wait 7)                         ; 邮件自动删除的期限 (单位: 天)
(setq nnmairix-allowfast-default t)                 ; 加快进入搜索结果的组
(setq gnus-use-correct-string-widths t)             ; 使用正确的字符宽度
(setq gc-cons-threshold 3500000)                    ; 加快 gnus 的速度
(setq gnus-use-cross-reference t)                   ; 开启交叉索引
(setq gnus-summary-display-while-building 50)       ; 生成 summary 时,每50封显示一下

;; 进入 summer 模式时，禁止自动选择第一个article,
;; 这样设置主要是因为有些 article 下载速度极慢，
;; 会降低响应速度
(setq gnus-auto-select-first nil)
(setq gnus-auto-select-next nil)

;; 设置 gnus 启动时,组级别大于3的不自动更新。
;; 当你添加了许多速度慢的组时，比如 rss,imap 等，启动速度会相当慢。这时你
;; 可以把它们的组级别设置为大于3的值，这样启动时就不自动更新了。
;; 当你需要更新这些组的时候，使用 "4-g" "5-g" 等快捷键
(setq gnus-activate-level 3)

;; 双窗口布局(水平)
(gnus-add-configuration
 '(article
   (vertical 1.0
             (summary 0.25 point)
             (article 1.0))))

;; 设置图片显示方式
(setq mm-inline-large-images t)
(add-to-list 'mm-attachment-override-types "image/*")

;; 设置summary缓冲区的显示格式
(setq gnus-extra-headers '(To From))
(setq nnmail-extra-headers gnus-extra-headers)
(setq gnus-summary-gather-subject-limit 'fuzzy)
(setq gnus-summary-make-false-root 'adopt)
(setq gnus-summary-line-format "%U%R%z %&user-date; |%I%B%s\n")

;; 设置 threads 的样式
(setq gnus-thread-indent-level 0)
(setq gnus-summary-same-subject "")
(setq gnus-sum-thread-tree-indent "  ")
(setq gnus-sum-thread-tree-single-indent "> ")
(setq gnus-sum-thread-tree-root "> ")
(setq gnus-sum-thread-tree-false-root "> ")
(setq gnus-sum-thread-tree-vertical "   |")
(setq gnus-sum-thread-tree-leaf-with-other "   |-> ")
(setq gnus-sum-thread-tree-single-leaf "    `-> ")

;; 设置 `gnus-summary-line-format' 中的 %&user-date;
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%H:%M")
        ((+ (* 24 3600)    (gnus-seconds-today)) . "YD   ")
        ((- (gnus-seconds-month) (* 72 3600)) . "%dth ")
        ((- (gnus-seconds-month) (* 48 3600)) . "%drd ")
        ((- (gnus-seconds-month) (* 24 3600)) . "%dnd ")
        ((gnus-seconds-month) . "%dst ")
        ((gnus-seconds-year)  . "%m-%d")
        (t . "%Y ")))

;; 将邮件的发出时间转换为本地时间
(add-hook 'gnus-article-prepare-hook #'gnus-article-date-local)

;; 跟踪组的时间轴
(add-hook 'gnus-select-group-hook #'gnus-group-set-timestamp)

(defun eh-gnus-summary-setup ()
  (interactive)
  ;; summary buffer 行距设置
  (setq line-spacing 5)

  ;; Highlight 当前行
  (hl-line-mode 1)

  ;; 重新定义键盘绑定
  (local-set-key (kbd "SPC")
                 (lambda ()
                   (interactive)
                   (gnus-summary-next-page)
                   (move-beginning-of-line 1)))
  (local-set-key (kbd "C-p")
                 (lambda ()
                   (interactive)
                   (delete-other-windows)
                   (forward-line -1)))
  (local-set-key (kbd "C-n")
                 (lambda ()
                   (interactive)
                   (delete-other-windows)
                   (forward-line 1)))
  (local-set-key (kbd "<up>")
                 (lambda ()
                   (interactive)
                   (delete-other-windows)
                   (forward-line -1)))
  (local-set-key (kbd "<down>")
                 (lambda ()
                   (interactive)
                   (delete-other-windows)
                   (forward-line 1))))

(add-hook 'gnus-summary-mode-hook #'eh-gnus-summary-setup)

;; visual
(setq gnus-treat-emphasize t
      gnus-treat-buttonize t
      gnus-treat-buttonize-head 'head
      gnus-treat-unsplit-urls 'last
      gnus-treat-leading-whitespace 'head
      gnus-treat-highlight-citation t
      gnus-treat-highlight-signature t
      gnus-treat-date-lapsed 'head
      gnus-treat-strip-trailing-blank-lines t
      gnus-treat-strip-cr t
      gnus-treat-overstrike nil
      gnus-treat-display-x-face t
      gnus-treat-display-face t
      gnus-treat-display-smileys nil
      gnus-treat-x-pgp-sig 'head)

;; 设置邮件报头显示的信息
(setq gnus-visible-headers
      (mapconcat 'regexp-quote
                 '("From:" "Newsgroups:" "Subject:" "Date:"
                   "Organization:" "To:" "Cc:" "Followup-To" "Gnus-Warnings:"
                   "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:"
                   "X-Mailer:" "Reply-To:" "X-Spam:" "X-Spam-Status:" "X-Now-Playing"
                   "X-Attachments" "X-Diagnostic" "X-RSS-URL")
                 "\\|"))

;; 设置邮件日期显示格式,使用两行日期，一行具体日期时间，
;; 另一行显示article, 距现在多长时间
(setq gnus-article-date-headers '(user-defined))
(setq gnus-article-time-format
      (lambda (time)
        (concat "X-Sent:   "
                (format-time-string "%Y年%m月%d日 星期%u %R" time)
                "\n"
                "X-Lasped: "
                (article-lapsed-string time))))

;; 用 Supercite 显示多种多样的引文形式
(setq sc-attrib-selection-list nil
      sc-auto-fill-region-p nil
      sc-blank-lines-after-headers 1
      sc-citation-delimiter-regexp "[>]+\\|\\(: \\)+"
      sc-cite-blank-lines-p nil
      sc-confirm-always-p nil
      sc-electric-references-p nil
      sc-fixup-whitespace-p t
      sc-nested-citation-p nil
      sc-preferred-header-style 4
      sc-use-only-preference-p nil)

;; 构建 threads 时抓取旧文章标题,
;; 注意： 网速不快时不要使用这个选项。
(setq gnus-fetch-old-headers nil)

;; 聚集 threads 的方式
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

;; Thread root 排序
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-number
        gnus-thread-sort-by-most-recent-date))

;; Subthread 排序
(setq gnus-subthread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-date))

;; 自动跳到第一个没有阅读的组
(add-hook 'gnus-switch-on-after-hook
          #'gnus-group-first-unread-group)

(add-hook 'gnus-summary-exit-hook
          #'gnus-group-first-unread-group)


;; * Footer
(provide 'eh-gnus-common)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-gnus-common.el ends here
