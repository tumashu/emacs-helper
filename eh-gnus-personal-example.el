;; ** 添加账户信息
;; Account file: "~/.authinfo.gpg" template

;; #### pop or imap ####
;; machine imap.163.com login xxx@163.com port 993 password PASSWORD
;; machine pop.163.com  login xxx@163.com port 995 password PASSWORD
;; machine imap.qq.com  login xxx@qq.com  port 993 password PASSWORD
;; machine pop.qq.com   login xxx@qq.com  port 995 password PASSWORD

;; #### smtp ####
;; machine smtp.163.com login xxxr@163.com port 465 password PASSWORD
;; machine smtp.qq.com  login xxx@qq.com   port 465 password PASSWORD

;; #### erc ####
;; machine irc.freenode.net login xxx port 6667 secret PASSWORD

;; ** Gnus 邮件基本设置.
(setq gnus-select-method
      '(nnml ""))

(setq gnus-secondary-select-methods
      '((nntp "news.gmane.io")
        (nntp "news.newsfan.net")
        (nnimap "imap.qq.com"
                (nnimap-inbox "INBOX")
                (nnimap-split-methods default)
                ;; (nnimap-expunge t)
                (nnimap-stream ssl)
                (nnimap-record-commands t)
                (nnimap-fetch-partial-articles t)
                (nnimap-split-methods nil)
                (nnimap-split-fancy nil))
        (nnimap "imap.163.com"
                (nnimap-inbox "INBOX")
                (nnimap-split-methods default)
                (nnimap-expunge t)
                (nnimap-stream ssl)
                ;; (nnimap-record-commands t)
                (nnimap-fetch-partial-articles t)
                (nnimap-split-methods nil)
                (nnimap-split-fancy nil))))

(setq mail-sources
      '((pop :server "pop.163.com"
             :user "myname@163.com"
             :port 995
             :stream ssl
             ;; 下载邮件之后，从服务器上删除。
             ;; :leave nil
             :postscript "notmuch new")
        (imap :server "imap.qq.com"
              :user "myname@qq.com"
              :port 993
              :stream ssl
              :fetchflag "\\Seen")))

;; ** 设置 gnus-posting-styles

;; 1. 邮件发送时字符编码设置.
;; 2. 发送邮件使用的方法.
(setq gnus-posting-styles
      '((".*"
         (signature "")
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 utf-8 gb2312 gbk utf-8 gb18030))))
        (message-mail-p
         (name    "My Name")
         (address "myname@163.com")
         (eval (setq smtpmail-stream-type 'ssl)
               (setq mm-coding-system-priorities
                     '(iso-8859-1 utf-8 gb2312 gbk utf-8 gb18030))))
        (".*newsfan.*"
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 gb2312 gbk gb18030 utf-8))))
        (".*cn99.*"
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 gb2312 gbk gb18030 utf-8))))))


;; Local Variables:
;; coding: utf-8-unix
;; End:
