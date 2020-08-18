;; ** Gnus 邮件基本设置.
(setq gnus-select-method '(nnml ""))
(setq mail-sources
      '((pop :server "pop.163.com"
             :user "myname@163.com"
             :port 995
             :stream ssl
             :leave t)
        (imap :server "imap.qq.com"
              :user "myname@qq.com"
              :port 993
              :stream ssl
              :fetchflag "\\Seen")))

;; ** Account file: "~/.authinfo.gpg" template

;; #### pop or imap ####
;; machine imap.163.com login xxx@163.com port 993 password PASSWORD
;; machine pop.163.com  login xxx@163.com port 995 password PASSWORD
;; machine imap.qq.com  login xxx@qq.com  port 993 password PASSWORD
;; machine pop.qq.com   login xxx@qq.com  port 995 password PASSWORD

;; #### smtp ####
;; machine smtp.163.com login xxxr@163.com port 465 password PASSWORD user-full-name "XXX" user-mail-address xxx@163.com
;; machine smtp.qq.com  login xxx@qq.com   port 465 password PASSWORD user-full-name "XXX" user-mail-address xxx@qq.com

;; #### erc ####
;; machine irc.freenode.net login xxx port 6667 secret PASSWORD

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
         ;; ("Cc" "My Name <myname@163.com>")
         ("X-Message-SMTP-Method" "smtp smtp.163.com 465")
         (eval (setq smtpmail-stream-type 'ssl)
               (setq mm-coding-system-priorities
                     '(iso-8859-1 utf-8 gb2312 gbk utf-8 gb18030))))
        (".*newsfan.*"
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 gb2312 gbk gb18030 utf-8))))
        (".*cn99.*"
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 gb2312 gbk gb18030 utf-8))))))

;; ** 其他一些常见的配置例子
;; #+BEGIN_EXAMPLE
;; (setq gnus-select-method
;;       '(nnimap "QQmail"
;;         (nnimap-address "imap.qq.com")
;;         (nnimap-stream ssl)))

;; (add-to-list 'gnus-secondary-select-methods
;;       '(nntp "news.gmane.io"))

;; (add-to-list 'gnus-secondary-select-methods
;;       '(nntp "news.newsfan.net"))
;; #+END_EXAMPLE

;; Local Variables:
;; coding: utf-8-unix
;; End:
