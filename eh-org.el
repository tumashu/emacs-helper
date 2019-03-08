;;; eh-org.el --- Tumashu's org-mode configuation

;; * Header
;; Copyright (c) 2012-2016, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.2

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

;; * 代码                                                       :code:

(use-package org
  :commands org-mode
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb))
  :mode ("\\.org\\'" . org-mode)
  :mode ("\\.org_archive\\'" . org-mode)
  :ensure nil
  :init
  ;; 鼠标点击链接时，不打开链接，这样设置适合在 termux 中使用 org-agenda
  (when (eh-termux-p)
    (setq org-mouse-1-follows-link nil))
  :config

  (defvar eh-org-local-directory
    (cl-find-if #'file-exists-p
                '("d:/org/"
                  "e:/org/"
                  "f:/org/"
                  "~/org/"
                  "~/storage/shared/org/")))

  (defvar eh-org-remote-directory eh-org-local-directory)

  ;; 确保 tag 可以对齐
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5
                  org-level-6
                  org-level-7))
    (set-face-attribute face nil :height 1.0))
  ;; (set-face-attribute 'org-todo nil :box nil)

  ;; 自定义变量
  (setq org-todo-keywords
        '((type "TODO(t)" "ISSUE(i)" "MAYBE(m)"
                "|" "DONE(d!)" "FIXED(f!)" "CANCELED(c@)")))

  ;; (setq org-todo-keyword-faces
  ;;       '(("TODO" . (:foreground "DarkOrange1" :weight bold))
  ;;         ("MAYBE" . (:foreground "sea green"))
  ;;         ("DONE" . (:foreground "light sea green"))
  ;;         ("CANCELLED" . (:foreground "forest green")))

  (setq org-tags-exclude-from-inheritance
        '("proj"))

  (setq org-stuck-projects
        '("+proj/-MAYBE-DONE-CANCELED"
          ("TODO")
          nil ""))

  (setq org-insert-heading-respect-content nil)
  (setq org-log-done t)
  (setq org-startup-indented nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-id-link-to-org-use-id t)
  (setq org-log-into-drawer t)

  ;; org 文件显示内嵌图片的时候，首先缩放一下。
  (setq org-image-actual-width '(600))

  ;; 插入日期戳的命令不弹出日历表，太占地方。
  (setq org-read-date-popup-calendar nil)

  ;; 在 termux 环境下，C-TAB 无法正常工作，所以必须
  ;; 设置这个变量为 t, 否则展开 ARCHIVE 就很麻烦了。
  (setq org-cycle-open-archived-trees t)

  (setq org-refile-targets
        '((nil . (:maxlevel . 2))
          (org-agenda-files . (:maxlevel . 2))))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-active-region-within-subtree t)

  (defun eh-org-fill-paragraph ()
    "Fill org paragraph"
    (interactive)
    (let ((fill-column 10000000))
      (org-fill-paragraph)))

  (defun eh-org-ctrl-c-ctrl-c (&optional arg)
    "根据光标处内容，智能折行，比如，在表格中禁止折行。"
    (interactive "P")
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      (pcase type
        ((or `table `table-cell `table-row `item `plain-list)
         (toggle-truncate-lines 1))
        (_ (toggle-truncate-lines -1))))
    (org-ctrl-c-ctrl-c arg))

  (defun eh-org-set-tags-command (orig-fun &optional arg)
    (interactive "P")
    (if (functionp 'counsel-org-tag)
        (funcall-interactively 'counsel-org-tag)
      (funcall orig-fun arg)))

  (advice-add 'org-set-tags-command
              :around #'eh-org-set-tags-command)

  (defun eh-org-smart-truncate-lines (&optional arg)
    (interactive)
    (org-defkey org-mode-map "\C-c\C-c" 'eh-org-ctrl-c-ctrl-c))

  (defun eh-org-visual-line-mode ()
    (interactive)
    (setq visual-line-fringe-indicators '(nil nil))
    (visual-line-mode)
    (if visual-line-mode
        (setq word-wrap nil)))

  (defun eh-org-cdlatex ()
    (when (featurep 'cdlatex)
      (turn-on-org-cdlatex)))

  (add-hook 'org-mode-hook 'eh-org-cdlatex)
  (add-hook 'org-mode-hook 'eh-org-visual-line-mode)
  (add-hook 'org-mode-hook 'eh-org-smart-truncate-lines))

(use-package org-protocol
  :after org
  :ensure nil)

(use-package org-download
  :after org
  :ensure nil
  :config
  (setq org-download-method 'attach)
  (org-download-enable))

(use-package org-collector
  :ensure nil
  :after org)

(use-package ox
  :commands (org-export-as
             org-export-to-file)
  :ensure nil
  :config
  ;; Export language
  (setq org-export-default-language "zh-CN")
  (setq org-export-backends
        '(ascii beamer html latex md deck rss s5 odt))

  ;; org默认使用"_下标"来定义一个下标，使用"^上标"定义一个上标，
  ;; 但这种方式在中文环境中与下划线冲突。
  ;; 这里强制使用"_{下标}"来定义一个下标。"^{上标}"来定义一个上标。
  (setq org-export-with-sub-superscripts '{})
  (setq org-use-sub-superscripts '{}))

(use-package ox-html
  :after ox
  :ensure nil
  :config

  (setq eh-org-mathtoweb-file "~/bin/mathtoweb.jar")
  (setq org-latex-to-mathml-convert-command
        "java -jar %j -unicode -force -df %o %I"
        org-latex-to-mathml-jar-file
        eh-org-mathtoweb-file)

  (setq org-html-coding-system 'utf-8)
  (setq org-html-head-include-default-style t)
  (setq org-html-head-include-scripts t)

  (defun eh-org-wash-text (text backend info)
    "导出 org file 时，删除中文之间不必要的空格。"
    (when (org-export-derived-backend-p backend 'html)
      (let ((regexp "[[:multibyte:]]")
            (string text))
        ;; org-mode 默认将一个换行符转换为空格，但中文不需要这个空格，删除。
        (setq string
              (replace-regexp-in-string
               (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
               "\\1\\2" string))
        ;; 删除粗体之后的空格
        (dolist (str '("</b>" "</code>" "</del>" "</i>"))
          (setq string
                (replace-regexp-in-string
                 (format "\\(%s\\)\\(%s\\)[ ]+\\(%s\\)" regexp str regexp)
                 "\\1\\2\\3" string)))
        ;; 删除粗体之前的空格
        (dolist (str '("<b>" "<code>" "<del>" "<i>" "<span class=\"underline\">"))
          (setq string
                (replace-regexp-in-string
                 (format "\\(%s\\)[ ]+\\(%s\\)\\(%s\\)" regexp str regexp)
                 "\\1\\2\\3" string)))
        string)))

  (add-hook 'org-export-filter-headline-functions #'eh-org-wash-text)
  (add-hook 'org-export-filter-paragraph-functions #'eh-org-wash-text))

(use-package ox-latex
  :after ox
  :ensure nil
  :config
  ;; 不要在latex输出文件中插入\maketitle
  (setq org-latex-title-command "")
  (setq org-latex-date-format "%Y-%m-%d")
  ;; (setq org-latex-create-formula-image-program 'imagemagick)  ;默认支持中文
  (setq org-latex-create-formula-image-program 'dvipng)          ;速度较快，但默认不支持中文
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.5))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :html-scale 2.5)))

(use-package org2ctex
  :after ox-latex
  :ensure nil
  :config (org2ctex-toggle t))

(use-package ox-odt
  :after ox
  :ensure nil)

(use-package ox-ascii
  :after ox
  :ensure nil)

(use-package ox-md
  :after ox
  :ensure nil)

;; org-plus-contrib
(use-package ox-extra
  :after org
  :ensure nil
  :config
  ;; 如果一个标题包含TAG: “ignore” ,导出latex时直接忽略这个标题，
  ;; 但对它的内容没有影响。
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package ob-core
  :commands (org-babel-execute-maybe
             org-babel-execute-safely-maybe)
  :ensure nil
  :config
  ;; org-bable设置
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((org . t)
     ;; (R . t)
     (ditaa . nil)
     (dot . nil)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (mscgen . t)
     (latex . t)
     (ocaml . nil)
     (perl . t)
     (python . nil)
     (ruby . nil)
     (screen . nil)
     ;; (shell . nil)
     (sql . nil)
     (sqlite . nil)))

  (defun eh-org-align-babel-table (&optional info)
    "Align all tables in the result of the current babel source."
    (interactive)
    (when (not org-export-current-backend)
      (let ((location (org-babel-where-is-src-block-result nil info)))
        (when location
          (save-excursion
            (goto-char location)
            (when (looking-at (concat org-babel-result-regexp ".*$"))
              (while (< (point) (progn (forward-line 1) (org-babel-result-end)))
                (when (org-at-table-p)
                  (toggle-truncate-lines 1)
                  (org-table-align)
                  (goto-char (org-table-end)))
                (forward-line))))))))

  (defun eh-org-show-babel-image ()
    (when (not org-export-current-backend)
      (org-display-inline-images)))

  (add-hook 'org-babel-after-execute-hook #'eh-org-show-babel-image)
  (add-hook 'org-babel-after-execute-hook #'eh-org-align-babel-table))

(use-package parse-time
  :after org-agenda
  :ensure nil
  :config

  (setq parse-time-months
        (append '(("yi" . 1) ("er" . 2) ("san" . 3)
                  ("si" . 4) ("wu" . 5) ("liu" . 6)
                  ("qi" . 7) ("ba" . 8) ("jiu" . 9)
                  ("shi" . 10) ("shiyi" . 11) ("shier" . 12))
                parse-time-months))

  (setq parse-time-weekdays
        (append '(("zri" . 0) ("zyi" . 1) ("zer" . 2) ("zsan" . 3)
                  ("zsi" . 4) ("zwu" . 5) ("zliu" . 6))
                parse-time-weekdays)))

(use-package cal-china-x
  :after org-agenda
  :ensure nil
  :config
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
  (setq calendar-holidays eh-calendar-holidays))

(use-package calendar
  :after org-agenda
  :ensure nil
  :config
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
      (diary-chinese-anniversary lunar-month lunar-day year mark))))

(use-package org-archive
  :after org
  :ensure nil
  :config
  ;; 使用 org-archive-subtree 时，原来的 header 层级容易被打乱，而且容易
  ;; 因为保存不及时而导致 archive 文件内容丢失， 所以这个命令适合每月的
  ;; 大归档, 日常情况下，使用 ARCHIVE TAG 来隐藏已经完成的任务，安全又方便。
  ;; (setq org-archive-default-command 'org-archive-subtree)
  (setq org-archive-default-command 'org-archive-set-tag)
  )

(use-package org-attach
  :after org
  :ensure nil
  :config
  (setq org-attach-commit nil)
  (setq org-attach-store-link-p 'attached)

  (defun eh-org-attach-abbrev (file-name)
    "Return `org-attach-dir' for the current entry."
    (concat (file-name-as-directory (org-attach-dir 'CREATE))
            file-name))

  (add-to-list 'org-link-abbrev-alist
               (cons "attach" "file:%(eh-org-attach-abbrev)"))

  (defun eh-org-attach-link-complete (&optional arg)
    "Completion function for attach: link."
    (let* ((attach-dir (org-attach-dir 'CREATE))
           (file-link
            (let ((default-directory attach-dir))
              (org-file-complete-link)))
           (file-path (apply #'concat (cdr (split-string file-link ":")))))
      (format "attach:%s" file-path)))

  (org-link-set-parameters "attach" :complete #'eh-org-attach-link-complete)

  (defun eh-org-attach-sync-all ()
    (interactive)
    (org-map-entries #'org-attach-sync)
    (org-align-all-tags))
  )

(use-package autorevert
  :after org
  :config
  (add-hook 'org-mode-hook #'turn-on-auto-revert-mode))

(use-package org-agenda
  :bind (("C-c a" . org-agenda)
         :map org-agenda-mode-map
         ("g" . eh-org-agenda-redo-all)
         ("i" . (lambda () (interactive) (org-capture nil "s")))
         ("A" . org-agenda-archive-default-with-confirmation)
         ("J" . counsel-org-agenda-headlines)
         ("h" . ignore)
         ("y" . ignore)
         ("a" . ignore))
  :ensure nil
  :config

  ;; 加快 agenda 启动速度
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-inhibit-startup t)

  (setq org-agenda-custom-commands
        '(("f" "Find items which need to be refiled or archived"
           tags "+LEVEL=1+TODO={DONE\\|CANCELED\\|FIXED}"
           ((org-agenda-skip-archived-trees nil)))
          ("p" "Personal agenda"
           agenda ""
           ((org-agenda-files
             `(,eh-org-remote-directory))))
          ("i" "List of issues"
           tags "TODO={ISSUE}")))

  ;; 我更习惯类似 google 的搜索方式。
  (setq org-agenda-search-view-always-boolean t)
  (setq org-agenda-search-view-force-full-words nil)

  (add-to-list 'org-agenda-files eh-org-local-directory t)

  (defun eh-revert-org-buffers ()
    "Refreshes all opened org buffers."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name)
                   (string-match-p "org$" (buffer-file-name))
                   (file-exists-p (buffer-file-name))
                   (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed all opened org files."))

  (defun eh-org-agenda-redo-all (&optional exhaustive)
    (interactive "P")
    (eh-revert-org-buffers)
    (funcall-interactively #'org-agenda-redo-all)
    (message (substitute-command-keys
              "刷新完成，记得按快捷键 '\\[org-save-all-org-buffers]' 来保存更改。")))

  (setq org-agenda-span 'day)
  (setq org-agenda-window-setup 'only-window)
  (setq org-agenda-include-diary nil)

  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)

  (setq org-agenda-todo-list-sublevels t)
  (setq org-agenda-todo-ignore-scheduled t)

  (setq org-agenda-time-leading-zero nil)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          ""
          "----------------"))

  (setq  org-agenda-current-time-string
         "now - - - - - - - - - - - - -")

  (setq org-agenda-entry-text-leaders
        (if (eh-termux-p)
            "        "
          "                              "))

  (setq org-agenda-prefix-format
        (if (eh-termux-p)
            '((agenda  . " %?t%(eh-org-agenda-prefix-format-1)")
              (todo  . " %i")
              (tags  . " %i")
              (search . "%i"))
          '((agenda  . " %i %-20:c %?t%(eh-org-agenda-prefix-format-1)")
            (todo  . " %i %-20:c ")
            (tags  . " %i %-20:c ")
            (search . " %i %-20:c "))))

  (setq org-agenda-scheduled-leaders
        '("§计划 @" "§拖%02d  "))

  (setq org-agenda-deadline-leaders
        '("§截止 ?" "§剩%02d  " "§逾%02d  "))

  (defun eh-org-agenda-prefix-format-1 ()
    (if (or (equal extra "") (equal extra nil))
        (if (or (equal "" time) (equal nil time))
            "§提醒  "
          "  ")
      (let ((str1 (car org-agenda-scheduled-leaders))
            (str2 (car org-agenda-deadline-leaders))
            (s extra))
        (unless (or (equal "" time) (equal nil time))
          (setq s (replace-regexp-in-string (regexp-quote str1) " @" extra))
          (setq s (replace-regexp-in-string (regexp-quote str2) " ?" s)))
        (concat s "" (get-text-property 0 'extra-space extra)))))

  (setq org-agenda-format-date 'eh-org-agenda-format-date-aligned)

  (defun eh-org-agenda-format-date-aligned (date)
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date))
           (day (cadr date))
           (day-of-week (calendar-day-of-week date))
           (month (car date))
           (monthname (calendar-month-name month))
           (year (nth 2 date))
           (iso-week (org-days-to-iso-week
                      (calendar-absolute-from-gregorian date)))
           (weekyear (cond ((and (= month 1) (>= iso-week 52))
                            (1- year))
                           ((and (= month 12) (<= iso-week 1))
                            (1+ year))
                           (t year)))
           (cn-date (calendar-chinese-from-absolute
                     (calendar-absolute-from-gregorian date)))
           (cn-year (cadr cn-date))
           (cn-month (cl-caddr cn-date))
           (cn-day (cl-cadddr cn-date))
           (cn-month-name
            ["正月" "二月" "三月" "四月" "五月" "六月"
             "七月" "八月" "九月" "十月" "冬月" "腊月"])
           (cn-day-name
            ["初一" "初二" "初三" "初四" "初五" "初六" "初七" "初八" "初九" "初十"
             "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九" "二十"
             "廿一" "廿二" "廿三" "廿四" "廿五" "廿六" "廿七" "廿八" "廿九" "三十"
             "卅一" "卅二" "卅三" "卅四" "卅五" "卅六" "卅七" "卅八" "卅九" "卅十"])
           (extra (format "(%s%s%s%s)"
                          (if (or (eq org-agenda-current-span 'day)
                                  (= day-of-week 1)
                                  (= cn-day 1))
                              (aref cn-month-name (1-  (floor cn-month)))
                            "")
                          (if (or (= day-of-week 1)
                                  (= cn-day 1))
                              (if (integerp cn-month) "" "[闰]")
                            "")
                          (aref cn-day-name (1- cn-day))
                          (if (or (= day-of-week 1)
                                  (eq org-agenda-current-span 'day))
                              (format "，第%02d周" iso-week)
                            ""))))
      (format "%04d-%02d-%02d %s %s"
              year month day dayname extra))))

(use-package org-capture
  :after org
  :ensure nil
  :bind (("C-c c" . org-capture))
  :config
  (setq org-capture-templates
        (let ((local-inbox (concat (file-name-as-directory eh-org-local-directory) "INBOX.org")))
          `(("n" "Note" entry (file ,local-inbox)
             "* %?
  :PROPERTIES:
  :created: %U
  :END:

%i")
            ("a" "Appointment" entry (file ,local-inbox)
             "* %?
  %t
  :PROPERTIES:
  :created: %U
  :END:

%i")
            ("s" "Schedule" entry (file ,local-inbox)
             "* TODO %?
  SCHEDULED: %t
  :PROPERTIES:
  :created: %U
  :END:

%i")
            ("k" "Deadline" entry (file ,local-inbox)
             "* TODO %?
  DEADLINE: %t
  :PROPERTIES:
  :created: %U
  :END:

%i")
            ("d" "Deadline" entry (file ,local-inbox)
             "* TODO %?
  DEADLINE: %t
  :PROPERTIES:
  :created: %U
  :END:

%i")
            ("t" "TODO" entry (file ,local-inbox)
             "* TODO %?
  :PROPERTIES:
  :created: %U
  :END:

%i"
             )
            ("p" "Project" entry (file ,local-inbox)
             "* %?                      :PROJECT:
  :PROPERTIES:
  :created: %U
  :END:

** 为什么要启动此项目？想要解决什么问题？
项目背景分析，可以从战略、客户需求、利益相关者、
领导等方面思考，至少说出6条理由。

** 项目的目标和产出是什么？

** 头脑风暴
可以用 5W2H 法来协助思考
(Who? What? When? Where? Why? How? How much?)
点子越多越好，不求质量，只求数量。

** 项目的里程碑及时间计划

** 项目团队成员安排
")
            ("A" "Anniversary" plain (file+headline ,local-inbox "阳历生日")
             "\%\%%(or \"(org-anniversary 1985 4 17)\") 今天是%?%d阳历岁生日")
            ("C" "Chinese Anniversary" plain (file+headline ,local-inbox "农历生日")
             "\%\%%(or \"(eh-org-chinese-anniversary 1985 4 17)\") 今天是%?%d岁农历生日")
            ("H" "Diary" plain (file+headline ,local-inbox "节假日")
             "\%\%%(or \"(org-calendar-holiday)\")"))))

  ;; (defun eh-org-capture (orig-fun &optional goto keys)
  ;;   "Advice function of org-capture."
  ;;   (interactive)
  ;;   (funcall orig-fun goto keys)
  ;;   (delete-other-windows))

  ;; (advice-add 'org-capture :around #'eh-org-capture)
  )


;; * Footer
(provide 'eh-org)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8-unix
;; End:

;;; eh-org.el ends here
