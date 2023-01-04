;;; eh-org.el --- Tumashu's org-mode configuation    -*- lexical-binding: t; -*-

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

;; ** org
(require 'org)
(require 'org-attach)
(require 'org-archive)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))

(defvar eh-org-directory
  (expand-file-name
   (cl-find-if #'file-exists-p
               '("d:/org/"
                 "e:/org/"
                 "f:/org/"
                 "~/org/"
                 "~/storage/shared/org/"))))

;; 这个附件设置只适合我自己，千万别乱抄。
(setq org-attach-id-dir
      (concat (file-name-as-directory
               eh-org-directory)
              "data/"))

(dolist (f '(org-open-file))
  (advice-add f :around 'eh-find-file))

(setq org-todo-keywords
      '((type "TODO(t)" "|" "DONE(d!)" "CANCELED(c!)")))

(setq org-tags-exclude-from-inheritance
      '("proj"))

(setq org-tag-persistent-alist
      '(("proj")
        ("ref")
        ("ATTACH")))

(setq org-stuck-projects
      '("+proj/-DONE-CANCELED"
        ("TODO")
        nil ""))

(setq org-use-fast-tag-selection t)
(setq org-complete-tags-always-offer-all-agenda-tags t)

(defun eh-org-fast-tag-selection (current _inherited table &optional _todo-table)
  (let* ((crm-separator "[ 	]*[:,][ 	]*")
         (current-tags (cl-copy-list current))
         (buf (current-buffer))
         (n (length current-tags))
         (max 5)
         (prompt (if (> n 0)
                     (format "Tag (%s%s): "
                             (mapconcat #'identity
                                        (cl-subseq current-tags 0 (min n max))
                                        ", ")
                             (if (> n max)
                                 " ..."
                               ""))
                   "Tag: "))
         tab-tags tags)

    (condition-case nil
        (unless tab-tags
          (setq tab-tags
                (delq nil
                      (mapcar (lambda (x)
                                (let ((item (car-safe x)))
                                  (and (stringp item)
                                       (list item))))
                              (org--tag-add-to-alist
                               (with-current-buffer buf
                                 (org-get-buffer-tags))
                               table))))))

    (setq tags (completing-read-multiple
                prompt (mapcar
                        (lambda (x)
                          (if (member (car x) current-tags)
                              (cons (propertize (car x) 'face '(:box t)) (cdr x))
                            x))
                        tab-tags)))

    (dolist (tg (delete-dups (remove "" tags)))
      (when (string-match "\\S-" tg)
        (if (member tg current-tags)
	    (setq current-tags (delete tg current-tags))
	  (push tg current-tags))))
    (org-make-tag-string current-tags)))

(advice-add 'org-fast-tag-selection :override #'eh-org-fast-tag-selection)

(defun eh-org-end-of-id-line ()
  (when (eq major-mode 'org-mode)
    (org-back-to-heading t)
    (org-id-get-create)
    (search-forward ":ID:")
    (end-of-line)
    (org-fold-show-all '(drawers))))

(setq org-insert-heading-respect-content nil)
(setq org-log-done t)
(setq org-startup-indented nil)
(setq org-adapt-indentation 'headline-data)
(setq org-edit-src-content-indentation 0)
(setq org-id-link-to-org-use-id t)
(setq org-log-into-drawer t)

;; org 文件显示内嵌图片的时候，首先缩放一下。
(setq org-image-actual-width t)

;; 插入日期戳的命令不弹出日历表，太占地方。
(setq org-read-date-popup-calendar nil)

(defun eh-org-refile-agenda-files ()
  (org-agenda-files t t))

(setq org-refile-targets
      '((nil . (:maxlevel . 1))
        (eh-org-refile-agenda-files . (:maxlevel . 1))))

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

(defun eh-org-smart-truncate-lines (&optional _arg)
  (interactive)
  (org-defkey org-mode-map "\C-c\C-c" 'eh-org-ctrl-c-ctrl-c))

(defun eh-org-visual-line-mode ()
  (interactive)
  (setq visual-line-fringe-indicators '(nil nil))
  (visual-line-mode)
  (if visual-line-mode
      (setq word-wrap nil)))

(add-hook 'org-mode-hook 'eh-org-visual-line-mode)
(add-hook 'org-mode-hook 'eh-org-smart-truncate-lines)

(require 'autorevert)
(add-hook 'org-mode-hook #'turn-on-auto-revert-mode)

;; (require 'org-protocol)

;; ** org-export
(require 'ox-odt)
(require 'ox-org)
(require 'ox-ascii)
(require 'ox-md)
(require 'ox-html)

(setq org-export-default-language "zh-CN")

;; org默认使用"_下标"来定义一个下标，使用"^上标"定义一个上标，
;; 但这种方式在中文环境中与下划线冲突。
;; 这里强制使用"_{下标}"来定义一个下标。"^{上标}"来定义一个上标。
(setq org-export-with-sub-superscripts '{})
(setq org-use-sub-superscripts '{})

;;; ** export html
(setq org-html-coding-system 'utf-8)
(setq org-html-head-include-default-style t)
(setq org-html-head-include-scripts t)
(setq org-html-validation-link nil)

(defun eh-org-wash-text (text backend _info)
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
(add-hook 'org-export-filter-paragraph-functions #'eh-org-wash-text)

;; ** org-bable设置
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)

(defun eh-org-show-babel-image ()
  (when (not org-export-current-backend)
    (org-display-inline-images)))

(add-hook 'org-babel-after-execute-hook #'eh-org-show-babel-image)

;; *** org babel other modules
(require 'ob-org)
(require 'ob-emacs-lisp)
(require 'ob-python)

;; ** org-archive
;; 日常情况下，使用 ARCHIVE TAG 来隐藏已经完成的任务，安全又方便。
(setq org-archive-default-command 'org-archive-set-tag)

;; 使用 org-archive-subtree 时，保持一级目录结构。
(defun eh-org-archive-subtree (orig_func &rest args)
  (let* ((tags (org-get-tags))
         (location (org-archive--compute-location
		    (or (org-entry-get nil "ARCHIVE" 'inherit)
			org-archive-location)))
	 (archive-file (car location))
         (subheading-p (save-excursion
                         (org-back-to-heading)
                         (> (org-outline-level) 1)))
         (top-headline (car (org-get-outline-path t)))
         (org-archive-location
          (if subheading-p
              (concat (car (split-string org-archive-location "::"))
                      "::* "
                      top-headline)
            org-archive-location)))
    (apply orig_func args)
    (when (and subheading-p archive-file tags)
      (with-current-buffer (find-file-noselect archive-file)
        (save-excursion
          (while (org-up-heading-safe))
          (org-set-tags tags))))))

(advice-add 'org-archive-subtree :around #'eh-org-archive-subtree)

;; ** org-attach
(setq org-attach-file-list-property nil)
(setq org-attach-store-link-p 'attached)
(setq org-attach-sync-delete-empty-dir t)

(defun eh-org-attach-sync-all ()
  (interactive)
  (org-map-entries #'org-attach-sync)
  (org-align-tags 'all))

(defun eh-org-attach-reveal ()
  (interactive)
  (let (marker)
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
      (call-interactively 'org-attach-reveal))))

(defun eh-org-attach-subtree ()
  (interactive)
  (when (yes-or-no-p "确定将 subtree 转移到 attach 目录中？ ")
    (org-back-to-heading t)
    (let* ((case-fold-search nil)
           (org-export-with-tags t)
           (filename (expand-file-name
                      (concat
                       (org-element-property
                        :title (org-element-at-point))
                       "-"
                       (format-time-string "%Y%m%dT%H%M%S")
                       ".org")
                      (org-attach-dir t))))
      (org-export-to-file 'org filename nil t)
      (org-end-of-meta-data)
      (delete-region (point) (org-end-of-subtree t)))))

;; ** org-capture
(require 'org-capture)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      (let ((file (concat (file-name-as-directory eh-org-directory) "projects.org")))
        `(("n" "Note" entry (file ,file)
           "* %?
  :PROPERTIES:
  :created: %U
  :END:

%i")
          ("s" "Schedule" entry (file+headline ,file "待整理")
           "* TODO %?
  SCHEDULED: %t
  :PROPERTIES:
  :created: %U
  :END:

%i")
          ("d" "Deadline" entry (file+headline ,file "待整理")
           "* TODO %?
  DEADLINE: %t
  :PROPERTIES:
  :created: %U
  :END:

%i"))))

(defun eh-org-capture-note ()
  (interactive)
  (org-capture nil "n"))

(defun eh-org-capture-schedule ()
  (interactive)
  (org-capture nil "s"))

(defun eh-org-capture-refresh-agenda (&rest _)
  (when (eq major-mode 'org-agenda-mode)
    (eh-org-agenda-redo-all)))

(advice-add 'org-capture-finalize :after #'eh-org-capture-refresh-agenda)
(advice-add 'org-capture-refile :after #'eh-org-capture-refresh-agenda)

;; ** org-agenda
(require 'org-agenda)

(global-set-key (kbd "C-c a") 'org-agenda)
(define-key org-agenda-mode-map (kbd "SPC") 'org-agenda-switch-to)
(define-key org-agenda-mode-map (kbd "i") 'org-agenda-switch-to)
(define-key org-agenda-mode-map (kbd "g") 'eh-org-agenda-redo-all)
(define-key org-agenda-mode-map (kbd "A") 'org-agenda-archive-default-with-confirmation)

(defun eh-org-agenda-kill ()
  (interactive)
  (call-interactively #'org-agenda-kill)
  (eh-org-agenda-redo-all))

(define-key org-agenda-mode-map (kbd "C-k") #'eh-org-agenda-kill)
(define-key org-agenda-mode-map (kbd "k") #'eh-org-agenda-kill)
(define-key org-agenda-mode-map (kbd "c") #'eh-org-capture-schedule)
(define-key org-agenda-mode-map (kbd "C") #'eh-org-capture-note)

(define-key org-agenda-mode-map (kbd "h") 'ignore)
(define-key org-agenda-mode-map (kbd "y") 'ignore)
(define-key org-agenda-mode-map (kbd "a") 'ignore)

;; 取消下面关于 archive 的快捷键，容易误操作
(org-defkey org-agenda-mode-map "\C-c\C-x\C-a" 'ignore)
(org-defkey org-agenda-mode-map "\C-c\C-xa"    'ignore)
(org-defkey org-agenda-mode-map "\C-c\C-xA"    'ignore)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-s" 'ignore)
(org-defkey org-agenda-mode-map "$"            'ignore)

;; 加快 agenda 启动速度
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-inhibit-startup t)

;; 我更习惯类似 google 的搜索方式。
(setq org-agenda-search-view-always-boolean t)
(setq org-agenda-search-view-force-full-words nil)

(add-to-list 'org-agenda-files eh-org-directory t)
(add-to-list 'org-agenda-files (concat (file-name-as-directory eh-org-directory) "orgzly") t)
(make-directory (concat (file-name-as-directory eh-org-directory) "orgzly") t)

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

(defun eh-org-agenda-redo-all (&optional arg)
  (interactive "P")
  (eh-revert-org-buffers)
  (funcall-interactively #'org-agenda-redo-all arg)
  (message (substitute-command-keys
            "刷新完成，记得按快捷键 '\\[org-save-all-org-buffers]' 来保存更改。")))

(setq org-agenda-span 'day)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-include-diary nil)

(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)

(setq org-agenda-todo-list-sublevels t)
(setq org-agenda-todo-ignore-scheduled t)

(setq org-agenda-breadcrumbs-separator " ~> ")

(setq org-agenda-prefix-format
      '((agenda  . " %i %-14:c%?-12t% s")
        (todo  . " %i %-14:c")
        (tags  . " %i %-14:c")
        (search . " %i %-14:c")))

(setq org-agenda-format-date 'eh-org-agenda-format-date-aligned)

(defun eh-org-agenda-format-date-aligned (date)
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date))
         (day (cadr date))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         (cn-date (calendar-chinese-from-absolute
                   (calendar-absolute-from-gregorian date)))
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
         (extra (format "(%s%s%s%s%s)"
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
                            (let ((holiday (mapconcat #'identity (calendar-check-holidays date) ", ")))
                              (if (> (length holiday) 0)
                                  (concat ", " holiday)
                                ""))
                          "")
                        (if (or (= day-of-week 1)
                                (eq org-agenda-current-span 'day))
                            (format ", 第%02d周" iso-week)
                          ""))))
    (format "%04d-%02d-%02d %s %s"
            year month day dayname extra)))

(defun eh-org-agenda-jump-to-first-item ()
  ;; 用 (goto-char (point-min)) 不管用，我估计是切换 tab 的缘故。
  (let ((window (get-buffer-window org-agenda-buffer)))
    (when (windowp window)
      (set-window-point window (point-min))
      (org-agenda-next-item 1))))

(add-hook 'org-agenda-finalize-hook #'eh-org-agenda-jump-to-first-item 100)

;; Org super agenda
(require 'org-super-agenda)
(setq org-super-agenda-unmatched-name "未分组")
(setq org-super-agenda-groups
      '((:name "Today" :time-grid t)
        (:name "待整理" :tag "待整理")
        (;; :auto-parent 和 :auto-outline-path 无法很好的处理一级
         ;; headline, 这里首先将一级 headline 归类。
         :name "(ROOT)"
         :pred (lambda (item)
                 (org-super-agenda--when-with-marker-buffer
                   (org-super-agenda--get-marker item)
                   (equal (org-current-level) 1))))
        (:auto-parent t)))

(org-super-agenda-mode 1)

(cl-pushnew
 '("a" "[org-super-agenda] Agenda for current week or day." agenda ""
   ((org-agenda-remove-tags t)
    (org-super-agenda-header-separator "\n")
    (org-super-agenda-final-group-separator "\n")))
 org-agenda-custom-commands)

;; org ql
(require 'org-ql)

;; 公文和协议
(defun eh-org-update-all-headlines ()
  (interactive)
  (org-map-entries #'eh-org-update-headline))

(defun eh-org-update-headline ()
  (interactive)
  (eh-org-update-gongwen-headline)
  (eh-org-update-xieyi-headline))

(defun eh-org-update-gongwen-headline ()
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (equal (org-entry-get (point) "CATEGORY")
                    "党政机关公文"))
    (let* ((title (org-entry-get (point) "标题"))
           (daizi (or (org-entry-get (point) "发文机关代字") ""))
           (year (or (org-entry-get (point) "年份") ""))
           (num (or (org-entry-get (point) "发文顺序号") ""))
           (data (or (org-entry-get (point) "成文日期") ""))
           (organization (org-entry-get (point) "发文机关标志"))
           (organization (if (= (length organization) 0)
                             "未知发文机关"
                           organization))
           (zihao1 (org-entry-get (point) "发文字号"))
           (zihao (cond ((and (> (length daizi) 0)
                              (> (length year) 0)
                              (> (length num) 0))
                         (format "%s〔%s〕%s号" daizi year num))
                        ;; 以前记录的一些公文信息，只记录字号，没有记录代字，年份和
                        ;; 顺序号。
                        ((> (length zihao1) 0)
                         (replace-regexp-in-string
                          (regexp-quote "]") "〕"
                          (replace-regexp-in-string
                           (regexp-quote "[") "〔"
                           zihao1)))
                        ((and (= (length daizi) 0)
                              (> (length data) 0))
                         (format "%s %s" organization data))
                        (t organization)))
           (prefix "[公文]"))
      (when (> (length title) 0)
        (when (and title (string-match-p "《" title))
          (setq title
                (replace-regexp-in-string
                 "《" "〈"
                 (replace-regexp-in-string "》" "〉" title))))
        (org-id-get-create)
        (org-edit-headline
         (format "%s《%s》（%s）" prefix title zihao))
        (when (string-match-p "〔" zihao)
          (org-set-property "发文字号" zihao))))))

(defun eh-org-update-xieyi-headline ()
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (equal (org-entry-get (point) "CATEGORY")
                    "协议或合同"))
    (let* ((project (org-entry-get (point) "项目或服务名称"))
           (weituo (or (org-entry-get (point) "委托方") ""))
           (chengjie (or (org-entry-get (point) "承接方") ""))
           (jiakuan (or (org-entry-get (point) "价款（元）") ""))
           (date (org-entry-get (point) "签署日期"))
           (prefix "[协议]"))
      (when (> (length project) 0)
        (org-id-get-create)
        (org-edit-headline
         (format "%s《%s》（%s %s %s %s）" prefix project weituo chengjie jiakuan date))))))

(add-hook 'write-file-functions #'eh-org-update-headline 100)
(add-hook 'org-capture-prepare-finalize-hook #'eh-org-update-headline 100)

;; * Footer
(provide 'eh-org)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org.el ends here
