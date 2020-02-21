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

;; ** org
;; 鼠标点击链接时，不打开链接，这样设置适合在 termux 中使用 org-agenda
(when (eh-termux-p)
  (setq org-mouse-1-follows-link nil))

(require 'org)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(define-key org-mode-map (kbd "<f1>") 'eh-org-attach-reveal)
(define-key org-mode-map (kbd "<f3>") 'share2computer-org)

(defvar eh-org-local-directory
  (cl-find-if #'file-exists-p
              '("d:/org/"
                "e:/org/"
                "f:/org/"
                "~/org/"
                "~/storage/shared/org/")))

(defvar eh-org-remote-directory eh-org-local-directory)

(dolist (f '(org-open-file))
  (advice-add f :around 'eh-find-file))

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

(setq org-todo-keywords
      '((type "TODO(t)" "ISSUE(i)" "MAYBE(m)"
              "|" "DONE(d!)" "FIXED(f!)" "CANCELED(c@)")))

;; (setq org-todo-keyword-faces
;;       '(("TODO" . (:foreground "DarkOrange1" :weight bold))
;;         ("MAYBE" . (:foreground "sea green"))
;;         ("DONE" . (:foreground "light sea green"))
;;         ("CANCELLED" . (:foreground "forest green")))

(setq org-fast-tag-selection-single-key nil)

(setq org-tags-exclude-from-inheritance
      '("proj"))

(setq org-tag-persistent-alist
      '(("proj" . ?p)
        ("ref"  . ?r)
        ("ATTACH"  . ?a)))

(setq org-stuck-projects
      '("+proj/-MAYBE-DONE-CANCELED"
        ("TODO")
        nil ""))

(defun eh-org-set-tags-command (&optional arg)
  (interactive)
  (let ((org-current-tag-alist
         (org--tag-add-to-alist
          (org--tag-add-to-alist org-current-tag-alist
                                 (org-get-buffer-tags))
          (eh-org-brain-as-tags))))
    (funcall-interactively #'counsel-org-tag)))

(advice-add 'org-set-tags-command :override #'eh-org-set-tags-command)

(defun eh-counsel-org-tag-action (orig_func x)
  (funcall orig_func x)
  (when (functionp 'eh-org-brain-add-entry)
    (eh-org-brain-add-entry x)
    (message ""))
  (when (memq this-command '(ivy-done
                             ivy-alt-done
                             ivy-immediate-done))
    (unless (member x counsel-org-tags)
      (if (y-or-n-p (format "Really remove tag %S?" x))
          (message "WARN: tag %S has been removed." x)
        (push x counsel-org-tags)
        (counsel-org--set-tags)
        (message "")))))

(advice-add 'counsel-org-tag-action :around #'eh-counsel-org-tag-action)

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
(add-hook 'org-mode-hook 'eh-org-smart-truncate-lines)

(require 'autorevert)
(add-hook 'org-mode-hook #'turn-on-auto-revert-mode)

(require 'org-protocol)
(require 'org-collector)

;; ** org-export
(require 'ox-odt)
(require 'ox-org)
(require 'ox-ascii)
(require 'ox-md)
(require 'ox-html)
(require 'ox-ascii)
(require 'ox-beamer)

(setq org-export-default-language "zh-CN")

;; org默认使用"_下标"来定义一个下标，使用"^上标"定义一个上标，
;; 但这种方式在中文环境中与下划线冲突。
;; 这里强制使用"_{下标}"来定义一个下标。"^{上标}"来定义一个上标。
(setq org-export-with-sub-superscripts '{})
(setq org-use-sub-superscripts '{})

(setq eh-org-mathtoweb-file "~/bin/mathtoweb.jar")
(setq org-latex-to-mathml-convert-command
      "java -jar %j -unicode -force -df %o %I"
      org-latex-to-mathml-jar-file
      eh-org-mathtoweb-file)

;;; ** export html
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
(add-hook 'org-export-filter-paragraph-functions #'eh-org-wash-text)

;; *** export latex
;; 不要在latex输出文件中插入\maketitle
(setq org-latex-title-command "")

(setq org-latex-date-format "%Y-%m-%d")
;; (setq org-latex-create-formula-image-program 'imagemagick)  ;默认支持中文
(setq org-latex-create-formula-image-program 'dvipng)          ;速度较快，但默认不支持中文
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.5))
(setq org-format-latex-options(plist-put org-format-latex-options :html-scale 2.5))

(require 'org2ctex)
(org2ctex-toggle t)

;; ** org-plus-contrib
(require 'ox-extra)
;; 如果一个标题包含TAG: “ignore” ,导出latex时直接忽略这个标题，
;; 但对它的内容没有影响。
(ox-extras-activate '(latex-header-blocks ignore-headlines))

;; ** org-bable设置
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
(add-hook 'org-babel-after-execute-hook #'eh-org-align-babel-table)

;; ** org-archive
;; 使用 org-archive-subtree 时，原来的 header 层级容易被打乱，而且容易
;; 因为保存不及时而导致 archive 文件内容丢失， 所以这个命令适合每月的
;; 大归档, 日常情况下，使用 ARCHIVE TAG 来隐藏已经完成的任务，安全又方便。
;; (setq org-archive-default-command 'org-archive-subtree)
(setq org-archive-default-command 'org-archive-set-tag)

;; ** org-attach
(setq org-attach-file-list-property nil)
(setq org-attach-store-link-p 'attached)

(defun eh-org-attach-sync-all ()
  (interactive)
  (org-map-entries #'org-attach-sync)
  (org-align-all-tags))

(defun eh-org-attach-reveal ()
  (interactive)
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

;; ** org-agenda
(require 'org-agenda)
(require 'org-super-agenda)

(global-set-key (kbd "C-c a") 'org-agenda)
(define-key org-agenda-mode-map (kbd "<f1>") 'eh-org-attach-reveal)
(define-key org-agenda-mode-map (kbd "<f3>") 'share2computer-org)
(define-key org-agenda-mode-map (kbd "SPC") 'eh-org-agenda-show-and-scroll-up)
(define-key org-agenda-mode-map (kbd "<return>") 'eh-org-agenda-show-and-scroll-up)
(define-key org-agenda-mode-map (kbd "g") 'eh-org-agenda-redo-all)
(define-key org-agenda-mode-map (kbd "i") (lambda () (interactive) (org-capture nil "s")))
(define-key org-agenda-mode-map (kbd "A") 'org-agenda-archive-default-with-confirmation)
(define-key org-agenda-mode-map (kbd "h") 'ignore)
(define-key org-agenda-mode-map (kbd "y") 'ignore)
(define-key org-agenda-mode-map (kbd "a") 'ignore)

;; (defun eh-org-agenda-delete-window (&rest args)
;;   (delete-other-windows))

;; (dolist (x '(org-agenda-next-line
;;              org-agenda-previous-line
;;              org-agenda-next-item
;;              org-agenda-previous-item))
;;   (advice-add x :after 'eh-org-agenda-delete-window))

(defvar eh-org-agenda-show-window-point nil)
(defun eh-org-agenda-show-and-scroll-up (&optional arg)
  (interactive "P")
  (let ((win (selected-window)))
    (if (and (window-live-p org-agenda-show-window)
             (eq this-command last-command))
        (progn
          (select-window org-agenda-show-window)
          (if (eq eh-org-agenda-show-window-point (window-point))
              (progn
                (goto-char (point-min))
                (message "已经滚动到底，返回第一行！"))
            (ignore-errors (scroll-up))
            (setq eh-org-agenda-show-window-point (window-point))))
      (org-agenda-goto t)
      (org-show-entry)
      (let ((org-indirect-buffer-display 'current-window))
        (org-tree-to-indirect-buffer)
        ;; 隐藏 indirect buffer
        (rename-buffer (concat " " (buffer-name))))
      (if arg (org-cycle-hide-drawers 'children)
        (org-with-wide-buffer
         (narrow-to-region (org-entry-beginning-position)
                           (org-entry-end-position))
         (org-show-all '(drawers))))
      (when arg )
      (setq org-agenda-show-window (selected-window)))
    (select-window win)))

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

(defun eh-org-agenda-redo-all (&optional arg)
  (interactive "P")
  (let ((eh-org-agenda-category-width
         (if arg
             (apply #'max
                    (org-map-entries
                     (lambda ()
                       (string-width
                        (or (org-get-category (point)) "")))
                     nil 'agenda))
           eh-org-agenda-category-width)))
    (eh-revert-org-buffers)
    (funcall-interactively #'org-agenda-redo-all)
    (message (substitute-command-keys
              "刷新完成，记得按快捷键 '\\[org-save-all-org-buffers]' 来保存更改。"))))

(setq org-agenda-span 'day)
(setq org-agenda-window-setup 'current-window)
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

(setq org-agenda-entry-text-leaders " > ")

;; (defun eh-org-agenda-entry-text-cleanup ()
;;   (while (re-search-forward "^[[:space:]	\t]+" nil t)
;;     (replace-match "" nil t)))

;; (add-hook 'org-agenda-entry-text-cleanup-hook
;;           #'eh-org-agenda-entry-text-cleanup)
(setq org-agenda-remove-tags t)
(setq org-agenda-breadcrumbs-separator " ~> ")

(add-hook 'org-agenda-finalize-hook
          #'eh-org-agenda-change-breadcrumbs-color)

(defun eh-org-agenda-change-breadcrumbs-color ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-agenda-breadcrumbs-separator nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face '(:foreground "green" :bold t)))))

(setq org-agenda-prefix-format
      (if (eh-termux-p)
          '((agenda  . " %(eh-org-agenda-prefix-format nil t t)")
            (todo  . " %i")
            (tags  . " %i")
            (search . "%i"))
        '((agenda  . " %i%(eh-org-agenda-prefix-format t t t)")
          (todo  . " %i%(eh-org-agenda-prefix-format t)")
          (tags  . " %i%(eh-org-agenda-prefix-format t)%b")
          (search . " %i%(eh-org-agenda-prefix-format t)%b"))))

(setq org-agenda-scheduled-leaders
      '("&计划 @" "&拖%02d  "))

(setq org-agenda-deadline-leaders
      '("&截止 ?" "&剩%02d  " "&逾%02d  "))

(defun eh-org-agenda-substring (string n)
  (if (> (string-width string) n)
      (let ((chars (string-to-list string))
            (str ""))
        (while chars
          (setq str (concat str (char-to-string (pop chars))))
          (when (> (+ (string-width str) 3) n)
            (setq chars nil)
            (setq str (substring str 0 -1))))
        (concat str "..."))
    string))

(defvar eh-org-agenda-category-width 13)

(defun eh-org-agenda-prefix-format (&optional show-category show-time show-extra)
  (concat
   (if show-category
       (let* ((w eh-org-agenda-category-width)
              (formater (format "%%-%ss " (+ 1 w)))
              (string (eh-org-agenda-substring category w)))
         (format formater
                 (if (> (length string) 0)
                     (concat string ":")
                   "")))
     "")
   (if (or (not show-time)
           (= (length time) 0))
       ""
     (format "%s" (concat time "")))
   (if show-extra
       (if (= (length extra) 0)
           (if (= (length time) 0)
               "§提醒  "
             "  ")
         (let ((str1 (car org-agenda-scheduled-leaders))
               (str2 (car org-agenda-deadline-leaders))
               (s extra))
           (unless (= (length time) 0)
             (setq s (replace-regexp-in-string (regexp-quote str1) " @" extra))
             (setq s (replace-regexp-in-string (regexp-quote str2) " ?" s)))
           (concat s "" (get-text-property 0 'extra-space extra))))
     "")))

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
            year month day dayname extra)))

;; ** org-capture
(require 'org-capture)

(global-set-key (kbd "C-c c") 'org-capture)
;; 保存的时候强制 refile
(define-key org-capture-mode-map (kbd "C-c C-c") 'org-capture-refile)

(setq org-capture-templates
      (let ((local-inbox (concat (file-name-as-directory eh-org-local-directory) "INBOX.org")))
        `(("n" "Note" entry (file ,local-inbox)
           "* %?
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
          ("d" "Deadline" entry (file ,local-inbox)
           "* TODO %?
  DEADLINE: %t
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

;; ** org-download
(require 'org-download)
(define-key org-mode-map (kbd "<f2>") 'org-download-screenshot)
(setq org-download-method 'attach)
(setq org-download-display-inline-images 'posframe)
(setq org-download-screenshot-file (concat temporary-file-directory "image.png"))
(setq org-download-image-attr-list
      '("#+ATTR_HTML: :width 80% :align center"))
(when (eq system-type 'windows-nt)
  (setq org-download-screenshot-method "convert clipboard: %s"))
(org-download-enable)

;; ** org-ql
(require 'org-ql)

(global-set-key (kbd "C-c j") 'eh-org-query)
(global-set-key (kbd "C-c l") 'eh-org-query-picklink)

(setq org-ql-default-predicate 'heading)

(defun eh-org-query ()
  (interactive)
  (ivy-read "Org query: " #'eh-org-query-collect
            :dynamic-collection t
            :initial-input (eh-org-query-string)
            :action #'eh-org-query-goto))

(defun eh-org-query-picklink ()
  (interactive)
  (ivy-read "Org query: " #'eh-org-query-collect
            :dynamic-collection t
            :initial-input (eh-org-query-string)
            :action #'eh-org-query-insert-link))

(defun eh-org-query-string ()
  (when mark-active
    (buffer-substring-no-properties
     (region-beginning) (region-end))))

(defun eh-tree-map (fn tree)
  "Apply FN to each element of TREE while preserving the tree structure."
  (cond
   ((null tree) nil)
   ((consp tree)
    (if (memq (car tree) '(regexp r heading h path))
        (funcall fn tree)
      (mapcar (lambda (x) (eh-tree-map fn x)) tree)))
   (t tree)))

(defvar eh-org-query-collect-timer nil)

(defun eh-org-query-collect (input)
  (when eh-org-query-collect-timer
    (cancel-timer eh-org-query-collect-timer))
  (if (< (length input) 4)
      (list "" (format "%d chars more" (- 4 (length input))))
    (setq eh-org-query-collect-timer
          (run-with-timer
           0.25 nil
           `(lambda ()
              (let ((files (org-agenda-files))
                    (query
                     (eh-tree-map
                      (lambda (x)
                        `(,(car x) ,@(mapcar 'pyim-cregexp-build (cdr x))))
                      (org-ql--plain-query ,input))))
                (when query
                  (ignore-errors
                    (setq ivy--all-candidates
                          (or
                           (org-ql-select files query
                             :action (lambda ()
                                       (propertize (org-get-heading t)
                                                   'marker (copy-marker (point)))))
                           '("" "Search no results!")))
                    (setq ivy--old-cands ivy--all-candidates)
                    (ivy--exhibit)))))))
    nil))

(defun eh-org-query-goto (headline)
  (interactive)
  (let ((marker (get-text-property 0 'marker headline)))
    (when (markerp marker)
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker)
      (org-show-entry))))

(defun eh-org-query-insert-link (headline &optional link-type breadcrumbs)
  (interactive)
  (let ((marker (get-text-property 0 'marker headline))
        store-link)
    (when (markerp marker)
      (org-with-point-at marker
        (let* ((id (org-id-get (point) t))
               (attach-dir (org-attach-dir t))
               (breadcrumbs
                (when breadcrumbs
                  (let ((s (org-format-outline-path
                            (org-get-outline-path)
                            (1- (frame-width))
                            nil org-picklink-breadcrumbs-separator)))
                    (if (eq "" s) "" (concat s org-picklink-breadcrumbs-separator)))))
               (item (concat (or breadcrumbs "") (org-entry-get (point) "ITEM")))
               (link
                (cl-case link-type
                  (attach (list :link attach-dir :description (concat item "(ATTACH)") :type "file"))
                  (t (list :link (concat "id:" id) :description item :type "id")))))
          (setq store-link link)))
      (org-insert-link nil (plist-get store-link :link) (plist-get store-link :description))
      (cond ((org-in-item-p)
             (call-interactively #'org-insert-item))
            (t (insert " "))))))

;; ** org-brain
(require 'org-brain)
(setq org-brain-path (expand-file-name "brain" eh-org-local-directory))
(add-to-list 'org-agenda-files org-brain-path t)

(setq org-brain-include-file-entries nil)
(setq org-brain-file-entries-use-title nil)
(setq org-brain-headline-entry-name-format-string "%2$s")
(setq org-brain-file-from-input-function
      #'(lambda (x) (if (cdr x) (car x) "brain")))

(defun eh-org-brain-as-tags ()
  (mapcar
   (lambda (x)
     (list (replace-regexp-in-string
            "[^[:alnum:]_@#%]" "" (or (car x) ""))))
   (mapcan #'org-brain--file-targets
           (org-brain-files))))

(defun eh-org-brain-add-entry (title)
  (unless org-id-locations (org-id-locations-load))
  (let* ((targets (mapcan #'org-brain--file-targets
                          (org-brain-files)))
         (id (or (cdr (assoc title targets))
                 title)))
    (or
     ;; Headline entry exists, return it
     (org-brain-entry-from-id id)
     ;; File entry
     (progn
       (setq id (split-string id "::" t))
       (let* ((entry-path (org-brain-entry-path
                           (funcall org-brain-file-from-input-function id)
                           t))
              (entry-file (org-brain-path-entry-name entry-path)))
         (unless (file-exists-p entry-path)
           (make-directory (file-name-directory entry-path) t)
           (write-region "" nil entry-path))
         (if (or (not org-brain-include-file-entries)
                 (equal (length id) 2)
                 (not (equal (car id) entry-file)))
             ;; Create new headline entry in file
             (org-with-point-at (org-brain-entry-marker entry-file)
               (if (and (not org-brain-include-file-entries)
                        (re-search-forward (concat "\n\\* +" (car id)) nil t))
                   (org-brain-entry-at-pt)
                 (goto-char (point-max))
                 (insert (concat "\n* " (or (cadr id) (car id))))
                 (let ((new-id (org-id-get-create)))
                   (run-hooks 'org-brain-new-entry-hook)
                   (save-buffer)
                   (list entry-file (or (cadr id) (car id)) new-id))))
           entry-file))))))

;; ** org-board
(require 'org-board)
(setq org-board-wget-show-buffer nil)
(define-key org-mode-map (kbd "C-c o") org-board-keymap)

;; * Footer
(provide 'eh-org)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8-unix
;; End:

;;; eh-org.el ends here
