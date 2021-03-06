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
;; 鼠标点击链接时，不打开链接，这样设置适合在 termux 中使用 org-agenda
(when (eh-termux-p)
  (setq org-mouse-1-follows-link nil))

(require 'org)

(define-key org-mode-map (kbd "<f1>") 'eh-org-attach-reveal)
(define-key org-mode-map (kbd "<f2>") 'eh-org-open-syncthing-dir)
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

(defun eh-org-tags-update-all ()
  (interactive)
  (when (yes-or-no-p "确定依照 org-brain 来更新所有 headlines 的 tag 吗? ")
    (org-map-entries
     (lambda ()
       (let ((org-use-tag-inheritance nil)
             (targets (mapcar #'car (org-brain--all-targets)))
             tags)
         (dolist (tag (org-get-tags))
           (when (member tag targets) ;测试 org-brain 数据库内是否有这个 tag
             (push tag tags)
             (push (nth 1 (org-brain-get-entry-from-title tag)) tags)))
         (org-set-tags (delete-dups (reverse tags)))))
     nil 'agenda)
    (org-save-all-org-buffers)
    (message "Tags 更新完成，最好使用 git diff 对比一下更新前后的内容。")))

(defun eh-org-set-tags-command (&optional _arg)
  (interactive)
  (let ((org-current-tag-alist
         (org--tag-add-to-alist
          (org--tag-add-to-alist org-current-tag-alist
                                 (org-get-buffer-tags))
          (eh-org-brain-as-tags))))
    (funcall-interactively #'counsel-org-tag)))

(advice-add 'org-set-tags-command :override #'eh-org-set-tags-command)

(defun eh-counsel-org-tag-action (orig_func tag)
  (funcall orig_func tag)
  (let ((tag1))
    (when (functionp 'org-brain-get-entry-from-title)
      ;; 主要处理 org-brain Nickname
      (setq tag1 (nth 1 (org-brain-get-entry-from-title tag)))
      (message ""))
    (when (memq this-command '(ivy-done
                               ivy-alt-done
                               ivy-immediate-done))
      (setq counsel-org-tags
            (delete-dups
             `(,tag ,tag1 ,@counsel-org-tags)))
      (counsel-org--set-tags)
      (message ""))))

(advice-add 'counsel-org-tag-action :around #'eh-counsel-org-tag-action)

(setq org-insert-heading-respect-content nil)
(setq org-log-done t)
(setq org-startup-indented nil)
(setq org-edit-src-content-indentation 0)
(setq org-id-link-to-org-use-id t)
(setq org-log-into-drawer t)

;; org 文件显示内嵌图片的时候，首先缩放一下。
(setq org-image-actual-width t)

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

(defun eh-org-smart-truncate-lines (&optional _arg)
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
;; (require 'org-collector)

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

;;; ** export html
(setq org-html-coding-system 'utf-8)
(setq org-html-head-include-default-style t)
(setq org-html-head-include-scripts t)

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

;; *** export latex
;; 不要在latex输出文件中插入\maketitle
(setq org-latex-title-command "")

(setq org-latex-date-format "%Y-%m-%d")
;; (setq org-latex-create-formula-image-program 'imagemagick)  ;默认支持中文
(setq org-preview-latex-default-process 'dvipng)          ;速度较快，但默认不支持中文
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.5))
(setq org-format-latex-options(plist-put org-format-latex-options :html-scale 2.5))

(require 'org2ctex)
(org2ctex-mode 1)

;; ** org-plus-contrib
;; (require 'ox-extra)
;; ;; 如果一个标题包含TAG: “ignore” ,导出latex时直接忽略这个标题，
;; ;; 但对它的内容没有影响。
;; (ox-extras-activate '(latex-header-blocks ignore-headlines))

;; ** org-bable设置
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)

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

;; *** org babel plantuml module
(require 'ob-plantuml)
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

;; *** org babel other modules
(require 'ob-org)
(require 'ob-emacs-lisp)
(require 'ob-mscgen)
(require 'ob-python)

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

(defvar eh-org-syncthing-dir "~/01-syncthing")

(defun eh-org-open-syncthing-dir ()
  (interactive)
  (let ((dir (file-name-as-directory
              (expand-file-name eh-org-syncthing-dir))))
    (when (file-directory-p dir)
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
          (dolist (file (directory-files dir t "000-.*"))
            (when (file-symlink-p file)
              (delete-file file)))
          (let* ((attach-dir (file-name-as-directory (org-attach-dir t)))
                 (name (format "000-%s"
                               (replace-regexp-in-string
                                "[[:space:]]" ""
                                (replace-regexp-in-string
                                 file-name-invalid-regexp ""
                                 (org-get-heading t t t t)))))
                 (file-link (concat (file-name-as-directory dir) name)))
            (shell-command (format "ln -s %s %s" attach-dir file-link)))
          (eh-system-open dir))))))

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
(define-key org-agenda-mode-map (kbd "<f2>") 'eh-org-open-syncthing-dir)
(define-key org-agenda-mode-map (kbd "<f3>") 'share2computer-org)
(define-key org-agenda-mode-map (kbd "SPC") 'eh-org-agenda-show-and-scroll-up)
(define-key org-agenda-mode-map (kbd "<return>") 'eh-org-agenda-show-and-scroll-up)
(define-key org-agenda-mode-map (kbd "g") 'eh-org-agenda-redo-all)
(define-key org-agenda-mode-map (kbd "i") 'eh-org-agenda-insert-heading)
(define-key org-agenda-mode-map (kbd "A") 'org-agenda-archive-default-with-confirmation)
(define-key org-agenda-mode-map (kbd "h") 'ignore)
(define-key org-agenda-mode-map (kbd "y") 'ignore)
(define-key org-agenda-mode-map (kbd "a") 'ignore)

;; 取消下面关于 archive 的快捷键，容易误操作
(org-defkey org-agenda-mode-map "\C-c\C-x\C-a" 'ignore)
(org-defkey org-agenda-mode-map "\C-c\C-xa"    'ignore)
(org-defkey org-agenda-mode-map "\C-c\C-xA"    'ignore)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-s" 'ignore)
(org-defkey org-agenda-mode-map "\C-c$"        'ignore)
(org-defkey org-agenda-mode-map "$"            'ignore)


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
      (setq org-agenda-show-window (selected-window)))
    (select-window win)))

(defvar eh-org-popedit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'eh-org-popedit-finalize)
    (define-key map "\C-c\C-k" #'eh-org-popedit-abort)
    map))

(defvar eh-org-popedit-info nil)
(defvar eh-org-popedit-erase-input-when-abort nil)
(defvar eh-org-popedit-erase-input-success nil)

(define-minor-mode eh-org-popedit-mode
  "eh-org-popedit-mode"
  nil " OPE" eh-org-popedit-mode-map
  (setq eh-org-popedit-erase-input-success nil)
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<eh-org-popedit-mode-map>Org Entry.  Finish \
`\\[eh-org-popedit-finalize]', abort `\\[eh-org-popedit-abort]'.")))

(defun eh-org-popedit-finalize ()
  (interactive)
  (save-buffer)
  (kill-buffer-and-window)
  (eh-org-popedit-update))

(defun eh-org-popedit-abort ()
  (interactive)
  (when (and eh-org-popedit-erase-input-when-abort
             ;; 测试 buffer 是否 narrowed, 防止不小心
             ;; 把整个文件都清空。
             (buffer-narrowed-p))
    (delete-region (point-min) (point-max))
    (setq eh-org-popedit-erase-input-success t))
  (kill-buffer-and-window)
  (eh-org-popedit-update))

(defun eh-org-popedit-goto-id (id)
  (let ((point (point))
        id-found-p)
    (when (equal major-mode 'org-agenda-mode)
      (goto-char (point-min))
      (while (and (not (= (line-end-position) (point-max)))
                  (let* ((marker (or (get-text-property (point) 'org-hd-marker)
		                     (get-text-property (point) 'org-marker)))
                         (id1 (save-excursion
                                (when marker
	                          (set-buffer (marker-buffer marker))
	                          (goto-char marker)
                                  (org-id-get)))))
                    (when (equal id id1)
                      (setq id-found-p t))
                    (or (not id1)
                        (not (equal id id1)))))
        (forward-line 1))
      (unless id-found-p
        (goto-char point)
        (message "注意: 新插入的 headling 在当前视图中没有显示。")))))

(defun eh-org-popedit-update ()
  (ignore-errors
    (let ((mode (plist-get eh-org-popedit-info :major-mode))
          (buffer (plist-get eh-org-popedit-info :buffer))
          (id (plist-get eh-org-popedit-info :id)))
      (cond ((equal mode 'org-agenda-mode)
             (with-current-buffer buffer
               (eh-org-agenda-redo-all)
               (unless eh-org-popedit-erase-input-success
                 (eh-org-popedit-goto-id id))))
            ((equal mode 'org-brain-visualize-mode)
             (with-current-buffer buffer
               (org-brain--revert-if-visualizing))))
      (setq eh-org-popedit-erase-input-when-abort nil))))

(defun eh-org-agenda-insert-heading ()
  (interactive)
  (when (y-or-n-p "确定在当前标题下面插入一个新标题么? ")
    (let ((win (selected-window))
          (org-capture-templates
           '(("t1" "" plain (file "/temp/null.org")
              "%?
:PROPERTIES:
:created: %U
:END:
"
              :immediate-finish t
              :no-save t)
             ("t2" "" plain (file "/temp/null.org")
              "TODO %?
SCHEDULED: %t
:PROPERTIES:
:created: %U
:END:
"
              :immediate-finish t
              :no-save t)))
          (key (cond ((equal (car org-agenda-redo-command) 'org-agenda-list)
                      "t2")
                     (t "t1")))
          tags)
      (setq eh-org-popedit-info
            (list :major-mode major-mode
                  :buffer (current-buffer)))
      (setq eh-org-popedit-erase-input-when-abort t)
      (org-agenda-goto t)
      ;; 获取上一个 headline 在 headline 上面直接打的 tag.
      (setq tags (org-get-tags nil t))
      (org-insert-heading-respect-content)
      (save-excursion
        (insert
         (with-temp-buffer
           ;; org-capture 报找不到文件的错误，不影响使用，
           ;; 使用特殊的方式让其不显示报错。
           (cl-letf (((symbol-function 'message) #'ignore))
             (ignore-errors
               (org-capture 0 key)))
           (buffer-string))))
      (goto-char (line-end-position))
      ;; 把上一个 Headline 的 tag 打到新插入的 headline,
      ;; 这样可以确保新插入的 headline 出现在当前视图中，
      ;; 不过有可能打上不合适的 TAG.
      (when tags
        (org-set-tags
         (cl-remove-if (lambda (x)
                         (member x '("ATTACH" "proj" "ref")))
                       tags))
        ;; 一打 tag, 星号后面的空格就没有了。
        (insert " "))
      (plist-put eh-org-popedit-info :id (org-id-get-create))
      (let ((org-indirect-buffer-display 'current-window))
        (org-tree-to-indirect-buffer)
        ;; 隐藏 indirect buffer
        (rename-buffer (concat " " (buffer-name))))
      (org-with-wide-buffer
       (narrow-to-region (org-entry-beginning-position)
                         (org-entry-end-position))
       (org-show-all '(drawers))
       (indent-region (point-min) (point-max)))
      (eh-org-popedit-mode 1))))

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
(add-to-list 'org-agenda-files (concat (file-name-as-directory eh-org-local-directory) "orgzly") t)

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

(defvar eh-org-agenda-query-string-before-matcher nil)

(defun eh-org-make-tags-matcher (orig_fun match)
  (let* ((result (funcall orig_fun match))
         (match (or match (car result))))
    (if (and match
             (stringp match)
             (string-match-p "[|&+-]" match))
        (progn
          (setq eh-org-agenda-query-string-before-matcher match)
          result)
      (let* ((entry (org-brain-get-entry-from-title match))
             (tag (propertize (nth 1 entry) 'id (nth 2 entry)))
             (parent-tags (eh-org-brain-get-all-parent-tags tag)))
        (setq eh-org-agenda-query-string-before-matcher tag)
        (funcall orig_fun (mapconcat #'identity `(,tag ,@parent-tags) "|"))))))

(advice-add 'org-make-tags-matcher :around #'eh-org-make-tags-matcher)

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
                         'face '(:foreground "green" :weight bold)))))

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

;; 这个函数编译的时候会有警告，因为启用词法作用域的缘故，但不影响使用，
;; 原因是这个函数最终会被 org-eval 包装，强制使用动态作用域。
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
         ;; (monthname (calendar-month-name month))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         ;; (weekyear (cond ((and (= month 1) (>= iso-week 52))
         ;;                  (1- year))
         ;;                 ((and (= month 12) (<= iso-week 1))
         ;;                  (1+ year))
         ;;                 (t year)))
         (cn-date (calendar-chinese-from-absolute
                   (calendar-absolute-from-gregorian date)))
         ;; (cn-year (cadr cn-date))
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

(global-set-key (kbd "C-c c") 'counsel-org-capture)

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
(define-key org-mode-map (kbd "<f4>") 'org-download-screenshot)
(setq org-download-method 'attach)
(setq org-download-display-inline-images 'posframe)
(setq org-download-screenshot-file (concat temporary-file-directory "image.png"))
(setq org-download-image-attr-list
      '("#+ATTR_HTML: :width 80% :align center"))
(when (eq system-type 'windows-nt)
  (setq org-download-screenshot-method "convert clipboard: %s"))
(org-download-enable)

;; ** org-brain
(require 'org-brain)
(setq org-brain-path (expand-file-name "brain" eh-org-local-directory))
(add-to-list 'org-agenda-files org-brain-path t)

(set-face-attribute 'org-brain-selected-face-template nil
                    :box (list :line-width 1))

(setq org-brain-include-file-entries nil)
(setq org-brain-file-entries-use-title nil)
(setq org-brain-headline-entry-name-format-string "%2$s")
(setq org-brain-default-file-parent "brain")
;; obsolete, delete future
(setq org-brain-file-from-input-function
      #'(lambda (x) (if (cdr x) (car x) "brain")))

(define-key org-brain-visualize-mode-map "\C-c\C-c" 'eh-org-brain-goto-current)

(defun eh-org-brain-goto-current (&optional same-window)
  (interactive "P")
  (setq eh-org-popedit-info
        (list :major-mode major-mode
              :buffer (current-buffer)))
  (org-brain-goto-current)
  (let ((org-indirect-buffer-display 'current-window))
    (org-tree-to-indirect-buffer)
    (eh-org-popedit-mode 1)
    ;; Hide indirect buffer
    (rename-buffer (concat " " (buffer-name)))
    (goto-char (point-max))))

(defun eh-org-brain-as-tags ()
  (mapcar
   (lambda (x)
     (list
      (propertize
       (replace-regexp-in-string
        "[^[:alnum:]_@#%]" "" (or (car x) ""))
       'id (cdr x))))
   (org-brain--all-targets)))

(defun eh-org-brain-get-parent-tags (tag)
  (let ((parents (org-brain-parents
                  (org-brain-entry-from-id
                   (get-text-property 0 'id tag)))))
    (mapcar (lambda (x)
              (propertize
               (nth 1 x)
               'id (nth 2 x)))
            parents)))

(defun eh-org-brain-get-all-parent-tags (tag)
  (let ((parents (eh-org-brain-get-parent-tags tag))
        p all)
    (while parents
      (setq p (pop parents))
      (setq parents `(,@parents ,@(eh-org-brain-get-parent-tags p)))
      (push p all))
    (delete-dups all)))

;; (eh-org-brain-get-all-parent-tags
;;  #("网络安全" 0 4 (id "11e8046f-a5af-43f3-985b-9daa866fed54")))

(defun eh-org-brain-set-title (&optional entry new-title)
  (interactive)
  (let* ((entry (or entry (org-brain-entry-at-pt t)))
         (title (org-brain-title entry))
         (new-title (or new-title (read-string "Title: " title))))
    (when (equal (length new-title) 0)
      (error "Title must be at least 1 character"))
    (org-brain-set-title entry new-title)
    (when (y-or-n-p "Set original title as a nickname of entry? ")
      (org-brain-add-nickname entry title))
    (org-brain--revert-if-visualizing)))

(define-key org-brain-visualize-mode-map "t" 'eh-org-brain-set-title)

(defun eh-org-brain-merge (entry1 entry2)
  "Merge ENTRY2 to ENTRY1, and set ENTRY2's title as a nickname of ENTRY1."
  (let ((title2 (org-brain-title entry2))
        (entry1-id (nth 2 entry1))
        (entry2-id (nth 2 entry2))
        (entry2-parent (org-brain-parents entry2))
        (entry2-children (org-brain-children entry2))
        (entry2-friends (org-brain-friends entry2))
        (entry1-tags (org-brain-get-tags entry1))
        (entry2-tags (org-brain-get-tags entry2)))
    ;; Merge tags
    (when entry2-tags
      (org-with-point-at (org-brain-entry-marker entry1)
        (org-set-tags (delete-dups (remove nil `(,@entry1-tags ,@entry2-tags))))))
    ;; Merge parent, children and friends1
    (org-brain-add-parent entry1 entry2-parent)
    (org-brain-add-child entry1 entry2-children)
    (org-brain-add-friendship entry1 entry2-friends)
    ;; Merge org brain resources
    (dolist (link (org-brain-resources entry2))
      (org-brain-add-resource
       (car link)
       (format "%s: %s" title2 (cdr link))
       nil entry1))
    ;; Merge org brain text
    (let ((text (org-brain-text entry2)))
      (when (> (length (replace-regexp-in-string "[[:space:]\n]+" "" text)) 0)
        (org-with-point-at (org-brain-entry-marker entry1)
          (save-excursion
            (org-back-to-heading t)
            (org-end-of-subtree t t)
            (when (org-at-heading-p) (backward-char 1))
            (insert text)))))
    ;; Merge attachment
    (let ((attach (org-with-point-at (org-brain-entry-marker entry2)
                    (org-attach-dir))))
      (when attach
        (org-brain-add-resource
         (format "file:%s" (file-name-as-directory attach))
         (format "%s: /" title2)
         nil entry1)))
    ;; Merge edge
    (dolist (e `(,@entry2-parent ,@entry2-children ,@entry2-friends))
      (let* ((id (nth 2 e))
             (entry2-edge
              (org-with-point-at (org-brain-entry-marker entry2)
                (org-entry-get
                 (org-brain-entry-marker entry2)
                 (format "BRAIN_EDGE_%s" id)))))
        (when entry2-edge
          (let ((m (org-brain-entry-marker entry1)))
            (org-with-point-at m
              (org-entry-put m (format "BRAIN_EDGE_%s" id) entry2-edge)))
          (let ((m (org-brain-entry-marker e)))
            (org-with-point-at m
              (org-entry-put
               m (format "BRAIN_EDGE_%s" entry1-id)
               (org-entry-get m (format "BRAIN_EDGE_%s" entry2-id))))))))
    (org-brain-delete-entry entry2 t)
    (org-brain-add-nickname entry1 title2)
    (org-brain--revert-if-visualizing)))

(defun eh-org-brain-add-nickname (entry nickname)
  (interactive (list (org-brain-entry-at-pt)
                     (read-string "Nickname: ")))
  (let* ((targets (org-brain--all-targets))
         (nickname-entry
          (org-brain-entry-from-id
           (cdr (assoc nickname targets)))))
    (if (not nickname-entry)
        (org-brain-add-nickname entry nickname)
      (when (yes-or-no-p
             (format "Entry '%s' already exists, merge it to '%s' then delete? "
                     nickname (nth 1 entry)))
        (eh-org-brain-merge entry nickname-entry)))))

(define-key org-brain-visualize-mode-map "N" 'eh-org-brain-add-nickname)

(defun eh-org-brain-remove-nickname (entry nickname)
  (interactive
   (let ((e (org-brain-entry-at-pt)))
     (list e (completing-read
              "Nickname: "
              (if (org-brain-filep e)
                  (ignore-errors
                    (cdr (assoc "NICKNAMES" (org-brain-keywords e))))
                (org-entry-get-multivalued-property
                 (org-brain-entry-marker e)
                 "NICKNAMES"))))))
  (if (org-brain-filep entry)
      (let ((nickname (org-entry-protect-space nickname)))
        (org-with-point-at (org-brain-entry-marker entry)
          (goto-char (point-min))
          (re-search-forward "^#\\+NICKNAMES: +$" nil t)
          (let* ((begin (point))
                 (end (line-end-position))
                 (s (buffer-substring-no-properties begin end)))
            (delete-region begin end)
            (insert (mapconcat #'identity (remove nickname (split-string s " ")) " "))
            (save-buffer))))
    (org-entry-remove-from-multivalued-property
     (org-brain-entry-marker entry) "NICKNAMES" nickname)
    (org-save-all-org-buffers))
  (org-brain--revert-if-visualizing))

(defun eh-org-brain-entry-nicknames (entry)
  (let ((lst (if (org-brain-filep entry)
                 (ignore-errors
                   (cdr (assoc "NICKNAMES" (org-brain-keywords entry))))
               (org-entry-get-multivalued-property
                (org-brain-entry-marker entry)
                "NICKNAMES"))))
    (when lst
      (format "(%s)" (mapconcat #'identity lst "|")))))

(add-to-list 'org-brain-vis-current-title-append-functions
             'eh-org-brain-entry-nicknames)

;; * Footer
(provide 'eh-org)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org.el ends here
