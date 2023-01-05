;; ** 设置 emacs 包管理器
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 10)
        ("nongnu" . 9)
        ("melpa" . 8)))

(defun eh-package-install-selected-packages ()
  (interactive)
  (let ((package-selected-packages
         '(;; Packages required by emacs-helper
           adaptive-wrap aggressive-indent
           cal-china-x citre cnfonts
           company company-posframe
           consult eat ebdb
           ebdb-i18n-chn el2org emms
           flycheck geiser-guile guix
           magit marginalia markdown-mode
           modus-themes orderless
           org-contrib org-download org-ql
           org-super-agenda ox-gfm paredit popon
           pos-tip projectile pyim
           pyim-basedict rainbow-delimiters
           rainbow-mode switch-window
           tempel vertico vundo
           wgrep xmlgen xr)))
    (package-install-selected-packages :no-confirm)))

(unless package--initialized
  (package-initialize))

(eh-package-install-selected-packages)
