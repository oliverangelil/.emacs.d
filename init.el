;; list the packages you want

(setq package-list '(auctex flycheck highlight-indentation magit))

;; =========Emacs Lisp Package Archive (ELPA) already comes with v24. These are other repositories=======================
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; =====================flycheck mode=====================
;; install with M-x package-install flycheck
;; for python, also install flake8 (pip install flake8)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; =====================highlight current line=====================
;; (global-hl-line-mode t) ;; To enable
;; (set-face-background 'hl-line "light blue")

;; =====================highlight indentation mode=====================
(add-hook 'prog-mode-hook 'highlight-indentation-mode) ;install with M-x package-list-packages (find package by antonj)
;; (add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)

;; =================disable backup files================
(setq make-backup-files nil)

;; ============='y' and 'n' for 'yes' and 'no'==============
(defalias 'yes-or-no-p 'y-or-n-p)

;; ====================Display Column Number in Mode Line=================
(setq column-number-mode t)

;; ====================Display Time & Date Info in Modeline=================
(setq display-time-day-and-date t
     display-time-24hr-format t)
(display-time)

;; =========Defines the column where line should be auto-wrapped===========
;; (setq-default auto-fill-function 'do-auto-fill)
;; (set-default 'fill-column 79)

;; ==============================================Key Bindings==================================================
;; ============================================================================================================
;; map the window manipulation keys to meta 0, 1, 2, o
(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap

;; To help Unlearn C-x 0, 1, 2, o
(global-unset-key (kbd "C-x 3")) ;( was split-window-horizontally)
(global-unset-key (kbd "C-x 2")) ; was split-window-vertically
(global-unset-key (kbd "C-x 1")) ; was delete-other-windows
(global-unset-key (kbd "C-x 0")) ; was delete-window
(global-unset-key (kbd "C-x o")) ; was other-window

;; ====================== Easier buffer killing======================
(global-set-key (kbd "M-k") 'kill-this-buffer)

;; ===========C-n adds new line if point is at end of buffer============
(setq next-line-add-newlines t)

;; ===========shortcut to jump to specific line========================
(global-set-key (kbd "M-g") 'goto-line)

;; ======================Enable mouse mode permanently====================== (for nadja)
;;(xterm-mouse-mode t)

;; ======================Enable IDO mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; ==============latex (auctex) stuff: autosave before compiling=========
(setq TeX-save-query nil)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; ================spelling======install aspell and relevant dictionaries ("aspell-dict-en") with macports
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(setq ispell-dictionary "en_GB")
(setq-default ispell-program-name "aspell")

;; ============================recent files======================
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 30)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; ===============================magit package.
(global-set-key (kbd "C-x g") 'magit-status)

;; ===============================ERC======================
;; for pw, create ~/.ercpass which contains something of the like: (setq freenode-nickone-pass "xxxxxxxx")
(if (file-readable-p "~/.ercpass") (load "~/.ercpass"))

(setq erc-nickserv-passwords
                `((freenode     (("molofishy" . ,freenode-nickone-pass)))))

(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)


;; This causes ERC to connect to the Freenode network upon hitting C-c e f
(global-set-key "\C-cef" (lambda () (interactive)
			   (erc :server "irc.freenode.net" :port "6667"
				:nick "molofishy")))

;; ignore notices
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Rename server buffers to reflect the current network name instead
;; of SERVER:PORT. (e.g. "freenode" instead of "irc.freenode.net:6667"). This
;; is useful when using a bouncer like ZNC where you have multiple
;; connections to the same server.
(setq erc-rename-buffers t)

;; ======================learn to use emacs properly======================
;; (add-to-list 'load-path "~/.emacs.d/elisp/")
;; (require 'no-easy-keys)
;; (no-easy-keys 1)

;; =======================CONVERT .PUG INTO .HTML WHEN SAVING (install node package manager (npm) to install pug)======================
;; (defun compile-pug ()
;;   (when (eq major-mode 'pug-mode)
;;     (shell-command (format "pug -P %s" buffer-file-name))))
;; (add-hook 'after-save-hook 'compile-pug)

;; =======================CONVERT .SASS INTO .CSS WHEN SAVING (sass was installed with gem) ======================
;; (require 'sass-mode)
;; (defun compile-sass ()
;;   (when (eq major-mode 'sass-mode)
;;     (shell-command (format "sass -t expanded --sourcemap=none %s %s.css" buffer-file-name (file-name-sans-extension buffer-file-name)))))
;; (add-hook 'after-save-hook 'compile-sass)

;======================WEB-MODE======================
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode)) ; here, web-mode is used for files ending with tpl.php
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;; (setq web-mode-enable-css-colorization t)













