;;; package --- init.el file
;;; Commentary:
;;; Emacs config file loaded when starting Emacs

;;; Code:
;; list the packages you want
(setq package-list '(auctex flycheck highlight-indentation magit markdown-mode ivy swiper counsel))

;; =========Emacs Lisp Package Archive (ELPA) already comes with v24. These are other repositories=======================
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; =====================flycheck package=====================
;; for python, also install flake8 with "pip install flake8"
(add-hook 'after-init-hook #'global-flycheck-mode)
;;(setq flymake-python-pyflakes-executable "flake8")

;; =========Defines the column where line should be auto-wrapped (only for python)===========
;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	  (setq auto-fill-function 'do-auto-fill)
;; 	  (setq fill-column 79)))

;; =====================highlight current line=====================
;; (global-hl-line-mode t) ;; To enable
;; (set-face-background 'hl-line "light blue")

;; =====================highlight indentation package=====================
(add-hook 'prog-mode-hook 'highlight-indentation-mode)
;; (add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)

;; =================backup files================
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))

(setq make-backup-files t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      backup-by-copying t)

;; ============='y' and 'n' for 'yes' and 'no'==============
(defalias 'yes-or-no-p 'y-or-n-p)

;; ====================Display Column Number in Mode Line=================
(setq column-number-mode t)

;; ====================Display Time & Date Info in Modeline=================
(setq display-time-day-and-date t
display-time-24hr-format t)
(display-time)

;; ====================== Easier buffer killing======================
(global-set-key (kbd "M-k") 'kill-this-buffer)

;; ===========C-n adds new line if point is at end of buffer============
(setq next-line-add-newlines t)

;; ===========shortcut to jump to specific line========================
(global-set-key (kbd "M-g") 'goto-line)

;; ======================Enable mouse mode permanently====================== (for nadja)
;;(xterm-mouse-mode t)

;; ======================ivy, swiper, counsel packages======================
(ivy-mode 1)
;; show recently opened files in ivy switch buffer
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)

;; ==============auctex package===============
(setq TeX-save-query nil)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-one-master '"\"<none>\"") ;; disallow emacs to add random lines to the bottom of files.

;; ================british spell checker=============
;; install aspell and relevant dictionaries ("aspell-dict-en") with macports
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
(setq magit-completing-read-function 'ivy-completing-read)

;; ===============================ERC======================
;; for pw, create ~/.ercpass which contains something of the like: (setq freenode-nickone-pass "xxxxxxxx")
(if (file-readable-p "~/.ercpass")
    (progn
      (load "~/.ercpass")
      (setq erc-nickserv-passwords
	    `((freenode     (("molofishy" . ,freenode-nickone-pass)))))))

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

(setq custom-file "~/.emacs.d/custom.el")
(if (file-readable-p custom-file)
    (load custom-file))











