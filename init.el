;;;
;;; Jacob Emacs
;;;

;; Bootstrap Straight.el
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Package setup -----
(let ((nixdir (file-name-as-directory (getenv "NIXCONFIG_DIR")))) 
  (setq straight-use-package-by-default t
		straight-current-profile 'base
		straight-profiles
		`((base . ,(file-name-concat nixdir "straight.lockfile.default.el"))
		  (anduril . ,(file-name-concat nixdir "straight.lockfile.anduril.el")))))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; Visuals ---------
(tool-bar-mode -1)
(menu-bar-mode -1)

(use-package modus-themes
  :config (setq modus-themes-bold-constructs t))

(use-package doom-themes
  :config (setq doom-themes-enable-bold t))

(load-theme 'doom-dark+ t)

(use-package diminish)

;;; Basics -----------
(setq use-short-answers t)                      ;; Use "y" and "n" rather than "yes" and "no"
(setq scroll-conservatively 101)                ;; Scroll screen with cursor
(setq-default display-line-numbers-width 3)     ;; Wider line numbers
(setq-default truncate-lines t)                 ;; Don't wrap text by default
(setq-default tab-width 4)                      ;; Tab width
(setq visible-bell nil)                         ;; Make it ring (so no visible bell) (default)
(setq ring-bell-function 'ignore)               ;; BUT ignore it, so we see and hear nothing
(setq help-window-select t)                     ;; Automatically focus help window
(setq gc-cons-threshold 100000000)              ;; Increase GC threshold
(setq read-process-output-max (* 1024 1024))    ;; 1mb
(setq create-lockfiles nil                      ;; Don't generate lockfiles.
      auto-save-list-file-prefix nil
      make-backup-files nil)                    ;; Don't generate backups.
(setq initial-scratch-message ""                ;; Blank scratch buffer
      initial-major-mode 'text-mode             ;; Make default mode `text-mode`
      initial-buffer-choice t)                  ;; Show a scratch buffer rather than startup screen
(setq recentf-max-saved-items 200)              ;; Save up to 200 recent items


(recentf-mode 1)                                ;; Track recent files
(xterm-mouse-mode t)                            ;; Enable mouse in terminal
(show-paren-mode)                               ;; Highlight matching parenthesis
(line-number-mode)                              ;; Show line number in modeline
(column-number-mode)                            ;; Show column number in modeline
                                                ;;(global-hl-line-mode)
(global-display-line-numbers-mode t)            ;; Display line numbers on the side
(blink-cursor-mode 0)                           ;; Stop cursor from blinking
(fringe-mode '(8 . 8))                          ;; Window margin

(setq-default mode-line-end-spaces nil)         ;; Don't pad modeline with dashes (ugly)

(use-package centered-cursor-mode :diminish centered-cursor-mode)

;; Auto-save
(use-package super-save
  :diminish super-save-mode
  :config
  (super-save-mode)
  (setq super-save-auto-save-when-idle t))

;; Persist history over Emacs restarts
(use-package savehist :init (savehist-mode))

(use-package golden-ratio
  :diminish golden-ratio-mode
  :config (golden-ratio-mode))

;;; Completions -----
(use-package vertico :init (vertico-mode))

(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package orderless :config (setq completion-styles '(orderless basic)))

(use-package consult
  :config
  ;; Sort consult buffer by recently used
  (consult-customize consult-buffer :sort t)) 

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (setq corfu-auto t
        corfu-count 8
        corfu-auto-prefix 2))

(use-package corfu-terminal :config (corfu-terminal-mode))

;;; Programming Modes ------

(let
	((straight-current-profile 'anduril)
	 (f (expand-file-name "programming.el" user-emacs-directory)))
  (when (file-exists-p f)
	(load (expand-file-name "programming.el" user-emacs-directory))))

;;; Org Mode -----------
(use-package org
  :ensure nil
  :diminish org-indent-mode
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-startup-indented t
        org-hide-emphasis-markers t))

(use-package org-roam
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :diminish (org-roam-ui-mode " ORU")
  :config
  (setq org-roam-ui-follow nil))

;;; Keybindings ------

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package evil
  :init
  (setq evil-want-fine-undo t
        evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'dired-mode 'emacs)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line))

(use-package general)

;; evil-delete-buffer also closes the window which is my preferred behavior
;; Maps C-w C-q to evil-delete-buffer (The first C-w puts you into evil-window-map)
(define-key evil-window-map "\C-q" 'evil-delete-buffer) 

(define-key help-mode-map "q" 'kill-current-buffer) 

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make Esc not a prefix and just quit
(global-unset-key (kbd "C-x C-z")) ;; Don't let C-z suspend terminal Emacs

;; Leader key maps
(general-define-key
  :states '(normal motion visual)
  :prefix "SPC"
  :keymaps 'override
  "SPC" 'find-file
  "/" 'consult-ripgrep
  "r" 'consult-recent-file
  "x" 'execute-extended-command
  "fs" 'save-buffer
  "fd" 'consult-fd
  "bd" 'evil-delete-buffer
  "bb" 'consult-buffer
  "bs" '(lambda () (interactive) (switch-to-buffer "*scratch*"))
  "wd" 'evil-window-delete
  "<right>" 'next-buffer
  "<left>" 'previous-buffer)

;; Insert mode keymaps
(general-def
  :states '(insert)
  "C-a" 'evil-beginning-of-visual-line
  "C-e" 'evil-end-of-visual-line
  "C-n" 'evil-next-visual-line
  "C-p" 'evil-previous-visual-line)

;; All mode maps
(general-def
  "M-s" 'other-window
  "M-b" 'consult-buffer
  "C-s" 'consult-line)

;; Non-insert mode keymaps
(general-def
  :states '(normal visual motion)
  "gc" 'comment-dwim
  "gC" 'comment-line

  "/" '(lambda () (interactive) (split-window-horizontally) (other-window 1))
  "-" '(lambda () (interactive) (split-window-vertically) (other-window 1))
  "s" 'consult-line)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(consult-buffer-filter
   '("\\` " "\\`\\*Completions\\*\\'" "\\`\\*Multiple Choice Help\\*\\'"
     "\\`\\*Flymake log\\*\\'" "\\`\\*Semantic SymRef\\*\\'"
     "\\`\\*vc\\*\\'" "\\`newsrc-dribble\\'" "\\`\\*tramp/.*\\*\\'"
     "\\`\\*Async-native-compile-log\\*\\'" "\\`\\*lsp-\.*\\'"))
 '(custom-safe-themes
   '("7771c8496c10162220af0ca7b7e61459cb42d18c35ce272a63461c0fc1336015"
     "4d5d11bfef87416d85673947e3ca3d3d5d985ad57b02a7bb2e32beaf785a100e"
     "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4"
     "77f281064ea1c8b14938866e21c4e51e4168e05db98863bd7430f1352cab294a"
     "5e39e95c703e17a743fb05a132d727aa1d69d9d2c9cde9353f5350e545c793d4"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
