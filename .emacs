;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; preliminary install stuff ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; melpa packages
(setq package-list '(
		     ;; generic emacs stuff
		     ido-vertical-mode
		     flycheck
		     ace-jump-mode

		     ;; language major modes
		     go-mode
		     js2-mode
		     rust-mode
		     web-mode

                     ;; visual stuff
                     monokai-theme
		     ))

;; add melpa to package archive list and install missing packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; editor customization ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load theme
(load-theme 'monokai)

;; disable scroll bar and ugly menu bar
(menu-bar-mode -1)
(when (display-graphic-p)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))
;; enable line numbers
(global-linum-mode)
;; display the time in the mode line
(display-time-mode 1)
;; display the current column number.
(setq column-number-mode t)
;;turn on parenthesis highlighting
(show-paren-mode 1)

;; add highlighting for TODOs
(defun hilite-todos ()
  "Highlight TODOs and such."
  (highlight-lines-matching-regexp
   "\\<\\(FIXME\\|WRITEME\\|WRITEME!\\|TODO\\|BUG\\):?"
   'hi-pink-b)
  )
(add-hook 'c-mode-common-hook 'hilite-todos)
(add-hook 'js2-mode-hook 'hilite-todos)


;; move temporary files to temporary directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; general mode configs ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ido-vertical-mode config
(ido-mode 1)
(ido-vertical-mode 1)
(require 'ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)


;; org-mode config
(setq org-startup-with-inline-images t)


;;enable electric-pair-mode
(electric-pair-mode 1)
;;add in auto-completion of brackets in electric pair more
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?{ . ?\})
                            ))


(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; language mode configs;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C/C++
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; js & web
(setq-default indent-tabs-mode nil)
(setq-default js2-basic-offset 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(setq js2-skip-preprocessor-directives t) ;; ignore shebang

;;;;;;;;;;;;;;;;;;;;;;;
;;;; emacs keybinds;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; window moving
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(setq x-alt-keysym 'meta)

;; misc. one-off binds
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-x C-j") 'count-matches)
(global-set-key (kbd"C-x C-y") 'eshell)
(global-set-key (kbd "C-:") 'execute-extended-command)

;; use regex search and not literal match
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; use regex search and not literal replace
(global-unset-key (kbd "C-x s"))
(global-set-key (kbd "C-x s") 'replace-regexp)

;; insertion macros
(fset 'consolelog ;; insert console.log for javascript
      "console.log('")
(global-set-key (kbd "C-c C-l") 'consolelog)
(fset 'todo-mk ;; insert TODOs
      "// TODO(Ben): ")
(global-set-key (kbd "C-c C-r") 'todo-mk)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; generated stuff;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" default))
 '(package-selected-packages
   '(monokai-theme web-mode rust-mode flycheck ido-vertical-mode js2-mode go-mode ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
