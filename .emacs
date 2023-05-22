;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; preliminary install stuff ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; melpa packages
(setq package-list '(
		     ;; generic emacs stuff
		     ido-vertical-mode
		     flycheck
		     ace-jump-mode
                     exec-path-from-shell

		     ;; language major modes
		     go-mode
		     js2-mode
		     rust-mode
		     web-mode

                     ;; visual stuff
                     monokai-theme

                     ;; IDE nonsense
                     company
                     lsp-mode
                     lsp-ui
                     yasnippet
                     dap-mode
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
(load-theme 'monokai t)

;; disable scroll bar and ugly menu bar
(menu-bar-mode -1)
(when (display-graphic-p)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))
;; enable line numbers
(global-display-line-numbers-mode)
;; display the time in the mode line
(display-time-mode 1)
;; display the current column number.
(setq column-number-mode t)
;;turn on parenthesis highlighting
(show-paren-mode 1)

;; add path var for OSX
(exec-path-from-shell-initialize)

;; emacs-plus no titlebar
(add-to-list 'default-frame-alist '(undecorated-round . t))

(defun image-type-available-p (type)
  "Return t if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
  (if (eq 'svg type)
      nil
    (and (fboundp 'init-image-library)
         (init-image-library type))))

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

(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

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

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

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

;;;;;;;;;;;;;;;;;;;
;;;; IDE stuff ;;;;
;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'global-company-mode)

(require 'dap-dlv-go)


;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)




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
   '(dap-mode yasnippet lsp-ui lsp-mode exec-path-from-shell company monokai-theme web-mode rust-mode flycheck ido-vertical-mode js2-mode go-mode ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
