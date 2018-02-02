;; Ben's emacs config file :^ )

;; list packages and install all in list
(setq package-list '(js2-mode))

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; move temporary files to temporary directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;ace-jump-mode & rust-mode
(add-to-list 'load-path "~/.emacs.d/elpa")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)



;;load themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("9568a88cf351728b05c983e7afa8ec3e08da00b16b25304feca2c044164bb3ce" "26ebf1964baf529f07ca121231bec4982a0d3e89f6897b19978edd91f8e1eddb" "8d8788b909824e7f491a600ad80b4f70d6571907879d7661651c93cf984fc947" "b33406bff03f0167fc1ce7da9f2bab6742db280e2cf083ec338ffd1176469ee6" "7ef2884658a1fed818a11854c232511fa25721c60083a2695e6ea34ce14777ee" "1bfb58637c50303c23d0f4ce1bb30a01ed722912aba5d1f0f13d7ac0e79a21fa" default)))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (pug-mode latex-math-preview go-mode powershell rust-mode exec-path-from-shell cmake-ide use-package slime company-irony-c-headers company-irony gdb-mi company clojure-mode rainbow-mode js2-mode)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
 
;;C indentation
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;;load theme
(add-to-list 'custom-theme-load-path' "~/emacspackage/themes")

;;disable menubar / scrollbar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;;load theme on startup
(load-theme 'hitagi)

;;enable electric-pair-mode
(electric-pair-mode 1)
;;add in auto-completion of brackets in electric pair more
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?{ . ?\})
                            ) )
;;turn on parenthesis highlighting
(show-paren-mode 1)

;;bind alt as meta key
(setq x-alt-keysym 'meta)

(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-x g") 'goto-line)

(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; window moving
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; collapsing code
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

;; company C stuff
;;(add-to-list 'company-backends 'company-c-headers)

;; line numbers
(global-linum-mode)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; setup irony mode
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; setup company mode
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              nil
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	company-backends                '((company-irony company-gtags)))
  :bind ("C-;" . company-complete-common))

(setq org-startup-with-inline-images t)

;; display the time in the mode line
(display-time-mode 1)
