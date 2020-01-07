;; Ben's emacs config file :^ )
;; I use emacs compiled for XWindow / Cocoa because its marginally faster (usually)
;; and has nicer colors with a lot less customization.

;; list packages and install all in list
(defvar package-list '(js2-mode))

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
(require 'rust-mode)
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
 '(js-indent-level 2)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (yaml-mode deft rainbow-delimiters json-mode flycheck ido-vertical-mode neotree typescript-mode lua-mode web-mode pug-mode latex-math-preview go-mode powershell rust-mode js2-mode)))
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

;;js indentation
(setq-default indent-tabs-mode nil)
(setq-default js2-basic-offset 2)

;;load theme
(add-to-list 'custom-theme-load-path' "~/emacspackage/themes")

;;disable menubar / scrollbar
(menu-bar-mode -1)
(when (display-graphic-p)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))


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
(global-set-key (kbd "C-x C-j") 'count-matches)
(global-set-key (kbd "C-q") (lambda()
                              (interactive)(neotree)))
(setq neo-autorefresh nil)

(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; window moving
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; company C stuff
;;(add-to-list 'company-backends 'company-c-headers)

;; line numbers
(global-linum-mode)

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)


(setq org-startup-with-inline-images t)

;; display the time in the mode line
(display-time-mode 1)

;; display the current column number.
(setq column-number-mode t)

;; load web-mode for all html files for javascript support.
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;; use C++ mode for all header files, C headers don't use any extra features.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; use js2-mode for all javascript files (IDE mode).
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; org-mode setup
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/workspace/todo.org"))
;; add path var for OSX
(exec-path-from-shell-initialize)


;; ESHELL SETTINGS
(global-set-key (kbd"C-x C-y") 'eshell)


(global-unset-key (kbd"C-x C-u"))
(global-unset-key (kbd"C-x C-l"))

;; add highlighting for TODO/WRITEME/WRITEME!/TODO/BUG
(defun hilite-todos ()
  "Highlight TODOs and such."
  (highlight-lines-matching-regexp "\\<\\(FIXME\\|WRITEME\\|WRITEME!\\|TODO\\|BUG\\):?"
       'hi-pink-b)
)

(add-hook 'c-mode-common-hook 'hilite-todos)
(add-hook 'js2-mode-hook 'hilite-todos)

;; ido-vertical mode
(ido-mode 1)
(ido-vertical-mode 1)
(require 'ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(defun revert-buffer-no-confirm ()
  "Revert a buffer without confirmation (taken from SO)."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))
(global-set-key (kbd "C-x C-l") 'revert-buffer-no-confirm)

;; flycheck setup
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-disabled-checkers '(javascript-jshint))
(setq flycheck-checkers '(javascript-eslint
                          json-jsonlint))
;; Disable jshint, since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
;; Disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))
(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook #'add-node-modules-path))
;; Use eslint with these modes
(flycheck-add-mode 'javascript-eslint 'js-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
;; Use jsonlint with these modes
(flycheck-add-mode 'json-jsonlint 'json-mode)

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; gyp
(setq load-path (cons "~/.emacs.d/elpa/gyp/tools/emacs" load-path))
(require 'gyp)

(add-hook 'emacs-lisp-mode #'rainbow-delimiters-mode)

;; copy to killring.
(global-set-key (kbd "C-c C-q") 'kill-ring-save)
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'isearch-forward-regexp)

;; deft config
(global-unset-key (kbd "C-x C-d"))
(global-set-key (kbd "C-x C-d") 'deft)
(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/notes")
(setq deft-recursive t)

;; indium
(unless (package-installed-p 'indium)
  (package-install 'indium))

;; setup flypsell for text-mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))



;; deft config
(global-unset-key (kbd "C-x C-d"))
(global-set-key (kbd "C-x C-d") 'deft)
(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/notes")
(setq deft-recursive t)

(fset 'consolelog
   "console.log('")
(global-set-key (kbd "C-c C-l") 'consolelog)

;; mocha shorthands
(fset 'newmochadesc
      "describe('', function() {});")
(global-set-key (kbd "C-c d") 'newmochadesc)

(fset 'newmochatest
      "it('', function() {});")
(global-set-key (kbd "C-c t") 'newmochatest)

(fset 'newmochabefore
      "beforeEach(function() {});")
(global-set-key (kbd "C-c b") 'newmochabefore)

(fset 'newmochaafter
      "afterEach(function() {});")
(global-set-key (kbd "C-c f") 'newmochaafter)


(defun beflh-fix-quotes ()
  (interactive)
  (while (re-search-forward "\"" nil t)
    (replace-match "'")))

(defun beflh-adaudio (n)
  (interactive)
  (shell-command (concat "osascript -e 'set Volume '" (number-to-string n))))

(defun beflh-geaudio ()
  (interactive)
  (shell-command-to-string "osascript -e 'output volume of (get volume settings)'"))

(defun beflh-seaudio ()
  (setq mode-line-beflh-audiol (beflh-geaudio)))

;; this is meant to be a jump off point for other scripts
(defun tag-word-or-region (text-begin text-end)
  "Surround current word or region with given text."
  (interactive "sStart tag: \nsEnd tag: ")
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (progn
          (goto-char (region-end))
          (insert text-end)
          (goto-char (region-beginning))
          (insert text-begin))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (goto-char (cdr bds))
        (insert text-end)
        (goto-char (car bds))
        (insert text-begin)))))

(setq js2-skip-preprocessor-directives t)

;; footer that FlyCheck insists on for whatever reason.
(provide '.emacs)
;;; .emacs ends here
