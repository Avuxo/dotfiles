;;; hitagi-theme.el --- A fruity color theme for Emacs.

;; Copyright (C) 2011-2016

;; Author: Kelvin Smith <oneKelvinSmith@gmail.com>
;; URL: http://github.com/oneKelvinSmith/hitagi-emacs
;; Version: 3.3.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of the popular Textmate theme Hitagi for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.
;;
;;; Credits:
;;
;; Wimer Hazenberg created the original theme.
;; - http://www.hitagi.nl/blog/2006/07/15/textmate-color-theme/
;;
;; Bozhidar Batsov created zenburn-theme.el and solarized-theme.el
;;  on which this file is based.
;; - https://github.com/bbatsov/zenburn-emacs
;;
;; Color Scheme Designer 3 for complementary colours.
;; - http://colorschemedesigner.com/
;;
;; Xterm 256 Color Chart
;; - https://upload.wikimedia.org/wikipedia/en/1/15/Xterm_256color_chart.svg
;;
;; K. Adam Christensen for his personal hitagi theme that addresses 256 colours.
;; - https://github.com/pope/personal/blob/master/etc/emacs.d/hitagi-theme.el
;;
;; Thomas Frössman for his work on solarized-emacs.
;; - http://github.com/bbatsov/solarized-emacs
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The hitagi theme requires Emacs 24 or later!"))

(deftheme hitagi "Ben's theme (based off of Hitagi)")

(defgroup hitagi nil
  "Hitagi theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom hitagi-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'hitagi)

(defcustom hitagi-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'hitagi)

(defcustom hitagi-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'hitagi)

(defcustom hitagi-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'hitagi)

(defcustom hitagi-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'hitagi)

(defcustom hitagi-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'hitagi)

(defcustom hitagi-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'hitagi)

;; Primary colors
(defcustom hitagi-yellow "#E6DB74"
  "Primary colors - yellow"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-orange "#FD971F"
  "Primary colors - orange"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-red "#F92672"
  "Primary colors - red"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-magenta "#FD5FF0"
  "Primary colors - magenta"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-blue "#66D9EF"
  "Primary colors - blue"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-green "#A6E22E"
  "Primary colors - green"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-cyan "#A1EFE4"
  "Primary colors - cyan"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-violet "#AE81FF"
  "Primary colors - violet"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-gray "#64645E"
  "Primary colors - gray"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-foreground "#F8F8F2"
  "Adaptive colors - foreground"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-background "#272822"
  "Adaptive colors - background"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-comments "#75715E"
  "Adaptive colors - comments"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-emphasis "#F8F8F0"
  "Adaptive colors - emphasis"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-line-number "#8F908A"
  "Adaptive colors - line number"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-highlight "#49483E"
  "Adaptive colors - highlight"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-highlight-alt "#3E3D31"
  "Adaptive colors - highlight"
  :type 'string
  :group 'hitagi)

(defcustom hitagi-highlight-line "#3C3D37"
  "Adaptive colors - line highlight"
  :type 'string
  :group 'hitagi)

(let* (;; Variable pitch
       (hitagi-pitch (if hitagi-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (hitagi-class '((class color) (min-colors 257)))

       ;; Darker and lighter accented colors
       (hitagi-yellow-d       "#BEB244")
       (hitagi-yellow-l       "#FFF7A8")
       (hitagi-orange-d       "#D47402")
       (hitagi-orange-l       "#FFAC4A")
       (hitagi-red-d          "#F70057")
       (hitagi-red-l          "#FA518D")
       (hitagi-magenta-d      "#FB35EA")
       (hitagi-magenta-l      "#FE8CF4")
       (hitagi-violet-d       "#945AFF")
       (hitagi-violet-l       "#C9ACFF")
       (hitagi-blue-d         "#40CAE4")
       (hitagi-blue-l         "#92E7F7")
       (hitagi-cyan-d         "#74DBCD")
       (hitagi-cyan-l         "#D3FBF6")
       (hitagi-green-d        "#86C30D")
       (hitagi-green-l        "#BBEF53")
       (hitagi-gray-d         "#35331D")
       (hitagi-gray-l         "#7B7962")
       ;; Adaptive higher/lower contrast accented colors
       (hitagi-foreground-hc  "#141414")
       (hitagi-foreground-lc  "#171A0B")
       ;; High contrast colors
       (hitagi-yellow-hc      "#FFFACE")
       (hitagi-yellow-lc      "#9A8F21")
       (hitagi-orange-hc      "#FFBE74")
       (hitagi-orange-lc      "#A75B00")
       (hitagi-red-hc         "#FEB0CC")
       (hitagi-red-lc         "#F20055")
       (hitagi-magenta-hc     "#FEC6F9")
       (hitagi-magenta-lc     "#F309DF")
       (hitagi-violet-hc      "#F0E7FF")
       (hitagi-violet-lc      "#7830FC")
       (hitagi-blue-hc        "#CAF5FD")
       (hitagi-blue-lc        "#1DB4D0")
       (hitagi-cyan-hc        "#D3FBF6")
       (hitagi-cyan-lc        "#4BBEAE")
       (hitagi-green-hc       "#CCF47C")
       (hitagi-green-lc       "#679A01")

       ;; Distinct fringe
       (hitagi-fringe-bg (if hitagi-distinct-fringe-background
                              hitagi-gray
                            hitagi-background))

       ;; Definitions for terminals that do not support 256 colors
       (hitagi-256-class '((class color) (min-colors 89)))
       ;; Primary colors
       (hitagi-256-yellow         "#CDC673")
       (hitagi-256-orange         "#FF8C00")
       (hitagi-256-red            "#FF1493")
       (hitagi-256-magenta        "#D700D7")
       (hitagi-256-violet         "#AF87FF")
       (hitagi-256-blue           "#5FD7FF")
       (hitagi-256-cyan           "#5FFFFF")
       (hitagi-256-green          "#87D700")
       (hitagi-256-gray           "#3D3D3D")
       ;; Darker and lighter accented colors
       (hitagi-256-yellow-d       "#878700")
       (hitagi-256-yellow-l       "#FFFF87")
       (hitagi-256-orange-d       "#AF5F00")
       (hitagi-256-orange-l       "#FFAF5F")
       (hitagi-256-red-d          "#870000")
       (hitagi-256-red-l          "#FF5F87")
       (hitagi-256-magenta-d      "#AF0087")
       (hitagi-256-magenta-l      "#FF87DF")
       (hitagi-256-violet-d       "#5F00AF")
       (hitagi-256-violet-l       "#AF87D7")
       (hitagi-256-blue-d         "#008787")
       (hitagi-256-blue-l         "#87D7FF")
       (hitagi-256-cyan-d         "#5FAFAF")
       (hitagi-256-cyan-l         "#AFFFFF")
       (hitagi-256-green-d        "#5F8700")
       (hitagi-256-green-l        "#AFD700")
       (hitagi-256-gray-d         "#333333")
       (hitagi-256-gray-l         "#707070")
       ;; Adaptive colors
       (hitagi-256-foreground     "#F5F5F5")
       (hitagi-256-background     "#1B1E1C")
       (hitagi-256-comments       "#8B8878")
       (hitagi-256-emphasis       "#FFFAFA")
       (hitagi-256-line-number    "#8F908A")
       (hitagi-256-highlight      "#474747")
       (hitagi-256-highlight-alt  "#3E3E3E")
       (hitagi-256-highlight-line "#000000")
       ;; Adaptive higher/lower contrast accented colors
       (hitagi-256-foreground-hc  "#171A0B")
       (hitagi-256-foreground-lc  "#141414")
       ;; High contrast colors
       (hitagi-256-yellow-hc      hitagi-256-yellow-d)
       (hitagi-256-yellow-lc      hitagi-256-yellow-l)
       (hitagi-256-orange-hc      hitagi-256-orange-d)
       (hitagi-256-orange-lc      hitagi-256-orange-l)
       (hitagi-256-red-hc         hitagi-256-red-d)
       (hitagi-256-red-lc         hitagi-256-red-l)
       (hitagi-256-magenta-hc     hitagi-256-magenta-d)
       (hitagi-256-magenta-lc     hitagi-256-magenta-l)
       (hitagi-256-violet-hc      hitagi-256-violet-d)
       (hitagi-256-violet-lc      hitagi-256-violet-l)
       (hitagi-256-blue-hc        hitagi-256-blue-d)
       (hitagi-256-blue-lc        hitagi-256-blue-l)
       (hitagi-256-cyan-hc        hitagi-256-cyan-d)
       (hitagi-256-cyan-lc        hitagi-256-cyan-l)
       (hitagi-256-green-hc       hitagi-256-green-d)
       (hitagi-256-green-lc       hitagi-256-green-l)

       ;; Distinct fringe
       (hitagi-256-fringe-bg (if hitagi-distinct-fringe-background
                                  hitagi-256-gray
                                hitagi-256-background)))

  ;; Define faces
  (custom-theme-set-faces
   'hitagi

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,hitagi-class (:foreground ,hitagi-red
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(font-lock-comment-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(font-lock-constant-face
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(font-lock-doc-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(font-lock-function-name-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(font-lock-keyword-face
     ((,hitagi-class (:foreground ,hitagi-red
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :weight normal))))

   `(font-lock-negation-char-face
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight bold))))

   `(font-lock-preprocessor-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(font-lock-regexp-grouping-construct
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,hitagi-class (:foreground ,hitagi-violet
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet
                                        :weight normal))))

   `(font-lock-string-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(font-lock-type-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :italic nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :italic nil))))

   `(font-lock-variable-name-face
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(font-lock-warning-face
     ((,hitagi-class (:foreground ,hitagi-orange
                                   :weight bold
                                   :italic t
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange
                                        :weight bold
                                        :italic t
                                        :underline t))))

   `(c-annotation-face
     ((,hitagi-class (:inherit font-lock-constant-face))
      (,hitagi-256-class  (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,hitagi-class (:foreground ,hitagi-foreground
                                    :background ,hitagi-background))
       (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                         :background ,hitagi-256-background))))

   `(highlight
     ((,hitagi-class (:background ,hitagi-highlight))
      (,hitagi-256-class  (:background ,hitagi-256-highlight))))

   `(lazy-highlight
     ((,hitagi-class (:inherit highlight
                                :background ,hitagi-highlight-alt))
      (,hitagi-256-class  (:inherit highlight
                                     :background ,hitagi-256-highlight-alt))))

   `(region
     ((,hitagi-class (:inherit highlight
                                :background ,hitagi-highlight))
      (,hitagi-256-class  (:inherit highlight
                                     :background ,hitagi-256-highlight))))

   `(secondary-selection
     ((,hitagi-class (:inherit region
                                :background ,hitagi-highlight-alt))
      (,hitagi-256-class  (:inherit region
                                     :background ,hitagi-256-highlight-alt))))

   `(shadow
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(match
     ((,hitagi-class (:background ,hitagi-green
                                   :foreground ,hitagi-background
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-green
                                        :foreground ,hitagi-256-background
                                        :weight bold))))

   `(cursor
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-foreground
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-foreground
                                        :inverse-video t))))

   `(mouse
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-foreground
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-foreground
                                        :inverse-video t))))

   `(escape-glyph
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(escape-glyph-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(fringe
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :background ,hitagi-fringe-bg))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :background ,hitagi-256-fringe-bg))))

   `(link
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :underline t
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :underline t
                                        :weight bold))))

   `(link-visited
     ((,hitagi-class (:foreground ,hitagi-violet
                                   :underline t
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet
                                        :underline t
                                        :weight normal))))

   `(success
     ((,hitagi-class (:foreground ,hitagi-green ))
      (,hitagi-256-class  (:foreground ,hitagi-256-green ))))

   `(warning
     ((,hitagi-class (:foreground ,hitagi-yellow ))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow ))))

   `(error
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(eval-sexp-fu-flash
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-green))))

   `(eval-sexp-fu-flash-error
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-red))))

   `(trailing-whitespace
     ((,hitagi-class (:background ,hitagi-red))
      (,hitagi-256-class  (:background ,hitagi-256-red))))

   `(vertical-border
     ((,hitagi-class (:foreground ,hitagi-gray))
      (,hitagi-256-class  (:foreground ,hitagi-256-gray))))

   `(menu
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :background ,hitagi-256-background))))

   `(minibuffer-prompt
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,hitagi-class (:foreground ,"#3dfbff"
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :weight bold))))

   `(mode-line
     ((,hitagi-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,hitagi-emphasis
                                      :background ,hitagi-highlight
                                      :box (:line-width 1
                                                        :color ,"#595959"
                                                        :style unspecified)))
      (,hitagi-256-class  (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,hitagi-256-foreground
                                           :background ,hitagi-256-background
                                           :box (:line-width 1
                                                             :color ,hitagi-256-highlight
                                                             :style unspecified)))))

   `(powerline-active1
     ((,hitagi-class (:background ,hitagi-gray-d))
      (,hitagi-256-class  (:background ,hitagi-256-gray-d))))

   `(powerline-active2
     ((,hitagi-class (:background ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-background))))


   `(mode-line-inactive
     ((,hitagi-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,hitagi-comments
                                      :background ,hitagi-background
                                      :box (:line-width 1
                                                        :color ,hitagi-gray
                                                        :style unspecified)))
      (,hitagi-256-class  (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,hitagi-256-comments
                                           :background ,hitagi-256-background
                                           :box (:line-width 1
                                                             :color ,hitagi-256-gray
                                                             :style unspecified)))))

   `(powerline-inactive1
     ((,hitagi-class (:background ,hitagi-gray-d))
      (,hitagi-256-class  (:background ,hitagi-256-gray-d))))

   `(powerline-inactive2
     ((,hitagi-class (:background ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-background))))

   ;; header-line
   `(header-line
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :background ,hitagi-highlight
                                   :box (:color ,hitagi-gray
                                                :line-width 1
                                                :style unspecified)))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :background ,hitagi-256-highlight
                                        :box (:color ,hitagi-256-gray
                                                     :line-width 1
                                                     :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,hitagi-class (:background ,hitagi-yellow
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-yellow
                                        :foreground ,hitagi-256-background))))

   `(cua-rectangle
     ((,hitagi-class (:inherit region))
      (,hitagi-256-class  (:inherit region))))

   `(cua-rectangle-noselect
     ((,hitagi-class (:inherit secondary-selection))
      (,hitagi-256-class  (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   ;; dired
   `(dired-directory
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(dired-flagged
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(dired-header
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :background ,hitagi-background
                                   :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :background ,hitagi-256-background
                                        :inherit bold))))

   `(dired-ignored
     ((,hitagi-class (:inherit shadow))
      (,hitagi-256-class  (:inherit shadow))))

   `(dired-mark
     ((,hitagi-class (:foreground ,hitagi-green
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :weight bold))))

   `(dired-marked
     ((,hitagi-class (:foreground ,hitagi-violet
                                   :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet
                                        :inherit bold))))

   `(dired-perm-write
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :underline t))))

   `(dired-symlink
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :slant italic))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :slant italic))))

   `(dired-warning
     ((,hitagi-class (:foreground ,hitagi-orange
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange
                                        :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-blue))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-blue))))

   `(dropdown-list-selection-face
     ((,hitagi-class (:background ,hitagi-green
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-green
                                        :foreground ,hitagi-256-background))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,hitagi-class (:background ,hitagi-blue
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-blue
                                        :foreground ,hitagi-256-background))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,hitagi-class (:inherit ecb-history-bucket-node-face
                                :foreground ,hitagi-yellow))
      (,hitagi-256-class  (:inherit ecb-history-bucket-node-face
                                     :foreground ,hitagi-256-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,hitagi-class (:inherit ecb-directories-general-face
                                :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:inherit ecb-directories-general-face
                                     :foreground ,hitagi-256-foreground))))

   `(ecb-history-dead-buffer-face
     ((,hitagi-class (:inherit ecb-history-general-face
                                :foreground ,hitagi-comments))
      (,hitagi-256-class  (:inherit ecb-history-general-face
                                     :foreground ,hitagi-256-comments))))

   `(ecb-directory-not-accessible-face
     ((,hitagi-class (:inherit ecb-directories-general-face
                                :foreground ,hitagi-comments))
      (,hitagi-256-class  (:inherit ecb-directories-general-face
                                     :foreground ,hitagi-256-comments))))

   `(ecb-bucket-node-face
     ((,hitagi-class (:inherit ecb-default-general-face
                                :weight normal
                                :foreground ,hitagi-blue))
      (,hitagi-256-class  (:inherit ecb-default-general-face
                                     :weight normal
                                     :foreground ,hitagi-256-blue))))

   `(ecb-tag-header-face
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,hitagi-class (:inherit ecb-analyse-general-face
                                :foreground ,hitagi-green))
      (,hitagi-256-class  (:inherit ecb-analyse-general-face
                                     :foreground ,hitagi-256-green))))

   `(ecb-directories-general-face
     ((,hitagi-class (:inherit ecb-default-general-face
                                :height 1.0))
      (,hitagi-256-class  (:inherit ecb-default-general-face
                                     :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,hitagi-class (:inherit ecb-methods-general-face
                                :foreground ,hitagi-cyan))
      (,hitagi-256-class  (:inherit ecb-methods-general-face
                                     :foreground ,hitagi-256-cyan))))

   `(ecb-mode-line-prefix-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(ecb-tree-guide-line-face
     ((,hitagi-class (:inherit ecb-default-general-face
                                :foreground ,hitagi-gray
                                :height 1.0))
      (,hitagi-256-class  (:inherit ecb-default-general-face
                                     :foreground ,hitagi-256-gray
                                     :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,hitagi-class (:foreground ,hitagi-emphasis))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis))))

   `(ee-category
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(ee-link
     ((,hitagi-class (:inherit link))
      (,hitagi-256-class  (:inherit link))))

   `(ee-link-visited
     ((,hitagi-class (:inherit link-visited))
      (,hitagi-256-class  (:inherit link-visited))))

   `(ee-marked
     ((,hitagi-class (:foreground ,hitagi-magenta
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-magenta
                                        :weight bold))))

   `(ee-omitted
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(ee-shadow
     ((,hitagi-class (:inherit shadow))
      (,hitagi-256-class  (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(grep-error-face
     ((,hitagi-class (:foreground ,hitagi-red
                                   :weight bold
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :weight bold
                                        :underline t))))

   `(grep-hit-face
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(grep-match-face
     ((,hitagi-class (:foreground ,hitagi-green
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :weight bold))))

   ;; isearch
   `(isearch
     ((,hitagi-class (:inherit region
                                :background ,hitagi-green))
      (,hitagi-256-class  (:inherit region
                                     :background ,hitagi-256-green))))

   `(isearch-fail
     ((,hitagi-class (:inherit isearch
                                :foreground ,hitagi-red
                                :background ,hitagi-background
                                :bold t))
      (,hitagi-256-class  (:inherit isearch
                                     :foreground ,hitagi-256-red
                                     :background ,hitagi-256-background
                                     :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :background ,hitagi-background
                                   :inverse-video nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :background ,hitagi-256-background
                                        :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :background ,hitagi-background
                                   :inverse-video nil
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :background ,hitagi-256-background
                                        :inverse-video nil
                                        :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,hitagi-class (:inherit bold
                                :foreground ,hitagi-emphasis))
      (,hitagi-256-class  (:inherit bold
                                     :foreground ,hitagi-256-emphasis))))

   `(font-latex-doctex-documentation-face
     ((,hitagi-class (:background unspecified))
      (,hitagi-256-class  (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,hitagi-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,hitagi-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,hitagi-class (:inherit italic :foreground ,hitagi-emphasis))
      (,hitagi-256-class  (:inherit italic :foreground ,hitagi-256-emphasis))))

   `(font-latex-math-face
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(font-latex-sectioning-0-face
     ((,hitagi-class (:inherit font-latex-sectioning-1-face
                                :height ,hitagi-height-plus-1))
      (,hitagi-256-class  (:inherit font-latex-sectioning-1-face
                                     :height ,hitagi-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,hitagi-class (:inherit font-latex-sectioning-2-face
                                :height ,hitagi-height-plus-1))
      (,hitagi-256-class  (:inherit font-latex-sectioning-2-face
                                     :height ,hitagi-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,hitagi-class (:inherit font-latex-sectioning-3-face
                                :height ,hitagi-height-plus-1))
      (,hitagi-256-class  (:inherit font-latex-sectioning-3-face
                                     :height ,hitagi-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,hitagi-class (:inherit font-latex-sectioning-4-face
                                :height ,hitagi-height-plus-1))
      (,hitagi-256-class  (:inherit font-latex-sectioning-4-face
                                     :height ,hitagi-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,hitagi-class (:inherit font-latex-sectioning-5-face
                                :height ,hitagi-height-plus-1))
      (,hitagi-256-class  (:inherit font-latex-sectioning-5-face
                                     :height ,hitagi-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-yellow
                                :weight bold))
      (,hitagi-256-class  (:inherit ,hitagi-pitch :
                                     foreground ,hitagi-256-yellow
                                     :weight bold))))

   `(font-latex-sedate-face
     ((,hitagi-class (:foreground ,hitagi-emphasis))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis))))

   `(font-latex-slide-title-face
     ((,hitagi-class (:inherit (,hitagi-pitch font-lock-type-face)
                                :weight bold
                                :height ,hitagi-height-plus-3))
      (,hitagi-256-class  (:inherit (,hitagi-pitch font-lock-type-face)
                                     :weight bold
                                     :height ,hitagi-height-plus-3))))

   `(font-latex-string-face
     ((,hitagi-class (:foreground ,hitagi-cyan))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan))))

   `(font-latex-subscript-face
     ((,hitagi-class (:height ,hitagi-height-minus-1))
      (,hitagi-256-class  (:height ,hitagi-height-minus-1))))

   `(font-latex-superscript-face
     ((,hitagi-class (:height ,hitagi-height-minus-1))
      (,hitagi-256-class  (:height ,hitagi-height-minus-1))))

   `(font-latex-verbatim-face
     ((,hitagi-class (:inherit fixed-pitch
                                :foreground ,hitagi-foreground
                                :slant italic))
      (,hitagi-256-class  (:inherit fixed-pitch
                                     :foreground ,hitagi-256-foreground
                                     :slant italic))))

   `(font-latex-warning-face
     ((,hitagi-class (:inherit bold
                                :foreground ,hitagi-orange))
      (,hitagi-256-class  (:inherit bold
                                     :foreground ,hitagi-256-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-blue))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-blue))))

   `(ac-selection-face
     ((,hitagi-class (:background ,hitagi-blue
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-blue
                                        :foreground ,hitagi-256-background))))

   `(ac-candidate-mouse-face
     ((,hitagi-class (:background ,hitagi-blue
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-blue
                                        :foreground ,hitagi-256-background))))

   `(ac-completion-face
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :underline t))))

   `(ac-gtags-candidate-face
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-blue))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-blue))))

   `(ac-gtags-selection-face
     ((,hitagi-class (:background ,hitagi-blue
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-blue
                                        :foreground ,hitagi-256-background))))

   `(ac-yasnippet-candidate-face
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-yellow))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-yellow))))

   `(ac-yasnippet-selection-face
     ((,hitagi-class (:background ,hitagi-yellow
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-yellow
                                        :foreground ,hitagi-256-background))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-blue))))

   `(ahs-edit-mode-face
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-highlight))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-highlight))))

   `(ahs-face
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-magenta
                                        :background unspecified))))

   `(ahs-plugin-bod-face
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-violet ))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-cyan ))))

   `(ahs-plugin-defalt-face
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-green))))

   `(ahs-warning-face
     ((,hitagi-class (:foreground ,hitagi-red
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(android-mode-error-face
     ((,hitagi-class (:foreground ,hitagi-orange
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange
                                        :weight bold))))

   `(android-mode-info-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(android-mode-verbose-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(android-mode-warning-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,hitagi-class (:foreground ,hitagi-violet
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet
                                        :weight bold))))

   ;; bm
   `(bm-face
     ((,hitagi-class (:background ,hitagi-yellow-lc
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-yellow-lc
                                        :foreground ,hitagi-256-background))))

   `(bm-fringe-face
     ((,hitagi-class (:background ,hitagi-yellow-lc
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-yellow-lc
                                        :foreground ,hitagi-256-background))))

   `(bm-fringe-persistent-face
     ((,hitagi-class (:background ,hitagi-green-lc
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-green-lc
                                        :foreground ,hitagi-256-background))))

   `(bm-persistent-face
     ((,hitagi-class (:background ,hitagi-green-lc
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-green-lc
                                        :foreground ,hitagi-256-background))))

   ;; calfw
   `(cfw:face-day-title
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   `(cfw:face-annotation
     ((,hitagi-class (:inherit cfw:face-day-title
                                :foreground ,hitagi-yellow))
      (,hitagi-256-class  (:inherit cfw:face-day-title
                                     :foreground ,hitagi-256-yellow))))

   `(cfw:face-default-content
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(cfw:face-default-day
     ((,hitagi-class (:inherit cfw:face-day-title
                                :weight bold))
      (,hitagi-256-class  (:inherit cfw:face-day-title
                                     :weight bold))))

   `(cfw:face-disable
     ((,hitagi-class (:inherit cfw:face-day-title
                                :foreground ,hitagi-comments))
      (,hitagi-256-class  (:inherit cfw:face-day-title
                                     :foreground ,hitagi-256-comments))))

   `(cfw:face-grid
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(cfw:face-header
     ((,hitagi-class (:foreground ,hitagi-blue-hc
                                   :background ,hitagi-blue-lc
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue-hc
                                        :background ,hitagi-256-blue-lc
                                        :weight bold))))

   `(cfw:face-holiday
     ((,hitagi-class (:background nil
                                   :foreground ,hitagi-red
                                   :weight bold))
      (,hitagi-256-class  (:background nil
                                        :foreground ,hitagi-256-red
                                        :weight bold))))

   `(cfw:face-periods
     ((,hitagi-class (:foreground ,hitagi-magenta))
      (,hitagi-256-class  (:foreground ,hitagi-256-magenta))))

   `(cfw:face-select
     ((,hitagi-class (:background ,hitagi-magenta-lc
                                   :foreground ,hitagi-magenta-hc))
      (,hitagi-256-class  (:background ,hitagi-256-magenta-lc
                                        :foreground ,hitagi-256-magenta-hc))))

   `(cfw:face-saturday
     ((,hitagi-class (:foreground ,hitagi-cyan-hc
                                   :background ,hitagi-cyan-lc))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan-hc
                                        :background ,hitagi-256-cyan-lc))))

   `(cfw:face-sunday
     ((,hitagi-class (:foreground ,hitagi-red-hc
                                   :background ,hitagi-red-lc
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-red-hc
                                        :background ,hitagi-256-red-lc
                                        :weight bold))))

   `(cfw:face-title
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-yellow
                                :weight bold
                                :height ,hitagi-height-plus-4))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-yellow
                                     :weight bold
                                     :height ,hitagi-height-plus-4))))

   `(cfw:face-today
     ((,hitagi-class (:weight bold
                               :background ,hitagi-highlight-line
                               :foreground nil))
      (,hitagi-256-class  (:weight bold
                                    :background ,hitagi-256-highlight-line
                                    :foreground nil))))

   `(cfw:face-today-title
     ((,hitagi-class (:background ,hitagi-yellow-lc
                                   :foreground ,hitagi-yellow-hc
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-yellow-lc
                                        :foreground ,hitagi-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-foreground))))

   `(cfw:face-toolbar-button-off
     ((,hitagi-class (:background ,hitagi-yellow-lc
                                   :foreground ,hitagi-yellow-hc
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-yellow-lc
                                        :foreground ,hitagi-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,hitagi-class (:background ,hitagi-yellow-hc
                                   :foreground ,hitagi-yellow-lc
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-yellow-hc
                                        :foreground ,hitagi-256-yellow-lc
                                        :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :background nil
                                   :box (:color ,hitagi-yellow :line-width -1 :style nil)))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :background nil
                                        :box (:color ,hitagi-256-yellow :line-width -1 :style nil))) ))

   `(cider-enlightened-local
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(cider-instrumented-face
     ((,hitagi-class (:foreground ,hitagi-violet
                                   :background nil
                                   :box (:color ,hitagi-violet :line-width -1 :style nil)))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet
                                        :background nil
                                        :box (:color ,hitagi-256-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :background nil
                                   :box (:color ,hitagi-blue :line-width -1 :style nil)))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :background nil
                                        :box (:color ,hitagi-256-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-orange))))

   `(cider-test-failure-face
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-red))))

   `(cider-test-success-face
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-green))))

   `(cider-traced-face
     ((,hitagi-class :box (:color ,hitagi-blue :line-width -1 :style nil))
      (,hitagi-256-class  :box (:color ,hitagi-256-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,hitagi-class (:foreground ,hitagi-red
                                   :weight bold
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-error-face
     ((,hitagi-class (:foreground ,hitagi-orange
                                   :weight bold
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-success-face
     ((,hitagi-class (:foreground ,hitagi-green
                                   :weight bold
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :weight bold
                                        :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-emphasis))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-emphasis))))

   `(company-tooltip-selection
     ((,hitagi-class (:background ,hitagi-blue
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-blue
                                        :foreground ,hitagi-256-background))))

   `(company-tooltip-mouse
     ((,hitagi-class (:background ,hitagi-blue
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-blue
                                        :foreground ,hitagi-256-background))))

   `(company-tooltip-common
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :underline t))))

   `(company-tooltip-common-selection
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-blue
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-blue
                                        :underline t))))

   `(company-preview
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-emphasis))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-emphasis))))

   `(company-preview-common
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :underline t))))

   `(company-scrollbar-bg
     ((,hitagi-class (:background ,hitagi-gray))
      (,hitagi-256-class  (:background ,hitagi-256-gray))))

   `(company-scrollbar-fg
     ((,hitagi-class (:background ,hitagi-comments))
      (,hitagi-256-class  (:background ,hitagi-256-comments))))

   `(company-tooltip-annotation
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-green))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-green))))

   `(company-template-field
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-blue))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-blue))))

   ;; compilation
   `(compilation-column-face
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :underline nil))))

   `(compilation-column-number
     ((,hitagi-class (:inherit font-lock-doc-face
                                :foreground ,hitagi-cyan
                                :underline nil))
      (,hitagi-256-class  (:inherit font-lock-doc-face
                                     :foreground ,hitagi-256-cyan
                                     :underline nil))))

   `(compilation-enter-directory-face
     ((,hitagi-class (:foreground ,hitagi-green
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :underline nil))))

   `(compilation-error
     ((,hitagi-class (:inherit error
                                :underline nil))
      (,hitagi-256-class  (:inherit error
                                     :underline nil))))

   `(compilation-error-face
     ((,hitagi-class (:foreground ,hitagi-red
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :underline nil))))

   `(compilation-face
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :underline nil))))

   `(compilation-info
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :underline nil
                                   :bold nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :underline nil
                                        :bold nil))))

   `(compilation-info-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :underline nil))))

   `(compilation-leave-directory-face
     ((,hitagi-class (:foreground ,hitagi-green
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :underline nil))))

   `(compilation-line-face
     ((,hitagi-class (:foreground ,hitagi-green
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :underline nil))))

   `(compilation-line-number
     ((,hitagi-class (:foreground ,hitagi-green
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :underline nil))))

   `(compilation-warning
     ((,hitagi-class (:inherit warning
                                :underline nil))
      (,hitagi-256-class  (:inherit warning
                                     :underline nil))))

   `(compilation-warning-face
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight normal
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(compilation-mode-line-exit
     ((,hitagi-class (:inherit compilation-info
                                :foreground ,hitagi-green
                                :weight bold))
      (,hitagi-256-class  (:inherit compilation-info
                                     :foreground ,hitagi-256-green
                                     :weight bold))))

   `(compilation-mode-line-fail
     ((,hitagi-class (:inherit compilation-error
                                :foreground ,hitagi-red
                                :weight bold))
      (,hitagi-256-class  (:inherit compilation-error
                                     :foreground ,hitagi-256-red
                                     :weight bold))))

   `(compilation-mode-line-run
     ((,hitagi-class (:foreground ,hitagi-orange
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange
                                        :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,hitagi-class (:foreground ,hitagi-green
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :weight bold))))

   `(cscope-function-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(cscope-line-number-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(cscope-line-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(cscope-mouse-face
     ((,hitagi-class (:background ,hitagi-blue
                                   :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:background ,hitagi-256-blue
                                        :foreground ,hitagi-256-foreground))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-emphasis
                                   :underline ,hitagi-emphasis
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-emphasis
                                        :underline ,hitagi-256-emphasis
                                        :weight bold))))

   `(ctbl:face-continue-bar
     ((,hitagi-class (:background ,hitagi-gray
                                   :foreground ,hitagi-yellow))
      (,hitagi-256-class  (:background ,hitagi-256-gray
                                        :foreground ,hitagi-256-yellow))))

   `(ctbl:face-row-select
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-foreground
                                   :underline t))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-foreground
                                        :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight bold))))

   `(coffee-mode-function-param
     ((,hitagi-class (:foreground ,hitagi-violet
                                   :slant italic))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet
                                        :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :height ,hitagi-height-plus-3
                                :foreground ,hitagi-violet
                                :weight bold))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :height ,hitagi-height-plus-3
                                     :foreground ,hitagi-256-violet
                                     :weight bold))))

   `(custom-variable-tag
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-cyan
                                :height ,hitagi-height-plus-3))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-cyan
                                     :height ,hitagi-height-plus-3))))

   `(custom-comment-tag
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(custom-group-tag
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-blue
                                :height ,hitagi-height-plus-3))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-blue
                                     :height ,hitagi-height-plus-3))))

   `(custom-group-tag-1
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-red
                                :height ,hitagi-height-plus-3))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-red
                                     :height ,hitagi-height-plus-3))))

   `(custom-state
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   ;; diff
   `(diff-added
     ((,hitagi-class (:foreground ,hitagi-green
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :background ,hitagi-256-background))))

   `(diff-changed
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :background ,hitagi-256-background))))

   `(diff-removed
     ((,hitagi-class (:foreground ,hitagi-red
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :background ,hitagi-256-background))))

   `(diff-header
     ((,hitagi-class (:background ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-background))))

   `(diff-file-header
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-foreground
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-foreground
                                        :weight bold))))

   `(diff-refine-added
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-green))))

   `(diff-refine-change
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-blue))))

   `(diff-refine-removed
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,hitagi-class (:background ,hitagi-blue-lc
                                   :foreground ,hitagi-blue-hc))
      (,hitagi-256-class  (:background ,hitagi-256-blue-lc
                                        :foreground ,hitagi-256-blue-hc))))

   `(diff-hl-delete
     ((,hitagi-class (:background ,hitagi-red-lc
                                   :foreground ,hitagi-red-hc))
      (,hitagi-256-class  (:background ,hitagi-256-red-lc
                                        :foreground ,hitagi-256-red-hc))))

   `(diff-hl-insert
     ((,hitagi-class (:background ,hitagi-green-lc
                                   :foreground ,hitagi-green-hc))
      (,hitagi-256-class  (:background ,hitagi-256-green-lc
                                        :foreground ,hitagi-256-green-hc))))

   `(diff-hl-unknown
     ((,hitagi-class (:background ,hitagi-violet-lc
                                   :foreground ,hitagi-violet-hc))
      (,hitagi-256-class  (:background ,hitagi-256-violet-lc
                                        :foreground ,hitagi-256-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,hitagi-class (:background ,hitagi-orange-lc))
      (,hitagi-256-class  (:background ,hitagi-256-orange-lc))))

   `(ediff-fine-diff-B
     ((,hitagi-class (:background ,hitagi-green-lc))
      (,hitagi-256-class  (:background ,hitagi-256-green-lc))))

   `(ediff-fine-diff-C
     ((,hitagi-class (:background ,hitagi-yellow-lc))
      (,hitagi-256-class  (:background ,hitagi-256-yellow-lc))))

   `(ediff-current-diff-C
     ((,hitagi-class (:background ,hitagi-blue-lc))
      (,hitagi-256-class  (:background ,hitagi-256-blue-lc))))

   `(ediff-even-diff-A
     ((,hitagi-class (:background ,hitagi-comments
                                   :foreground ,hitagi-foreground-lc ))
      (,hitagi-256-class  (:background ,hitagi-256-comments
                                        :foreground ,hitagi-256-foreground-lc ))))

   `(ediff-odd-diff-A
     ((,hitagi-class (:background ,hitagi-comments
                                   :foreground ,hitagi-foreground-hc ))
      (,hitagi-256-class  (:background ,hitagi-256-comments
                                        :foreground ,hitagi-256-foreground-hc ))))

   `(ediff-even-diff-B
     ((,hitagi-class (:background ,hitagi-comments
                                   :foreground ,hitagi-foreground-hc ))
      (,hitagi-256-class  (:background ,hitagi-256-comments
                                        :foreground ,hitagi-256-foreground-hc ))))

   `(ediff-odd-diff-B
     ((,hitagi-class (:background ,hitagi-comments
                                   :foreground ,hitagi-foreground-lc ))
      (,hitagi-256-class  (:background ,hitagi-256-comments
                                        :foreground ,hitagi-256-foreground-lc ))))

   `(ediff-even-diff-C
     ((,hitagi-class (:background ,hitagi-comments
                                   :foreground ,hitagi-foreground ))
      (,hitagi-256-class  (:background ,hitagi-256-comments
                                        :foreground ,hitagi-256-foreground ))))

   `(ediff-odd-diff-C
     ((,hitagi-class (:background ,hitagi-comments
                                   :foreground ,hitagi-background ))
      (,hitagi-256-class  (:background ,hitagi-256-comments
                                        :foreground ,hitagi-256-background ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) hitagi-class)
       (:underline (:style line :color ,hitagi-red)
                   :inherit unspecified))
      (,hitagi-class (:foreground ,hitagi-red-hc
                                   :background ,hitagi-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) hitagi-256-class )
       (:underline (:style line :color ,hitagi-256-red)
                   :inherit unspecified))
      (,hitagi-256-class  (:foreground ,hitagi-256-red-hc
                                        :background ,hitagi-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) hitagi-class)
       (:underline (:style line :color ,hitagi-yellow)
                   :inherit unspecified))
      (,hitagi-class (:foreground ,hitagi-yellow-hc
                                   :background ,hitagi-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) hitagi-256-class )
       (:underline (:style line :color ,hitagi-256-yellow)
                   :inherit unspecified))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow-hc
                                        :background ,hitagi-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,hitagi-class (:foreground ,hitagi-red
                                   :background unspecified
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :background unspecified
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-error-mode-line
     ((,hitagi-class (:background ,hitagi-red
                                   :foreground unspecified))
      (,hitagi-256-class  (:background ,hitagi-256-red
                                        :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,hitagi-class (:background ,hitagi-yellow
                                   :foreground unspecified))
      (,hitagi-256-class  (:background ,hitagi-256-yellow
                                        :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(elfeed-search-feed-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(elfeed-search-tag-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(elfeed-search-title-face
     ((,hitagi-class (:foreground ,hitagi-cyan))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan))))

   ;; ein
   `(ein:cell-input-area
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))
   `(ein:cell-input-prompt
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))
   `(ein:cell-output-prompt
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))
   `(ein:notification-tab-normal
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))
   `(ein:notification-tab-selected
     ((,hitagi-class (:foreground ,hitagi-orange :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,hitagi-class (:inherit font-lock-string-face))
      (,hitagi-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,hitagi-class (:inherit font-lock-string-face))
      (,hitagi-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,hitagi-class (:inherit font-lock-string-face))
      (,hitagi-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,hitagi-class (:inherit font-lock-keyword-face))
      (,hitagi-256-class  (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) hitagi-class)
       (:underline (:style wave :color ,hitagi-red)
                   :inherit unspecified))
      (,hitagi-class (:foreground ,hitagi-red-hc
                                   :background ,hitagi-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) hitagi-256-class )
       (:underline (:style wave :color ,hitagi-256-red)
                   :inherit unspecified))
      (,hitagi-256-class  (:foreground ,hitagi-256-red-hc
                                        :background ,hitagi-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) hitagi-class)
       (:underline (:style wave :color ,hitagi-orange)
                   :inherit unspecified))
      (,hitagi-class (:foreground ,hitagi-orange-hc
                                   :background ,hitagi-orange-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) hitagi-256-class )
       (:underline (:style wave :color ,hitagi-256-orange)
                   :inherit unspecified))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange-hc
                                        :background ,hitagi-256-orange-lc
                                        :weight bold
                                        :underline t))))

   ;; epc
   `(epc:face-title
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :background ,hitagi-background
                                   :weight normal
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :background ,hitagi-256-background
                                        :weight normal
                                        :underline nil))))

   ;; erc
   `(erc-action-face
     ((,hitagi-class (:inherit erc-default-face))
      (,hitagi-256-class  (:inherit erc-default-face))))

   `(erc-bold-face
     ((,hitagi-class (:weight bold))
      (,hitagi-256-class  (:weight bold))))

   `(erc-current-nick-face
     ((,hitagi-class (:foreground ,hitagi-blue :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :weight bold))))

   `(erc-dangerous-host-face
     ((,hitagi-class (:inherit font-lock-warning-face))
      (,hitagi-256-class  (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(erc-highlight-face
     ((,hitagi-class (:inherit erc-default-face
                                :background ,hitagi-highlight))
      (,hitagi-256-class  (:inherit erc-default-face
                                     :background ,hitagi-256-highlight))))

   `(erc-direct-msg-face
     ((,hitagi-class (:inherit erc-default-face))
      (,hitagi-256-class  (:inherit erc-default-face))))

   `(erc-error-face
     ((,hitagi-class (:inherit font-lock-warning-face))
      (,hitagi-256-class  (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,hitagi-class (:inherit erc-default-face))
      (,hitagi-256-class  (:inherit erc-default-face))))

   `(erc-input-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(erc-keyword-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :weight bold))))

   `(erc-nick-default-face
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight bold))))

   `(erc-my-nick-face
     ((,hitagi-class (:foreground ,hitagi-red
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :weight bold))))

   `(erc-nick-msg-face
     ((,hitagi-class (:inherit erc-default-face))
      (,hitagi-256-class  (:inherit erc-default-face))))

   `(erc-notice-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(erc-pal-face
     ((,hitagi-class (:foreground ,hitagi-orange
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange
                                        :weight bold))))

   `(erc-prompt-face
     ((,hitagi-class (:foreground ,hitagi-orange
                                   :background ,hitagi-background
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange
                                        :background ,hitagi-256-background
                                        :weight bold))))

   `(erc-timestamp-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :inherit bold))))

   `(eshell-ls-archive
     ((,hitagi-class (:foreground ,hitagi-red
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :inherit bold))))

   `(eshell-ls-backup
     ((,hitagi-class (:inherit font-lock-comment-face))
      (,hitagi-256-class  (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,hitagi-class (:inherit font-lock-comment-face))
      (,hitagi-256-class  (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :inherit bold))))

   `(eshell-ls-executable
     ((,hitagi-class (:foreground ,hitagi-green
                                   :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :inherit bold))))

   `(eshell-ls-unreadable
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(eshell-ls-missing
     ((,hitagi-class (:inherit font-lock-warning-face))
      (,hitagi-256-class  (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,hitagi-class (:inherit font-lock-doc-face))
      (,hitagi-256-class  (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :inherit bold))))

   `(eshell-ls-symlink
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-red-l
                                   :inherit italic))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-red-l
                                        :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-green-l
                                   :inherit italic))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line :foreground ,hitagi-256-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,hitagi-class (:inherit region))
      (,hitagi-256-class  (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-orange
                                   :underline t
                                   :slant italic))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-orange
                                        :underline t
                                        :slant italic))))

   `(fic-face
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-orange
                                   :weight normal
                                   :slant italic))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-orange
                                        :weight normal
                                        :slant italic))))

   `(font-lock-fic-face
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-orange
                                   :weight normal
                                   :slant italic))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-orange
                                        :weight normal
                                        :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :weight normal
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) hitagi-class)
       (:underline (:style wave :color ,hitagi-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,hitagi-class (:foreground ,hitagi-red-hc
                                   :background ,hitagi-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) hitagi-256-class )
       (:underline (:style wave :color ,hitagi-256-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,hitagi-256-class  (:foreground ,hitagi-256-red-hc
                                        :background ,hitagi-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) hitagi-class)
       (:underline (:style wave :color ,hitagi-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,hitagi-class (:foreground ,hitagi-green-hc
                                   :background ,hitagi-green-lc))
      (,(append '((supports :underline (:style wave))) hitagi-256-class )
       (:underline (:style wave :color ,hitagi-256-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,hitagi-256-class  (:foreground ,hitagi-256-green-hc
                                        :background ,hitagi-256-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) hitagi-class)
       (:underline (:style wave :color ,hitagi-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,hitagi-class (:foreground ,hitagi-yellow-hc
                                   :background ,hitagi-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) hitagi-256-class )
       (:underline (:style wave :color ,hitagi-256-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow-hc
                                        :background ,hitagi-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) hitagi-class)
       (:underline (:style line :color ,hitagi-red)))
      (,hitagi-class (:foreground ,hitagi-red
                                   :background ,hitagi-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) hitagi-256-class )
       (:underline (:style line :color ,hitagi-256-red)))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :background ,hitagi-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) hitagi-class)
       (:underline (:style line :color ,hitagi-orange)))
      (,hitagi-class (:foreground ,hitagi-orange
                                   :background ,hitagi-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) hitagi-256-class )
       (:underline (:style line :color ,hitagi-256-orange)))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange
                                        :background ,hitagi-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style line))) hitagi-class)
       (:underline (:style line :color ,hitagi-blue)))
      (,hitagi-class (:foreground ,hitagi-blue
                                   :background ,hitagi-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) hitagi-256-class )
       (:underline (:style line :color ,hitagi-256-blue)))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :background ,hitagi-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-fringe-error
     ((,hitagi-class (:foreground ,hitagi-red-l
                                   :background unspecified
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-red-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-warning
     ((,hitagi-class (:foreground ,hitagi-orange-l
                                   :background unspecified
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-info
     ((,hitagi-class (:foreground ,hitagi-blue-l
                                   :background unspecified
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue-l
                                        :background unspecified
                                        :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) hitagi-class)
       (:underline (:style wave :color ,hitagi-yellow)
                   :inherit unspecified))
      (,hitagi-class (:foreground ,hitagi-yellow
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) hitagi-256-class )
       (:underline (:style wave :color ,hitagi-256-yellow)
                   :inherit unspecified))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight bold
                                        :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) hitagi-class)
       (:underline (:style wave :color ,hitagi-red)
                   :inherit unspecified))
      (,hitagi-class (:foreground ,hitagi-red
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) hitagi-256-class )
       (:underline (:style wave :color ,hitagi-256-red)
                   :inherit unspecified))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :weight bold
                                        :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,hitagi-class (:background ,hitagi-green
                                   :foreground ,hitagi-background
                                   :inherit bold))
      (,hitagi-256-class  (:background ,hitagi-256-green
                                        :foreground ,hitagi-256-background
                                        :inherit bold))))

   `(git-gutter:deleted
     ((,hitagi-class (:background ,hitagi-red
                                   :foreground ,hitagi-background
                                   :inherit bold))
      (,hitagi-256-class  (:background ,hitagi-256-red
                                        :foreground ,hitagi-256-background
                                        :inherit bold))))

   `(git-gutter:modified
     ((,hitagi-class (:background ,hitagi-blue
                                   :foreground ,hitagi-background
                                   :inherit bold))
      (,hitagi-256-class  (:background ,hitagi-256-blue
                                        :foreground ,hitagi-256-background
                                        :inherit bold))))

   `(git-gutter:unchanged
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-background
                                   :inherit bold))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-background
                                        :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,hitagi-class (:foreground ,hitagi-green
                                   :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :inherit bold))))

   `(git-gutter-fr:deleted
     ((,hitagi-class (:foreground ,hitagi-red
                                   :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :inherit bold))))

   `(git-gutter-fr:modified
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,hitagi-class (:background ,hitagi-green
                                   :foreground ,hitagi-background
                                   :inherit bold))
      (,hitagi-256-class  (:background ,hitagi-256-green
                                        :foreground ,hitagi-256-background
                                        :inherit bold))))

   `(git-gutter+-deleted
     ((,hitagi-class (:background ,hitagi-red
                                   :foreground ,hitagi-background
                                   :inherit bold))
      (,hitagi-256-class  (:background ,hitagi-256-red
                                        :foreground ,hitagi-256-background
                                        :inherit bold))))

   `(git-gutter+-modified
     ((,hitagi-class (:background ,hitagi-blue
                                   :foreground ,hitagi-background
                                   :inherit bold))
      (,hitagi-256-class  (:background ,hitagi-256-blue
                                        :foreground ,hitagi-256-background
                                        :inherit bold))))

   `(git-gutter+-unchanged
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-background
                                   :inherit bold))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-background
                                        :inherit bold))))

   `(git-gutter-fr+-added
     ((,hitagi-class (:foreground ,hitagi-green
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :weight bold))))

   `(git-gutter-fr+-deleted
     ((,hitagi-class (:foreground ,hitagi-red
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :weight bold))))

   `(git-gutter-fr+-modified
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :background ,hitagi-highlight-line
                                   :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-blue
                                        :background ,hitagi-256-highlight-line
                                        :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(guide-key/key-face
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(guide-key/prefix-command-face
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,hitagi-class (:weight bold
                               :inherit gnus-group-mail-1-empty))
      (,hitagi-256-class  (:weight bold
                                    :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,hitagi-class (:inherit gnus-group-news-1-empty))
      (,hitagi-256-class  (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,hitagi-class (:weight bold
                               :inherit gnus-group-mail-2-empty))
      (,hitagi-256-class  (:weight bold
                                    :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,hitagi-class (:inherit gnus-group-news-2-empty))
      (,hitagi-256-class  (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,hitagi-class (:weight bold
                               :inherit gnus-group-mail-3-empty))
      (,hitagi-256-class  (:weight bold
                                    :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,hitagi-class (:inherit gnus-group-news-3-empty))
      (,hitagi-256-class  (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,hitagi-class (:weight bold
                               :inherit gnus-group-mail-low-empty))
      (,hitagi-256-class  (:weight bold
                                    :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,hitagi-class (:inherit gnus-group-news-low-empty))
      (,hitagi-256-class  (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,hitagi-class (:weight bold
                               :inherit gnus-group-news-1-empty))
      (,hitagi-256-class  (:weight bold
                                    :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,hitagi-class (:weight bold
                               :inherit gnus-group-news-2-empty))
      (,hitagi-256-class  (:weight bold
                                    :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,hitagi-class (:weight bold
                               :inherit gnus-group-news-3-empty))
      (,hitagi-256-class  (:weight bold
                                    :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,hitagi-class (:weight bold
                               :inherit gnus-group-news-4-empty))
      (,hitagi-256-class  (:weight bold
                                    :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,hitagi-class (:weight bold
                               :inherit gnus-group-news-5-empty))
      (,hitagi-256-class  (:weight bold
                                    :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,hitagi-class (:weight bold
                               :inherit gnus-group-news-6-empty))
      (,hitagi-256-class  (:weight bold
                                    :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,hitagi-class (:weight bold
                               :inherit gnus-group-news-low-empty))
      (,hitagi-256-class  (:weight bold
                                    :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,hitagi-class (:inherit message-header-other))
      (,hitagi-256-class  (:inherit message-header-other))))

   `(gnus-header-from
     ((,hitagi-class (:inherit message-header-other))
      (,hitagi-256-class  (:inherit message-header-other))))

   `(gnus-header-name
     ((,hitagi-class (:inherit message-header-name))
      (,hitagi-256-class  (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,hitagi-class (:inherit message-header-other))
      (,hitagi-256-class  (:inherit message-header-other))))

   `(gnus-header-subject
     ((,hitagi-class (:inherit message-header-subject))
      (,hitagi-256-class  (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(gnus-summary-high-ancient
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :weight bold))))

   `(gnus-summary-high-read
     ((,hitagi-class (:foreground ,hitagi-green
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :weight bold))))

   `(gnus-summary-high-ticked
     ((,hitagi-class (:foreground ,hitagi-orange
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange
                                        :weight bold))))

   `(gnus-summary-high-unread
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :weight bold))))

   `(gnus-summary-low-ancient
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(gnus-summary-low-read
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(gnus-summary-low-ticked
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(gnus-summary-low-unread
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(gnus-summary-normal-ancient
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(gnus-summary-normal-read
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(gnus-summary-normal-ticked
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(gnus-summary-normal-unread
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(gnus-summary-selected
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight bold))))

   `(gnus-cite-1
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(gnus-cite-2
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(gnus-cite-3
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(gnus-cite-4
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(gnus-cite-5
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(gnus-cite-6
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(gnus-cite-7
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(gnus-cite-8
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(gnus-cite-9
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(gnus-cite-10
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(gnus-cite-11
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(gnus-group-news-1-empty
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(gnus-group-news-2-empty
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(gnus-group-news-3-empty
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(gnus-group-news-4-empty
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(gnus-group-news-5-empty
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(gnus-group-news-6-empty
     ((,hitagi-class (:foreground ,hitagi-blue-lc))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue-lc))))

   `(gnus-group-news-low-empty
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(gnus-signature
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(gnus-x-face
     ((,hitagi-class (:background ,hitagi-foreground
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-foreground
                                        :foreground ,hitagi-256-background))))


   ;; helm
   `(helm-apt-deinstalled
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(helm-apt-installed
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(helm-bookmark-directory
     ((,hitagi-class (:inherit helm-ff-directory))
      (,hitagi-256-class  (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(helm-bookmark-gnus
     ((,hitagi-class (:foreground ,hitagi-cyan))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan))))

   `(helm-bookmark-info
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(helm-bookmark-man
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(helm-bookmark-w3m
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(helm-bookmarks-su
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(helm-buffer-file
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(helm-buffer-directory
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(helm-buffer-process
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(helm-buffer-saved-out
     ((,hitagi-class (:foreground ,hitagi-red
                                   :background ,hitagi-background
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :background ,hitagi-256-background
                                        :inverse-video t))))

   `(helm-buffer-size
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(helm-candidate-number
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-emphasis
                                   :bold t))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-emphasis
                                        :bold t))))

   `(helm-ff-directory
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(helm-ff-executable
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(helm-ff-file
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-foreground))))

   `(helm-ff-invalid-symlink
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-orange
                                   :slant italic))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-orange
                                        :slant italic))))

   `(helm-ff-prefix
     ((,hitagi-class (:background ,hitagi-green
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-green
                                        :foreground ,hitagi-256-background))))

   `(helm-ff-symlink
     ((,hitagi-class (:foreground ,hitagi-cyan))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan))))

   `(helm-grep-file
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :underline t))))

   `(helm-grep-finish
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(helm-grep-lineno
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(helm-grep-match
     ((,hitagi-class (:inherit helm-match)))
     ((,hitagi-256-class  (:inherit helm-match))))

   `(helm-grep-running
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(helm-header
     ((,hitagi-class (:inherit header-line))
      (,hitagi-256-class  (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(helm-lisp-show-completion
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :background ,hitagi-highlight-line
                                   :bold t))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :background ,hitagi-256-highlight-line
                                        :bold t))))

   `(helm-M-x-key
     ((,hitagi-class (:foreground ,hitagi-orange
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange
                                        :underline t))))

   `(helm-moccur-buffer
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :underline t))))

   `(helm-match
     ((,hitagi-class (:foreground ,hitagi-green :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-green :inherit bold))))

   `(helm-match-item
     ((,hitagi-class (:inherit helm-match))
      (,hitagi-256-class  (:inherit helm-match))))

   `(helm-selection
     ((,hitagi-class (:background ,hitagi-highlight
                                   :inherit bold
                                   :underline nil))
      (,hitagi-256-class  (:background ,hitagi-256-highlight
                                        :inherit bold
                                        :underline nil))))

   `(helm-selection-line
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-emphasis
                                   :underline nil))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-emphasis
                                        :underline nil))))

   `(helm-separator
     ((,hitagi-class (:foreground ,hitagi-gray))
      (,hitagi-256-class  (:foreground ,hitagi-256-gray))))

   `(helm-source-header
     ((,hitagi-class (:background ,hitagi-violet-l
                                   :foreground ,hitagi-background
                                   :underline nil))
      (,hitagi-256-class  (:background ,hitagi-256-violet-l
                                        :foreground ,hitagi-256-background
                                        :underline nil))))

   `(helm-swoop-target-line-face
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   `(helm-swoop-target-word-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(helm-time-zone-current
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(helm-time-zone-home
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(helm-visible-mark
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-magenta :bold t))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,hitagi-class :foreground ,hitagi-blue)
      (,hitagi-256-class  :foreground ,hitagi-256-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,hitagi-class :foreground ,hitagi-blue-l)
      (,hitagi-256-class  :foreground ,hitagi-256-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,hitagi-class :foreground ,hitagi-blue-l)
      (,hitagi-256-class  :foreground ,hitagi-256-blue-l)))

   `(helm-ls-git-untracked-face
     ((,hitagi-class :foreground ,hitagi-orange)
      (,hitagi-256-class  :foreground ,hitagi-256-orange)))

   `(helm-ls-git-added-copied-face
     ((,hitagi-class :foreground ,hitagi-green)
      (,hitagi-256-class  :foreground ,hitagi-256-green)))

   `(helm-ls-git-added-modified-face
     ((,hitagi-class :foreground ,hitagi-green-l)
      (,hitagi-256-class  :foreground ,hitagi-256-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,hitagi-class :foreground ,hitagi-red)
      (,hitagi-256-class  :foreground ,hitagi-256-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,hitagi-class :foreground ,hitagi-red-l)
      (,hitagi-256-class  :foreground ,hitagi-256-red-l)))

   `(helm-ls-git-conflict-face
     ((,hitagi-class :foreground ,hitagi-yellow)
      (,hitagi-256-class  :foreground ,hitagi-256-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,hitagi-class (:foreground ,hitagi-yellow-lc
                                   :background ,hitagi-yellow-hc))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow-lc
                                        :background ,hitagi-256-yellow-hc))))

   `(hi-pink
     ((,hitagi-class (:foreground ,hitagi-magenta-lc
                                   :background ,hitagi-magenta-hc))
      (,hitagi-256-class  (:foreground ,hitagi-256-magenta-lc
                                        :background ,hitagi-256-magenta-hc))))

   `(hi-green
     ((,hitagi-class (:foreground ,hitagi-green-lc
                                   :background ,hitagi-green-hc))
      (,hitagi-256-class  (:foreground ,hitagi-256-green-lc
                                        :background ,hitagi-256-green-hc))))

   `(hi-blue
     ((,hitagi-class (:foreground ,hitagi-blue-lc
                                   :background ,hitagi-blue-hc))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue-lc
                                        :background ,hitagi-256-blue-hc))))

   `(hi-black-b
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :background ,hitagi-background
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :background ,hitagi-256-background
                                        :weight bold))))

   `(hi-blue-b
     ((,hitagi-class (:foreground ,hitagi-blue-lc
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue-lc
                                        :weight bold))))

   `(hi-green-b
     ((,hitagi-class (:foreground ,hitagi-green-lc
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-green-lc
                                        :weight bold))))

   `(hi-red-b
     ((,hitagi-class (:foreground ,hitagi-red
                                   :weight bold))))

   `(hi-black-hb
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :background ,hitagi-background
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :background ,hitagi-256-background
                                        :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(highlight-changes-delete
     ((,hitagi-class (:foreground ,hitagi-red
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,hitagi-class (:background ,hitagi-gray))
      (,hitagi-256-class  (:background ,hitagi-256-gray))))

   `(highlight-indentation-current-column-face
     ((,hitagi-class (:background ,hitagi-gray))
      (,hitagi-256-class  (:background ,hitagi-256-gray))))

   ;; highlight-symbol
   `(highlight-symbol-face
     ((,hitagi-class (:background ,hitagi-highlight))
      (,hitagi-256-class  (:background ,hitagi-256-highlight))))

   ;; hl-line-mode
   `(hl-line
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   `(hl-line-face
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight normal))))

   `(ido-only-match
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-yellow
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-yellow
                                        :weight normal))))

   `(ido-subdir
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(ido-incomplete-regexp
     ((,hitagi-class (:foreground ,hitagi-red
                                   :weight bold ))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :weight bold ))))

   `(ido-indicator
     ((,hitagi-class (:background ,hitagi-red
                                   :foreground ,hitagi-background
                                   :width condensed))
      (,hitagi-256-class  (:background ,hitagi-256-red
                                        :foreground ,hitagi-256-background
                                        :width condensed))))

   `(ido-virtual
     ((,hitagi-class (:foreground ,hitagi-cyan))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan))))

   ;; info
   `(info-header-xref
     ((,hitagi-class (:foreground ,hitagi-green
                                   :inherit bold
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :inherit bold
                                        :underline t))))

   `(info-menu
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(info-node
     ((,hitagi-class (:foreground ,hitagi-violet
                                   :inherit bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet
                                        :inherit bold))))

   `(info-quoted-name
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(info-reference-item
     ((,hitagi-class (:background nil
                                   :underline t
                                   :inherit bold))
      (,hitagi-256-class  (:background nil
                                        :underline t
                                        :inherit bold))))

   `(info-string
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(info-title-1
     ((,hitagi-class (:height ,hitagi-height-plus-4))
      (,hitagi-256-class  (:height ,hitagi-height-plus-4))))

   `(info-title-2
     ((,hitagi-class (:height ,hitagi-height-plus-3))
      (,hitagi-256-class  (:height ,hitagi-height-plus-3))))

   `(info-title-3
     ((,hitagi-class (:height ,hitagi-height-plus-2))
      (,hitagi-256-class  (:height ,hitagi-height-plus-2))))

   `(info-title-4
     ((,hitagi-class (:height ,hitagi-height-plus-1))
      (,hitagi-256-class  (:height ,hitagi-height-plus-1))))

   ;; ivy
   `(ivy-current-match
     ((,hitagi-class (:background ,hitagi-gray :inherit bold))
      (,hitagi-256-class  (:background ,hitagi-gray-l :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,hitagi-class (:inherit bold))
      (,hitagi-256-class  (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,hitagi-class (:foreground ,hitagi-violet
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet
                                        :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,hitagi-class (:foreground ,hitagi-green
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :underline t))))

   `(ivy-remote
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(swiper-line-face
     ((,hitagi-class (:background ,hitagi-highlight-line))))

   `(swiper-match-face-1
     ((,hitagi-class (:background ,hitagi-gray-d))))

   `(swiper-match-face-2
     ((,hitagi-class (:background ,hitagi-green))))

   `(swiper-match-face-3
     ((,hitagi-class (:background ,hitagi-orange))))

   `(swiper-match-face-4
     ((,hitagi-class (:background ,hitagi-magenta))))

   ;; jabber
   `(jabber-activity-face
     ((,hitagi-class (:weight bold
                               :foreground ,hitagi-red))
      (,hitagi-256-class  (:weight bold
                                    :foreground ,hitagi-256-red))))

   `(jabber-activity-personal-face
     ((,hitagi-class (:weight bold
                               :foreground ,hitagi-blue))
      (,hitagi-256-class  (:weight bold
                                    :foreground ,hitagi-256-blue))))

   `(jabber-chat-error
     ((,hitagi-class (:weight bold
                               :foreground ,hitagi-red))
      (,hitagi-256-class  (:weight bold
                                    :foreground ,hitagi-256-red))))

   `(jabber-chat-prompt-foreign
     ((,hitagi-class (:weight bold
                               :foreground ,hitagi-red))
      (,hitagi-256-class  (:weight bold
                                    :foreground ,hitagi-256-red))))

   `(jabber-chat-prompt-local
     ((,hitagi-class (:weight bold
                               :foreground ,hitagi-blue))
      (,hitagi-256-class  (:weight bold
                                    :foreground ,hitagi-256-blue))))

   `(jabber-chat-prompt-system
     ((,hitagi-class (:weight bold
                               :foreground ,hitagi-green))
      (,hitagi-256-class  (:weight bold
                                    :foreground ,hitagi-256-green))))

   `(jabber-chat-text-foreign
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(jabber-chat-text-local
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(jabber-chat-rare-time-face
     ((,hitagi-class (:underline t
                                  :foreground ,hitagi-green))
      (,hitagi-256-class  (:underline t
                                       :foreground ,hitagi-256-green))))

   `(jabber-roster-user-away
     ((,hitagi-class (:slant italic
                              :foreground ,hitagi-green))
      (,hitagi-256-class  (:slant italic
                                   :foreground ,hitagi-256-green))))

   `(jabber-roster-user-chatty
     ((,hitagi-class (:weight bold
                               :foreground ,hitagi-orange))
      (,hitagi-256-class  (:weight bold
                                    :foreground ,hitagi-256-orange))))

   `(jabber-roster-user-dnd
     ((,hitagi-class (:slant italic
                              :foreground ,hitagi-red))
      (,hitagi-256-class  (:slant italic
                                   :foreground ,hitagi-256-red))))

   `(jabber-roster-user-error
     ((,hitagi-class (:weight light
                               :slant italic
                               :foreground ,hitagi-red))
      (,hitagi-256-class  (:weight light
                                    :slant italic
                                    :foreground ,hitagi-256-red))))

   `(jabber-roster-user-offline
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(jabber-roster-user-online
     ((,hitagi-class (:weight bold
                               :foreground ,hitagi-blue))
      (,hitagi-256-class  (:weight bold
                                    :foreground ,hitagi-256-blue))))

   `(jabber-roster-user-xa
     ((,hitagi-class (:slant italic
                              :foreground ,hitagi-magenta))
      (,hitagi-256-class  (:slant italic
                                   :foreground ,hitagi-256-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(js2-external-variable
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(js2-function-call
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(js2-function-param
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(js2-instance-member
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(js2-jsdoc-html-tag-delimiter
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(js2-jsdoc-html-tag-name
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(js2-jsdoc-tag
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(js2-jsdoc-type
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(js2-jsdoc-value
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(js2-magic-paren
     ((,hitagi-class (:underline t))
      (,hitagi-256-class  (:underline t))))

   `(js2-object-property
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(js2-private-function-call
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(js2-private-member
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(js2-warning
     ((,hitagi-class (:underline ,hitagi-orange))
      (,hitagi-256-class  (:underline ,hitagi-256-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,hitagi-class (:inherit bold))
      (,hitagi-256-class  (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,hitagi-class (:foreground ,hitagi-line-number
                                   :background ,hitagi-fringe-bg
                                   :inherit default
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-line-number
                                        :background ,hitagi-256-fringe-bg
                                        :inherit default
                                        :underline nil))))

   ;; linum-relative-current-face
   `(linum-relative-current-face
     ((,hitagi-class (:foreground ,hitagi-line-number
                                   :background ,hitagi-highlight-line
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-line-number
                                        :background ,hitagi-256-highlight-line
                                        :underline nil))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,hitagi-class (:inherit dihitagi-red-directory))
      (,hitagi-256-class  (:inherit dihitagi-red-directory))))

   `(lusty-file-face
     ((,hitagi-class nil)
      (,hitagi-256-class  nil)))

   `(lusty-match-face
     ((,hitagi-class (:inherit ido-first-match))
      (,hitagi-256-class  (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :weight bold))))

   ;; magit
   ;;
   ;; TODO: Add supports for all magit faces
   ;; https://github.com/magit/magit/search?utf8=%E2%9C%93&q=face
   ;;
   `(magit-diff-added
     ((,hitagi-class (:foreground ,hitagi-green
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :background ,hitagi-256-background))))

   `(magit-diff-added-highlight
     ((,hitagi-class (:foreground ,hitagi-green
                                   :background ,hitagi-highlight-line))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :background ,hitagi-256-highlight-line))))

   `(magit-diff-removed
     ((,hitagi-class (:foreground ,hitagi-red
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :background ,hitagi-256-background))))

   `(magit-diff-removed-highlight
     ((,hitagi-class (:foreground ,hitagi-red
                                   :background ,hitagi-highlight-line))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :background ,hitagi-256-highlight-line))))

   `(magit-section-title
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight bold))))

   `(magit-branch
     ((,hitagi-class (:foreground ,hitagi-orange
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange
                                        :weight bold))))

   `(magit-item-highlight
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :weight unspecified))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :weight unspecified))))

   `(magit-log-author
     ((,hitagi-class (:foreground ,hitagi-cyan))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan))))

   `(magit-log-graph
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(magit-log-head-label-bisect-bad
     ((,hitagi-class (:background ,hitagi-red-hc
                                   :foreground ,hitagi-red-lc
                                   :box 1))
      (,hitagi-256-class  (:background ,hitagi-256-red-hc
                                        :foreground ,hitagi-256-red-lc
                                        :box 1))))

   `(magit-log-head-label-bisect-good
     ((,hitagi-class (:background ,hitagi-green-hc
                                   :foreground ,hitagi-green-lc
                                   :box 1))
      (,hitagi-256-class  (:background ,hitagi-256-green-hc
                                        :foreground ,hitagi-256-green-lc
                                        :box 1))))

   `(magit-log-head-label-default
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :box 1))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :box 1))))

   `(magit-log-head-label-local
     ((,hitagi-class (:background ,hitagi-blue-lc
                                   :foreground ,hitagi-blue-hc
                                   :box 1))
      (,hitagi-256-class  (:background ,hitagi-256-blue-lc
                                        :foreground ,hitagi-256-blue-hc
                                        :box 1))))

   `(magit-log-head-label-patches
     ((,hitagi-class (:background ,hitagi-red-lc
                                   :foreground ,hitagi-red-hc
                                   :box 1))
      (,hitagi-256-class  (:background ,hitagi-256-red-lc
                                        :foreground ,hitagi-256-red-hc
                                        :box 1))))

   `(magit-log-head-label-remote
     ((,hitagi-class (:background ,hitagi-green-lc
                                   :foreground ,hitagi-green-hc
                                   :box 1))
      (,hitagi-256-class  (:background ,hitagi-256-green-lc
                                        :foreground ,hitagi-256-green-hc
                                        :box 1))))

   `(magit-log-head-label-tags
     ((,hitagi-class (:background ,hitagi-yellow-lc
                                   :foreground ,hitagi-yellow-hc
                                   :box 1))
      (,hitagi-256-class  (:background ,hitagi-256-yellow-lc
                                        :foreground ,hitagi-256-yellow-hc
                                        :box 1))))

   `(magit-log-sha1
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   ;; man
   `(Man-overstrike
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :weight bold))))

   `(Man-reverse
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(Man-underline
     ((,hitagi-class (:foreground ,hitagi-green :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight bold))))

   `(monky-diff-add
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(monky-diff-del
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(markdown-header-face-1
     ((,hitagi-class (:inherit markdown-header-face
                                :height ,hitagi-height-plus-4))
      (,hitagi-256-class  (:inherit markdown-header-face
                                     :height ,hitagi-height-plus-4))))

   `(markdown-header-face-2
     ((,hitagi-class (:inherit markdown-header-face
                                :height ,hitagi-height-plus-3))
      (,hitagi-256-class  (:inherit markdown-header-face
                                     :height ,hitagi-height-plus-3))))

   `(markdown-header-face-3
     ((,hitagi-class (:inherit markdown-header-face
                                :height ,hitagi-height-plus-2))
      (,hitagi-256-class  (:inherit markdown-header-face
                                     :height ,hitagi-height-plus-2))))

   `(markdown-header-face-4
     ((,hitagi-class (:inherit markdown-header-face
                                :height ,hitagi-height-plus-1))
      (,hitagi-256-class  (:inherit markdown-header-face
                                     :height ,hitagi-height-plus-1))))

   `(markdown-header-face-5
     ((,hitagi-class (:inherit markdown-header-face))
      (,hitagi-256-class  (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,hitagi-class (:inherit markdown-header-face))
      (,hitagi-256-class  (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(message-header-name
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(message-header-other
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :weight normal))))

   `(message-header-to
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :weight normal))))

   `(message-header-cc
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :weight normal))))

   `(message-header-newsgroups
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight bold))))

   `(message-header-subject
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :weight normal))))

   `(message-header-xheader
     ((,hitagi-class (:foreground ,hitagi-cyan))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan))))

   `(message-mml
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight bold))))

   `(message-separator
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :slant italic))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(mew-face-header-from
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(mew-face-header-date
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(mew-face-header-to
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(mew-face-header-key
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(mew-face-header-private
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(mew-face-header-important
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(mew-face-header-marginal
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :weight bold))))

   `(mew-face-header-warning
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(mew-face-header-xmew
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(mew-face-header-xmew-bad
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(mew-face-body-url
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(mew-face-body-comment
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :slant italic))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :slant italic))))

   `(mew-face-body-cite1
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(mew-face-body-cite2
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(mew-face-body-cite3
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(mew-face-body-cite4
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(mew-face-body-cite5
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(mew-face-mark-review
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(mew-face-mark-escape
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(mew-face-mark-delete
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(mew-face-mark-unlink
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(mew-face-mark-refile
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(mew-face-mark-unread
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(mew-face-eof-message
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(mew-face-eof-part
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(mingus-pausing-face
     ((,hitagi-class (:foreground ,hitagi-magenta))
      (,hitagi-256-class  (:foreground ,hitagi-256-magenta))))

   `(mingus-playing-face
     ((,hitagi-class (:foreground ,hitagi-cyan))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan))))

   `(mingus-playlist-face
     ((,hitagi-class (:foreground ,hitagi-cyan ))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan ))))

   `(mingus-song-file-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(mingus-stopped-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,hitagi-class (:background ,hitagi-violet-d))
      (,hitagi-256-class  (:background ,hitagi-256-violet-d))))

   `(mmm-cleanup-submode-face
     ((,hitagi-class (:background ,hitagi-orange-d))
      (,hitagi-256-class  (:background ,hitagi-256-orange-d))))

   `(mmm-declaration-submode-face
     ((,hitagi-class (:background ,hitagi-cyan-d))
      (,hitagi-256-class  (:background ,hitagi-256-cyan-d))))

   `(mmm-comment-submode-face
     ((,hitagi-class (:background ,hitagi-blue-d))
      (,hitagi-256-class  (:background ,hitagi-256-blue-d))))

   `(mmm-output-submode-face
     ((,hitagi-class (:background ,hitagi-red-d))
      (,hitagi-256-class  (:background ,hitagi-256-red-d))))

   `(mmm-special-submode-face
     ((,hitagi-class (:background ,hitagi-green-d))
      (,hitagi-256-class  (:background ,hitagi-256-green-d))))

   `(mmm-code-submode-face
     ((,hitagi-class (:background ,hitagi-gray))
      (,hitagi-256-class  (:background ,hitagi-256-gray))))

   `(mmm-default-submode-face
     ((,hitagi-class (:background ,hitagi-gray-d))
      (,hitagi-256-class  (:background ,hitagi-256-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,hitagi-class (:underline t))
      (,hitagi-256-class  (:underline t))))

   `(moccur-edit-done-face
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :background ,hitagi-background
                                   :slant italic))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :background ,hitagi-256-background
                                        :slant italic))))

   `(moccur-edit-face
     ((,hitagi-class (:background ,hitagi-yellow
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-yellow
                                        :foreground ,hitagi-256-background))))

   `(moccur-edit-file-face
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   `(moccur-edit-reject-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(moccur-face
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-emphasis
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-emphasis
                                        :weight bold))))

   `(search-buffers-face
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-emphasis
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-emphasis
                                        :weight bold))))

   `(search-buffers-header-face
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-yellow
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-yellow
                                        :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,hitagi-class (:foreground ,hitagi-green
                                   :slant italic
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-2-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :slant italic
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-3-face
     ((,hitagi-class (:foreground ,hitagi-orange
                                   :slant italic
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-4-face
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :slant italic
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-5-face
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :slant italic
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-6-face
     ((,hitagi-class (:foreground ,hitagi-green
                                   :slant italic
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-7-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :slant italic
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-flagged-face
     ((,hitagi-class (:foreground ,hitagi-magenta
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-magenta
                                        :weight bold))))

   `(mu4e-view-url-number-face
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight normal))))

   `(mu4e-warning-face
     ((,hitagi-class (:foreground ,hitagi-red
                                   :slant normal
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :slant normal
                                        :weight bold))))

   `(mu4e-header-highlight-face
     ((,hitagi-class (:inherit unspecified
                                :foreground unspecified
                                :background ,hitagi-highlight-line
                                :underline ,hitagi-emphasis
                                :weight normal))
      (,hitagi-256-class  (:inherit unspecified
                                     :foreground unspecified
                                     :background ,hitagi-256-highlight-line
                                     :underline ,hitagi-256-emphasis
                                     :weight normal))))


   `(mu4e-draft-face
     ((,hitagi-class (:inherit font-lock-string-face))
      (,hitagi-256-class  (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,hitagi-class (:inherit font-lock-comment-face))
      (,hitagi-256-class  (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,hitagi-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,hitagi-256-class  (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-header-face
     ((,hitagi-class (:inherit default))
      (,hitagi-256-class  (:inherit default))))

   `(mu4e-header-marks-face
     ((,hitagi-class (:inherit font-lock-preprocessor-face))
      (,hitagi-256-class  (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,hitagi-class (:inherit font-lock-type-face))
      (,hitagi-256-class  (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,hitagi-class (:inherit font-lock-pseudo-keyword-face
                                :weight bold))
      (,hitagi-256-class  (:inherit font-lock-pseudo-keyword-face
                                     :weight bold))))

   `(mu4e-moved-face
     ((,hitagi-class (:inherit font-lock-comment-face
                                :slant italic))
      (,hitagi-256-class  (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-ok-face
     ((,hitagi-class (:inherit font-lock-comment-face
                                :slant normal
                                :weight bold))
      (,hitagi-256-class  (:inherit font-lock-comment-face
                                     :slant normal
                                     :weight bold))))

   `(mu4e-replied-face
     ((,hitagi-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,hitagi-256-class  (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-system-face
     ((,hitagi-class (:inherit font-lock-comment-face
                                :slant italic))
      (,hitagi-256-class  (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-title-face
     ((,hitagi-class (:inherit font-lock-type-face
                                :weight bold))
      (,hitagi-256-class  (:inherit font-lock-type-face
                                     :weight bold))))

   `(mu4e-trashed-face
     ((,hitagi-class (:inherit font-lock-comment-face
                                :strike-through t))
      (,hitagi-256-class  (:inherit font-lock-comment-face
                                     :strike-through t))))

   `(mu4e-unread-face
     ((,hitagi-class (:inherit font-lock-keyword-face
                                :weight bold))
      (,hitagi-256-class  (:inherit font-lock-keyword-face
                                     :weight bold))))

   `(mu4e-view-attach-number-face
     ((,hitagi-class (:inherit font-lock-variable-name-face
                                :weight bold))
      (,hitagi-256-class  (:inherit font-lock-variable-name-face
                                     :weight bold))))

   `(mu4e-view-contact-face
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :weight normal))))

   `(mu4e-view-header-key-face
     ((,hitagi-class (:inherit message-header-name
                                :weight normal))
      (,hitagi-256-class  (:inherit message-header-name
                                     :weight normal))))

   `(mu4e-view-header-value-face
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :weight normal
                                   :slant normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :weight normal
                                        :slant normal))))

   `(mu4e-view-link-face
     ((,hitagi-class (:inherit link))
      (,hitagi-256-class  (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :weight normal
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(nav-face-button-num
     ((,hitagi-class (:foreground ,hitagi-cyan))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan))))

   `(nav-face-dir
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(nav-face-hdir
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(nav-face-file
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(nav-face-hfile
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :background ,hitagi-background
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :background ,hitagi-256-background
                                        :weight bold))))


   `(neo-header-face
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :background ,hitagi-256-background))))

   `(neo-root-dir-face
     ((,hitagi-class (:foreground ,hitagi-green
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :background ,hitagi-256-background))))

   `(neo-dir-link-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :background ,hitagi-256-background))))

   `(neo-file-link-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(neo-button-face
     ((,hitagi-class (:underline nil))
      (,hitagi-256-class  (:underline nil))))

   `(neo-expand-btn-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(neo-vc-default-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(neo-vc-user-face
     ((,hitagi-class (:foreground ,hitagi-red
                                   :slant italic))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :slant italic))))

   `(neo-vc-up-to-date-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(neo-vc-edited-face
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(neo-vc-needs-update-face
     ((,hitagi-class (:underline t))
      (,hitagi-256-class  (:underline t))))

   `(neo-vc-needs-merge-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(neo-vc-unlocked-changes-face
     ((,hitagi-class (:foreground ,hitagi-red
                                   :background ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :background ,hitagi-256-comments))))

   `(neo-vc-added-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(neo-vc-removed-face
     ((,hitagi-class (:strike-through t))
      (,hitagi-256-class  (:strike-through t))))

   `(neo-vc-conflict-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(neo-vc-missing-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(neo-vc-ignored-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face
     ((,hitagi-class (:foreground ,hitagi-gray-l))
      (,hitagi-256-class  (:foreground ,hitagi-256-gray-l))))

   `(markup-table-face
     ((,hitagi-class (:foreground ,hitagi-blue-hc
                                   :background ,hitagi-blue-lc))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue-hc
                                        :background ,hitagi-256-blue-lc))))

   `(markup-verbatim-face
     ((,hitagi-class (:background ,hitagi-orange-lc))
      (,hitagi-256-class  (:background ,hitagi-256-orange-lc))))

   `(markup-list-face
     ((,hitagi-class (:foreground ,hitagi-violet-hc
                                   :background ,hitagi-violet-lc))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet-hc
                                        :background ,hitagi-256-violet-lc))))

   `(markup-replacement-face
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(markup-complex-replacement-face
     ((,hitagi-class (:foreground ,hitagi-violet-hc
                                   :background ,hitagi-violet-lc))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet-hc
                                        :background ,hitagi-256-violet-lc))))

   `(markup-gen-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(markup-secondary-text-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   ;; org-mode
   `(org-agenda-structure
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :background ,hitagi-highlight-line
                                   :weight bold
                                   :slant normal
                                   :inverse-video nil
                                   :height ,hitagi-height-plus-1
                                   :underline nil
                                   :box (:line-width 2 :color ,hitagi-background)))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :background ,hitagi-256-highlight-line
                                        :weight bold
                                        :slant normal
                                        :inverse-video nil
                                        :height ,hitagi-height-plus-1
                                        :underline nil
                                        :box (:line-width 2 :color ,hitagi-256-background)))))

   `(org-agenda-calendar-event
     ((,hitagi-class (:foreground ,hitagi-emphasis))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis))))

   `(org-agenda-calendar-sexp
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :slant italic))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :slant italic))))

   `(org-agenda-date
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :background ,hitagi-background
                                   :weight normal
                                   :inverse-video nil
                                   :overline nil
                                   :slant normal
                                   :height 1.0
                                   :box (:line-width 2 :color ,hitagi-background)))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :background ,hitagi-256-background
                                        :weight normal
                                        :inverse-video nil
                                        :overline nil
                                        :slant normal
                                        :height 1.0
                                        :box (:line-width 2 :color ,hitagi-256-background)))) t)

   `(org-agenda-date-weekend
     ((,hitagi-class (:inherit org-agenda-date
                                :inverse-video nil
                                :background unspecified
                                :foreground ,hitagi-comments
                                :weight unspecified
                                :underline t
                                :overline nil
                                :box unspecified))
      (,hitagi-256-class  (:inherit org-agenda-date
                                     :inverse-video nil
                                     :background unspecified
                                     :foreground ,hitagi-256-comments
                                     :weight unspecified
                                     :underline t
                                     :overline nil
                                     :box unspecified))) t)

   `(org-agenda-date-today
     ((,hitagi-class (:inherit org-agenda-date
                                :inverse-video t
                                :weight bold
                                :underline unspecified
                                :overline nil
                                :box unspecified
                                :foreground ,hitagi-blue
                                :background ,hitagi-background))
      (,hitagi-256-class  (:inherit org-agenda-date
                                     :inverse-video t
                                     :weight bold
                                     :underline unspecified
                                     :overline nil
                                     :box unspecified
                                     :foreground ,hitagi-256-blue
                                     :background ,hitagi-256-background))) t)

   `(org-agenda-done
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :slant italic))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :slant italic))) t)

   `(org-archived
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :weight normal))))

   `(org-block
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :background ,hitagi-highlight-alt))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :background ,hitagi-256-highlight-alt))))

   `(org-block-background
     ((,hitagi-class (:background ,hitagi-highlight-alt))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-alt))))

   `(org-block-begin-line
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :background ,hitagi-gray-d
                                   :slant italic))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :background ,hitagi-256-gray-d
                                        :slant italic))))

   `(org-block-end-line
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :background ,hitagi-gray-d
                                   :slant italic))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :background ,hitagi-256-gray-d
                                        :slant italic))))

   `(org-checkbox
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-foreground
                                   :box (:line-width 1 :style released-button)))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-foreground
                                        :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(org-date
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :underline t))))

   `(org-done
     ((,hitagi-class (:weight bold
                               :foreground ,hitagi-green))
      (,hitagi-256-class  (:weight bold
                                    :foreground ,hitagi-256-green))))

   `(org-ellipsis
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(org-formula
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(org-headline-done
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(org-hide
     ((,hitagi-class (:foreground ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-background))))

   `(org-level-1
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :height ,hitagi-height-plus-4
                                :foreground ,hitagi-orange))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :height ,hitagi-height-plus-4
                                     :foreground ,hitagi-256-orange))))

   `(org-level-2
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :height ,hitagi-height-plus-3
                                :foreground ,hitagi-green))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :height ,hitagi-height-plus-3
                                     :foreground ,hitagi-256-green))))

   `(org-level-3
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :height ,hitagi-height-plus-2
                                :foreground ,hitagi-blue))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :height ,hitagi-height-plus-2
                                     :foreground ,hitagi-256-blue))))

   `(org-level-4
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :height ,hitagi-height-plus-1
                                :foreground ,hitagi-yellow))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :height ,hitagi-height-plus-1
                                     :foreground ,hitagi-256-yellow))))

   `(org-level-5
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-cyan))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-cyan))))

   `(org-level-6
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-green))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-green))))

   `(org-level-7
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-red))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-red))))

   `(org-level-8
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-blue))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-blue))))

   `(org-link
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :underline t))))

   `(org-sexp-date
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(org-scheduled
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(org-scheduled-previously
     ((,hitagi-class (:foreground ,hitagi-cyan))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan))))

   `(org-scheduled-today
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :weight normal))))

   `(org-special-keyword
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :weight bold))))

   `(org-table
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(org-tag
     ((,hitagi-class (:weight bold))
      (,hitagi-256-class  (:weight bold))))

   `(org-time-grid
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(org-todo
     ((,hitagi-class (:foreground ,hitagi-red
                                   :weight bold)))
     ((,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :weight bold))))

   `(org-upcoming-deadline
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight normal
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(org-warning
     ((,hitagi-class (:foreground ,hitagi-orange
                                   :weight normal
                                   :underline nil))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange
                                        :weight normal
                                        :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,hitagi-class (:background ,hitagi-blue-lc
                                   :foreground ,hitagi-blue-hc))
      (,hitagi-256-class  (:background ,hitagi-256-blue-lc
                                        :foreground ,hitagi-256-blue-hc))))

   `(org-habit-clear-future-face
     ((,hitagi-class (:background ,hitagi-blue-lc))
      (,hitagi-256-class  (:background ,hitagi-256-blue-lc))))

   `(org-habit-ready-face
     ((,hitagi-class (:background ,hitagi-green-lc
                                   :foreground ,hitagi-green))
      (,hitagi-256-class  (:background ,hitagi-256-green-lc
                                        :foreground ,hitagi-256-green))))

   `(org-habit-ready-future-face
     ((,hitagi-class (:background ,hitagi-green-lc))
      (,hitagi-256-class  (:background ,hitagi-256-green-lc))))

   `(org-habit-alert-face
     ((,hitagi-class (:background ,hitagi-yellow
                                   :foreground ,hitagi-yellow-lc))
      (,hitagi-256-class  (:background ,hitagi-256-yellow
                                        :foreground ,hitagi-256-yellow-lc))))

   `(org-habit-alert-future-face
     ((,hitagi-class (:background ,hitagi-yellow-lc))
      (,hitagi-256-class  (:background ,hitagi-256-yellow-lc))))

   `(org-habit-overdue-face
     ((,hitagi-class (:background ,hitagi-red
                                   :foreground ,hitagi-red-lc))
      (,hitagi-256-class  (:background ,hitagi-256-red
                                        :foreground ,hitagi-256-red-lc))))

   `(org-habit-overdue-future-face
     ((,hitagi-class (:background ,hitagi-red-lc))
      (,hitagi-256-class  (:background ,hitagi-256-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(org-agenda-restriction-lock
     ((,hitagi-class (:background ,hitagi-yellow))
      (,hitagi-256-class  (:background ,hitagi-256-yellow))))

   `(org-clock-overlay
     ((,hitagi-class (:background ,hitagi-yellow))
      (,hitagi-256-class  (:background ,hitagi-256-yellow))))

   `(org-column
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :strike-through nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :inherit default))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :strike-through nil
                                        :underline nil
                                        :slant normal
                                        :weight normal
                                        :inherit default))))

   `(org-column-title
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :underline t
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :underline t
                                        :weight bold))))

   `(org-date-selected
     ((,hitagi-class (:foreground ,hitagi-red
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :inverse-video t))))

   `(org-document-info
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(org-document-title
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :weight bold
                                   :height ,hitagi-height-plus-4))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :weight bold
                                        :height ,hitagi-height-plus-4))))

   `(org-drawer
     ((,hitagi-class (:foreground ,hitagi-cyan))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan))))

   `(org-footnote
     ((,hitagi-class (:foreground ,hitagi-magenta
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-magenta
                                        :underline t))))

   `(org-latex-and-export-specials
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(org-mode-line-clock-overrun
     ((,hitagi-class (:inherit mode-line))
      (,hitagi-256-class  (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,hitagi-class (:inherit org-level-1))
      (,hitagi-256-class  (:inherit org-level-1))))

   `(outline-2
     ((,hitagi-class (:inherit org-level-2))
      (,hitagi-256-class  (:inherit org-level-2))))

   `(outline-3
     ((,hitagi-class (:inherit org-level-3))
      (,hitagi-256-class  (:inherit org-level-3))))

   `(outline-4
     ((,hitagi-class (:inherit org-level-4))
      (,hitagi-256-class  (:inherit org-level-4))))

   `(outline-5
     ((,hitagi-class (:inherit org-level-5))
      (,hitagi-256-class  (:inherit org-level-5))))

   `(outline-6
     ((,hitagi-class (:inherit org-level-6))
      (,hitagi-256-class  (:inherit org-level-6))))

   `(outline-7
     ((,hitagi-class (:inherit org-level-7))
      (,hitagi-256-class  (:inherit org-level-7))))

   `(outline-8
     ((,hitagi-class (:inherit org-level-8))
      (,hitagi-256-class  (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,hitagi-256-class  (:foreground ,hitagi-comments))))

   ;; perspective
   `(persp-selected-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight normal))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight normal))))

   ;; popup
   `(popup-face
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-foreground))))

   `(popup-isearch-match
     ((,hitagi-class (:background ,hitagi-green))
      (,hitagi-256-class  (:background ,hitagi-256-green))))

   `(popup-menu-face
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-foreground))))

   `(popup-menu-mouse-face
     ((,hitagi-class (:background ,hitagi-blue
                                   :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:background ,hitagi-256-blue
                                        :foreground ,hitagi-256-foreground))))

   `(popup-menu-selection-face
     ((,hitagi-class (:background ,hitagi-magenta
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-magenta
                                        :foreground ,hitagi-256-background))))

   `(popup-scroll-bar-background-face
     ((,hitagi-class (:background ,hitagi-comments))
      (,hitagi-256-class  (:background ,hitagi-256-comments))))

   `(popup-scroll-bar-foreground-face
     ((,hitagi-class (:background ,hitagi-emphasis))
      (,hitagi-256-class  (:background ,hitagi-256-emphasis))))

   `(popup-tip-face
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(rainbow-delimiters-depth-4-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(rainbow-delimiters-depth-7-face
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(rainbow-delimiters-depth-10-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(rainbow-delimiters-unmatched-face
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :background ,hitagi-background
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :background ,hitagi-256-background
                                        :inverse-video t))))

   ;; realgud
   `(realgud-overlay-arrow1
     ((,hitagi-class (:foreground ,hitagi-green-d))
      (,hitagi-256-class  (:foreground ,hitagi-256-green-d))))

   `(realgud-overlay-arrow2
     ((,hitagi-class (:foreground ,hitagi-yellow-d))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow-d))))

   `(realgud-overlay-arrow3
     ((,hitagi-class (:foreground ,hitagi-orange-d))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange-d))))

   `(realgud-bp-enabled-face
     ((,hitagi-class (:inherit error)))
     ((,hitagi-256-class (:inherit error))))

   `(realgud-bp-disabled-face
     ((,hitagi-class (:inherit secondary-selection)))
     ((,hitagi-256-class (:inherit secondary-selection))))

   `(realgud-bp-line-enabled-face
     ((,hitagi-class (:foreground ,hitagi-red-d)))
     ((,hitagi-256-class (:foreground ,hitagi-256-red-d))))

   `(realgud-bp-line-disabled-face
     ((,hitagi-class (:inherit secondary-selection)))
     ((,hitagi-256-class (:inherit secondary-selection))))

   `(realgud-line-number
     ((,hitagi-class (:inerhit hitagi-line-number)))
     ((,hitagi-256-class (:inerhit hitagi-line-number))))

   `(realgud-backtrace-number
     ((,hitagi-class (:foreground ,hitagi-yellow-d
                                   :weight bold)))
     ((,hitagi-256-class (:foreground ,hitagi-256-yellow
                                       :weight bold))))

   ;; rhtm-mode
   `(erb-face
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :background ,hitagi-256-background))))

   `(erb-delim-face
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :background ,hitagi-256-background))))

   `(erb-exec-face
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :background ,hitagi-256-background))))

   `(erb-exec-delim-face
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :background ,hitagi-256-background))))

   `(erb-out-face
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :background ,hitagi-256-background))))

   `(erb-out-delim-face
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :background ,hitagi-256-background))))

   `(erb-comment-face
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :background ,hitagi-256-background))))

   `(erb-comment-delim-face
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :background ,hitagi-256-background))))

   ;; rst-mode
   `(rst-level-1-face
     ((,hitagi-class (:background ,hitagi-yellow
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-yellow
                                        :foreground ,hitagi-256-background))))

   `(rst-level-2-face
     ((,hitagi-class (:background ,hitagi-cyan
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-cyan
                                        :foreground ,hitagi-256-background))))

   `(rst-level-3-face
     ((,hitagi-class (:background ,hitagi-blue
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-blue
                                        :foreground ,hitagi-256-background))))

   `(rst-level-4-face
     ((,hitagi-class (:background ,hitagi-violet
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-violet
                                        :foreground ,hitagi-256-background))))

   `(rst-level-5-face
     ((,hitagi-class (:background ,hitagi-magenta
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-magenta
                                        :foreground ,hitagi-256-background))))

   `(rst-level-6-face
     ((,hitagi-class (:background ,hitagi-red
                                   :foreground ,hitagi-background))
      (,hitagi-256-class  (:background ,hitagi-256-red
                                        :foreground ,hitagi-256-background))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(rpm-spec-doc-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(rpm-spec-ghost-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(rpm-spec-macro-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(rpm-spec-package-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(rpm-spec-section-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(rpm-spec-tag-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(rpm-spec-var-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,hitagi-class (:foreground ,hitagi-violet
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet
                                        :weight bold))))

   `(sh-escaped-newline
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight bold))))

   `(sh-heredoc
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   `(sp-wrap-overlay-face
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   `(sp-show-pair-enclosing
     ((,hitagi-class (:inherit highlight))
      (,hitagi-256-class  (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,hitagi-class (:foreground ,hitagi-green
                                   :background ,hitagi-background
                                   :weight normal
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :background ,hitagi-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,hitagi-class (:foreground ,hitagi-red
                                   :background ,hitagi-background
                                   :weight normal
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :background ,hitagi-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,hitagi-class (:foreground ,hitagi-green
                                   :background ,hitagi-background
                                   :weight normal
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :background ,hitagi-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(show-paren-mismatch
     ((,hitagi-class (:foreground ,hitagi-red
                                   :background ,hitagi-background
                                   :weight normal
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :background ,hitagi-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,hitagi-class (:foreground ,hitagi-green
                                   :background ,hitagi-background
                                   :weight normal
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :background ,hitagi-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-mismatch
     ((,hitagi-class (:foreground ,hitagi-red
                                   :background ,hitagi-background
                                   :weight normal
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :background ,hitagi-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-no-match
     ((,hitagi-class (:foreground ,hitagi-red
                                   :background ,hitagi-background
                                   :weight normal
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :background ,hitagi-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   ;; speedbar
   `(speedbar-button-face
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-comments))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-comments))))

   `(speedbar-directory-face
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-blue))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-blue))))

   `(speedbar-file-face
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-foreground))))

   `(speedbar-highlight-face
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :background ,hitagi-highlight-line))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :background ,hitagi-256-highlight-line))))

   `(speedbar-selected-face
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-yellow
                                :underline t))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-yellow
                                     :underline t))))

   `(speedbar-separator-face
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :background ,hitagi-blue
                                :foreground ,hitagi-background
                                :overline ,hitagi-cyan-lc))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :background ,hitagi-256-blue
                                     :foreground ,hitagi-256-background
                                     :overline ,hitagi-256-cyan-lc))))

   `(speedbar-tag-face
     ((,hitagi-class (:inherit ,hitagi-pitch
                                :foreground ,hitagi-green))
      (,hitagi-256-class  (:inherit ,hitagi-pitch
                                     :foreground ,hitagi-256-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,hitagi-class (:background ,hitagi-blue
                                   :foreground ,hitagi-background
                                   :height ,hitagi-height-plus-1
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-blue
                                        :foreground ,hitagi-256-background
                                        :height ,hitagi-height-plus-1
                                        :weight bold))))

   `(sr-editing-path-face
     ((,hitagi-class (:background ,hitagi-yellow
                                   :foreground ,hitagi-background
                                   :weight bold
                                   :height ,hitagi-height-plus-1))
      (,hitagi-256-class  (:background ,hitagi-256-yellow
                                        :foreground ,hitagi-256-background
                                        :weight bold
                                        :height ,hitagi-height-plus-1))))

   `(sr-highlight-path-face
     ((,hitagi-class (:background ,hitagi-green
                                   :foreground ,hitagi-background
                                   :weight bold
                                   :height ,hitagi-height-plus-1))
      (,hitagi-256-class  (:background ,hitagi-256-green
                                        :foreground ,hitagi-256-background
                                        :weight bold
                                        :height ,hitagi-height-plus-1))))

   `(sr-passive-path-face
     ((,hitagi-class (:background ,hitagi-comments
                                   :foreground ,hitagi-background
                                   :weight bold
                                   :height ,hitagi-height-plus-1))
      (,hitagi-256-class  (:background ,hitagi-256-comments
                                        :foreground ,hitagi-256-background
                                        :weight bold
                                        :height ,hitagi-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,hitagi-class (:inherit dihitagi-red-marked))
      (,hitagi-256-class  (:inherit dihitagi-red-marked))))

   `(sr-marked-file-face
     ((,hitagi-class (:inherit dihitagi-red-marked))
      (,hitagi-256-class  (:inherit dihitagi-red-marked))))

   `(sr-alt-marked-dir-face
     ((,hitagi-class (:background ,hitagi-magenta
                                   :foreground ,hitagi-background
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-magenta
                                        :foreground ,hitagi-256-background
                                        :weight bold))))

   `(sr-alt-marked-file-face
     ((,hitagi-class (:background ,hitagi-magenta
                                   :foreground ,hitagi-background
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-magenta
                                        :foreground ,hitagi-256-background
                                        :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,hitagi-class (:inherit dihitagi-red-directory
                                :weight normal))
      (,hitagi-256-class  (:inherit dihitagi-red-directory
                                     :weight normal))))

   `(sr-symlink-directory-face
     ((,hitagi-class (:inherit dihitagi-red-directory
                                :slant italic
                                :weight normal))
      (,hitagi-256-class  (:inherit dihitagi-red-directory
                                     :slant italic
                                     :weight normal))))

   `(sr-symlink-face
     ((,hitagi-class (:inherit dihitagi-red-symlink
                                :slant italic
                                :weight normal))
      (,hitagi-256-class  (:inherit dihitagi-red-symlink
                                     :slant italic
                                     :weight normal))))

   `(sr-broken-link-face
     ((,hitagi-class (:inherit dihitagi-red-warning
                                :slant italic
                                :weight normal))
      (,hitagi-256-class  (:inherit dihitagi-red-warning
                                     :slant italic
                                     :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(sr-encrypted-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(sr-log-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(sr-packaged-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(sr-html-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(sr-xml-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,hitagi-class (:background ,hitagi-red
                                   :foreground ,hitagi-background
                                   :weight bold))
      (,hitagi-256-class  (:background ,hitagi-256-red
                                        :foreground ,hitagi-256-background
                                        :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-yellow))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-yellow))))

   `(syslog-hour-face
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-green))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-green))))

   `(syslog-error-face
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-red
                                   :weight bold))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-red
                                        :weight bold))))

   `(syslog-warn-face
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-orange
                                   :weight bold))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-orange
                                        :weight bold))))

   `(syslog-info-face
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-blue
                                   :weight bold))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-blue
                                        :weight bold))))

   `(syslog-debug-face
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-cyan
                                   :weight bold))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-cyan
                                        :weight bold))))

   `(syslog-su-face
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-magenta))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-magenta))))

   ;; table
   `(table-cell
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :background ,hitagi-highlight-line))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :background ,hitagi-256-highlight-line))))

   ;; term
   `(term-color-black
     ((,hitagi-class (:foreground ,hitagi-background
                                   :background ,hitagi-highlight-line))
      (,hitagi-256-class  (:foreground ,hitagi-256-background
                                        :background ,hitagi-256-highlight-line))))

   `(term-color-red
     ((,hitagi-class (:foreground ,hitagi-red
                                   :background ,hitagi-red-d))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :background ,hitagi-256-red-d))))

   `(term-color-green
     ((,hitagi-class (:foreground ,hitagi-green
                                   :background ,hitagi-green-d))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :background ,hitagi-256-green-d))))

   `(term-color-yellow
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :background ,hitagi-yellow-d))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :background ,hitagi-256-yellow-d))))

   `(term-color-blue
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :background ,hitagi-blue-d))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :background ,hitagi-256-blue-d))))

   `(term-color-magenta
     ((,hitagi-class (:foreground ,hitagi-magenta
                                   :background ,hitagi-magenta-d))
      (,hitagi-256-class  (:foreground ,hitagi-256-magenta
                                        :background ,hitagi-256-magenta-d))))

   `(term-color-cyan
     ((,hitagi-class (:foreground ,hitagi-cyan
                                   :background ,hitagi-cyan-d))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan
                                        :background ,hitagi-256-cyan-d))))

   `(term-color-white
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :background ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :background ,hitagi-256-foreground))))

   `(term-default-fg-color
     ((,hitagi-class (:inherit term-color-white))
      (,hitagi-256-class  (:inherit term-color-white))))

   `(term-default-bg-color
     ((,hitagi-class (:inherit term-color-black))
      (,hitagi-256-class  (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,hitagi-class (:background ,hitagi-yellow-hc
                                   :foreground ,hitagi-background
                                   :inherit ,hitagi-pitch))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,hitagi-class (:foreground ,hitagi-magenta
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-magenta
                                        :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :background ,hitagi-highlight-line
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :background ,hitagi-256-highlight-line
                                        :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,hitagi-class (:foreground ,hitagi-emphasis))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis))))

   `(tuareg-font-lock-error-face
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :background ,hitagi-red
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :background ,hitagi-256-red
                                        :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,hitagi-class (:foreground ,hitagi-cyan))
      (,hitagi-256-class  (:foreground ,hitagi-256-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :background ,hitagi-background))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :background ,hitagi-256-background))))

   `(undo-tree-visualizer-unmodified-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(undo-tree-visualizer-current-face
     ((,hitagi-class (:foreground ,hitagi-blue
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue
                                        :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :background ,hitagi-background
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :background ,hitagi-256-background
                                        :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   ;; volatile highlights
   `(vhl/default-face
     ((,hitagi-class (:background ,hitagi-green-lc
                                   :foreground ,hitagi-green-hc))
      (,hitagi-256-class  (:background ,hitagi-256-green-lc
                                        :foreground ,hitagi-256-green-hc))))

   ;; w3m
   `(w3m-anchor
     ((,hitagi-class (:inherit link))
      (,hitagi-256-class  (:inherit link))))

   `(w3m-arrived-anchor
     ((,hitagi-class (:inherit link-visited))
      (,hitagi-256-class  (:inherit link-visited))))

   `(w3m-form
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-foreground))))

   `(w3m-header-line-location-title
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-yellow))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-yellow))))

   `(w3m-header-line-location-content

     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-foreground))))

   `(w3m-bold
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :weight bold))))

   `(w3m-image-anchor
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-cyan
                                   :inherit link))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-cyan
                                        :inherit link))))

   `(w3m-image
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-cyan))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,hitagi-class (:foreground ,hitagi-emphasis))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis))))

   `(w3m-lnum-match
     ((,hitagi-class (:background ,hitagi-highlight-line))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line))))

   `(w3m-lnum
     ((,hitagi-class (:underline nil
                                  :bold nil
                                  :foreground ,hitagi-red))
      (,hitagi-256-class  (:underline nil
                                       :bold nil
                                       :foreground ,hitagi-256-red))))

   `(w3m-session-select
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(w3m-session-selected
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :bold t
                                   :underline t))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :bold t
                                        :underline t))))

   `(w3m-tab-background
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-foreground))))

   `(w3m-tab-selected-background
     ((,hitagi-class (:background ,hitagi-background
                                   :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:background ,hitagi-256-background
                                        :foreground ,hitagi-256-foreground))))

   `(w3m-tab-mouse
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-yellow))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-yellow))))

   `(w3m-tab-selected
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-emphasis
                                   :bold t))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-emphasis
                                        :bold t))))

   `(w3m-tab-unselected
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-foreground))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-foreground))))

   `(w3m-tab-selected-retrieving
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-red))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-red))))

   `(w3m-tab-unselected-retrieving
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-orange))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-orange))))

   `(w3m-tab-unselected-unseen
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :foreground ,hitagi-violet))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :foreground ,hitagi-256-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(web-mode-comment-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(web-mode-constant-face
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(web-mode-current-element-highlight-face
     ((,hitagi-class (:underline unspecified
                                  :weight unspecified
                                  :background ,hitagi-highlight-line))
      (,hitagi-256-class  (:underline unspecified
                                       :weight unspecified
                                       :background ,hitagi-256-highlight-line))))

   `(web-mode-doctype-face
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :slant italic
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :slant italic
                                        :weight bold))))

   `(web-mode-folded-face
     ((,hitagi-class (:underline t))
      (,hitagi-256-class  (:underline t))))

   `(web-mode-function-name-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(web-mode-html-attr-name-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(web-mode-html-attr-custom-face
     ((,hitagi-class (:inherit web-mode-html-attr-name-face))
      (,hitagi-256-class  (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-engine-face
     ((,hitagi-class (:inherit web-mode-block-delimiter-face))
      (,hitagi-256-class  (:inherit web-mode-block-delimiter-face))))

   `(web-mode-html-attr-equal-face
     ((,hitagi-class (:inherit web-mode-html-attr-name-face))
      (,hitagi-256-class  (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-value-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(web-mode-html-tag-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(web-mode-keyword-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(web-mode-preprocessor-face
     ((,hitagi-class (:foreground ,hitagi-yellow
                                   :slant normal
                                   :weight unspecified))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow
                                        :slant normal
                                        :weight unspecified))))

   `(web-mode-string-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(web-mode-type-face
     ((,hitagi-class (:inherit font-lock-type-face))
      (,hitagi-256-class  (:inherit font-lock-type-face))))

   `(web-mode-variable-name-face
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(web-mode-warning-face
     ((,hitagi-class (:inherit font-lock-warning-face))
      (,hitagi-256-class  (:inherit font-lock-warning-face))))

   `(web-mode-block-face
     ((,hitagi-class (:background unspecified))
      (,hitagi-256-class  (:background unspecified))))

   `(web-mode-block-delimiter-face
     ((,hitagi-class (:inherit font-lock-preprocessor-face))
      (,hitagi-256-class  (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-comment-face
     ((,hitagi-class (:inherit web-mode-comment-face))
      (,hitagi-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,hitagi-class (:inherit font-lock-preprocessor-face))
      (,hitagi-256-class  (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-string-face
     ((,hitagi-class (:inherit web-mode-string-face))
      (,hitagi-256-class  (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,hitagi-class (:box 1 :weight bold))
      (,hitagi-256-class  (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face
     ((,hitagi-class (:inherit font-lock-constant-face))
      (,hitagi-256-class  (:inherit font-lock-constant-face))))

   `(web-mode-css-pseudo-class-face
     ((,hitagi-class (:inherit font-lock-builtin-face))
      (,hitagi-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-color-face
     ((,hitagi-class (:inherit font-lock-builtin-face))
      (,hitagi-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-filter-face
     ((,hitagi-class (:inherit font-lock-function-name-face))
      (,hitagi-256-class  (:inherit font-lock-function-name-face))))

   `(web-mode-css-function-face
     ((,hitagi-class (:inherit font-lock-builtin-face))
      (,hitagi-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-call-face
     ((,hitagi-class (:inherit font-lock-function-name-face))
      (,hitagi-256-class  (:inherit font-lock-function-name-face))))

   `(web-mode-css-priority-face
     ((,hitagi-class (:inherit font-lock-builtin-face))
      (,hitagi-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,hitagi-class (:inherit font-lock-variable-name-face))
      (,hitagi-256-class  (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,hitagi-class (:inherit font-lock-keyword-face))
      (,hitagi-256-class  (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,hitagi-class (:inherit web-mode-string-face))
      (,hitagi-256-class  (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,hitagi-class (:inherit web-mode-string-face))
      (,hitagi-256-class  (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face
     ((,hitagi-class (:inherit web-mode-comment-face))
      (,hitagi-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-json-context-face
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(web-mode-json-key-face
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(web-mode-json-string-face
     ((,hitagi-class (:inherit web-mode-string-face))
      (,hitagi-256-class  (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(web-mode-part-comment-face
     ((,hitagi-class (:inherit web-mode-comment-face))
      (,hitagi-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,hitagi-class (:inherit web-mode-block-face))
      (,hitagi-256-class  (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,hitagi-class (:inherit web-mode-string-face))
      (,hitagi-256-class  (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,hitagi-class (:foreground ,hitagi-violet))
      (,hitagi-256-class  (:foreground ,hitagi-256-violet))))

   `(web-mode-whitespace-face
     ((,hitagi-class (:background ,hitagi-red))
      (,hitagi-256-class  (:background ,hitagi-256-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-comments
                                   :inverse-video unspecified
                                   :slant italic))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-comments
                                        :inverse-video unspecified
                                        :slant italic))))

   `(whitespace-hspace
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-emphasis
                                   :inverse-video unspecified))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-emphasis
                                        :inverse-video unspecified))))

   `(whitespace-tab
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-red
                                   :inverse-video unspecified
                                   :weight bold))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-red
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-newline
     ((,hitagi-class(:background unspecified
                                  :foreground ,hitagi-comments
                                  :inverse-video unspecified))
      (,hitagi-256-class (:background unspecified
                                       :foreground ,hitagi-256-comments
                                       :inverse-video unspecified))))

   `(whitespace-trailing
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-orange-lc
                                   :inverse-video t))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-orange-lc
                                        :inverse-video t))))

   `(whitespace-line
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-magenta
                                   :inverse-video unspecified))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-magenta
                                        :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,hitagi-class (:background ,hitagi-red-lc
                                   :foreground unspecified
                                   :inverse-video unspecified))
      (,hitagi-256-class  (:background ,hitagi-256-red-lc
                                        :foreground unspecified
                                        :inverse-video unspecified))))

   `(whitespace-indentation
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-yellow
                                   :inverse-video unspecified
                                   :weight bold))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-yellow
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-empty
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-red-lc
                                   :inverse-video t))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-red-lc
                                        :inverse-video t))))

   `(whitespace-space-after-tab
     ((,hitagi-class (:background unspecified
                                   :foreground ,hitagi-orange
                                   :inverse-video t
                                   :weight bold))
      (,hitagi-256-class  (:background unspecified
                                        :foreground ,hitagi-256-orange
                                        :inverse-video t
                                        :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(wl-highlight-folder-many-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(wl-highlight-folder-path-face
     ((,hitagi-class (:foreground ,hitagi-orange))
      (,hitagi-256-class  (:foreground ,hitagi-256-orange))))

   `(wl-highlight-folder-unread-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(wl-highlight-folder-zero-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(wl-highlight-folder-unknown-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(wl-highlight-message-citation-header
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(wl-highlight-message-cited-text-1
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(wl-highlight-message-cited-text-2
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(wl-highlight-message-cited-text-3
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(wl-highlight-message-cited-text-4
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(wl-highlight-message-header-contents-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(wl-highlight-message-headers-face
     ((,hitagi-class (:foreground ,hitagi-red))
      (,hitagi-256-class  (:foreground ,hitagi-256-red))))

   `(wl-highlight-message-important-header-contents
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(wl-highlight-message-header-contents
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(wl-highlight-message-important-header-contents2
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(wl-highlight-message-signature
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(wl-highlight-summary-answehitagi-red-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(wl-highlight-summary-disposed-face
     ((,hitagi-class (:foreground ,hitagi-foreground
                                   :slant italic))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground
                                        :slant italic))))

   `(wl-highlight-summary-new-face
     ((,hitagi-class (:foreground ,hitagi-blue))
      (,hitagi-256-class  (:foreground ,hitagi-256-blue))))

   `(wl-highlight-summary-normal-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(wl-highlight-summary-thread-top-face
     ((,hitagi-class (:foreground ,hitagi-yellow))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow))))

   `(wl-highlight-thread-indent-face
     ((,hitagi-class (:foreground ,hitagi-magenta))
      (,hitagi-256-class  (:foreground ,hitagi-256-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(wl-highlight-summary-displaying-face
     ((,hitagi-class (:underline t
                                  :weight bold))
      (,hitagi-256-class  (:underline t
                                       :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,hitagi-class (:inherit error))
      (,hitagi-256-class  (:inherit error))))

   `(weechat-highlight-face
     ((,hitagi-class (:foreground ,hitagi-emphasis
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-emphasis
                                        :weight bold))))

   `(weechat-nick-self-face
     ((,hitagi-class (:foreground ,hitagi-green
                                   :weight unspecified
                                   :inverse-video t))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :weight unspecified
                                        :inverse-video t))))

   `(weechat-prompt-face
     ((,hitagi-class (:inherit minibuffer-prompt))
      (,hitagi-256-class  (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   ;; which-func-mode
   `(which-func
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   ;; which-key
   `(which-key-key-face
     ((,hitagi-class (:foreground ,hitagi-green
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-green
                                        :weight bold))))

   `(which-key-separator-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(which-key-note-face
     ((,hitagi-class (:foreground ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments))))

   `(which-key-command-description-face
     ((,hitagi-class (:foreground ,hitagi-foreground))
      (,hitagi-256-class  (:foreground ,hitagi-256-foreground))))

   `(which-key-local-map-description-face
     ((,hitagi-class (:foreground ,hitagi-yellow-hc))
      (,hitagi-256-class  (:foreground ,hitagi-256-yellow-hc))))

   `(which-key-group-description-face
     ((,hitagi-class (:foreground ,hitagi-red
                                   :weight bold))
      (,hitagi-256-class  (:foreground ,hitagi-256-red
                                        :weight bold))))
   ;; window-number-mode
   `(window-number-face
     ((,hitagi-class (:foreground ,hitagi-green))
      (,hitagi-256-class  (:foreground ,hitagi-256-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :background ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :background ,hitagi-256-comments))))

   `(yascroll:thumb-fringe
     ((,hitagi-class (:foreground ,hitagi-comments
                                   :background ,hitagi-comments))
      (,hitagi-256-class  (:foreground ,hitagi-256-comments
                                        :background ,hitagi-256-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,hitagi-class (:background ,hitagi-highlight-line
                                   :box ,hitagi-emphasis))
      (,hitagi-256-class  (:background ,hitagi-256-highlight-line
                                        :box ,hitagi-256-emphasis)))))

  (custom-theme-set-variables
   'hitagi
   `(ansi-color-names-vector [,hitagi-background ,hitagi-red ,hitagi-green ,hitagi-yellow
                                                  ,hitagi-blue ,hitagi-magenta ,hitagi-cyan ,hitagi-foreground])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,hitagi-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,hitagi-magenta ,hitagi-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,hitagi-highlight-line . 0)
       (,hitagi-green-lc . 20)
       (,hitagi-cyan-lc . 30)
       (,hitagi-blue-lc . 50)
       (,hitagi-yellow-lc . 60)
       (,hitagi-orange-lc . 70)
       (,hitagi-magenta-lc . 85)
       (,hitagi-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,hitagi-background)
   `(pos-tip-background-color ,hitagi-yellow-hc)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,hitagi-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,hitagi-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,hitagi-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,hitagi-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,hitagi-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     (unspecified ,hitagi-background ,hitagi-highlight-line
                  ,hitagi-red-d ,hitagi-red
                  ,hitagi-green-d ,hitagi-green
                  ,hitagi-yellow-d ,hitagi-yellow
                  ,hitagi-blue-d ,hitagi-blue
                  ,hitagi-magenta-d ,hitagi-magenta
                  ,hitagi-cyan-d ,hitagi-cyan
                  ,hitagi-foreground ,hitagi-emphasis))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'hitagi)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; hitagi-theme.el ends here
