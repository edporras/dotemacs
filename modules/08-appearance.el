;;; package --- Summary
;;; Commentary:

;;; Code:

;; enable sRGB colors in the Cocoa version of em
;;(setq ns-use-srgb-colorspace t)

(setq
 ;; display line & column numbers in mode-line
 line-number-mode t
 column-number-mode t

 ;; speed up screen re-paint on local sessions
 redisplay-dont-pause t 

 ;; general look and feel things
 font-lock-maximum-decoration t
 color-theme-is-global t
 visible-bell nil
 truncate-partial-width-windows nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; put the current filename and path in the frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; font and spacing
(set-frame-font "Menlo-14")
(setq-default line-spacing 4)

;; color emoji support
(if (fboundp 'set-fontset-font)
	(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; cursor settings
;;(setq default-cursor-type 'bar)
(setq cursor-type 'box)
(blink-cursor-mode 100)
;;(setq cursor-in-non-selected-windows nil)

;; fancy lambda, &c
(global-prettify-symbols-mode 1)
(mapc (lambda (m) (add-hook m (lambda () (push '("fn" . ?ƒ) prettify-symbols-alist))))
      '(clojure-mode-hook clojurescript-mode-hook))

;; diminish global minor modes
(eval-after-load 'undo-tree
  '(diminish 'undo-tree-mode))

;; locally hacked version of noctilux, turning into something else
(add-to-list 'load-path (concat user-emacs-directory "eigengrau/"))
(require 'eigengrau-theme)

;; doom-modeline config
;;(setq doom-modeline-height 20)
(set-face-attribute 'mode-line nil :family "Meslo LG S DZ Regular" :height 110)
;;(set-face-attribute 'mode-line-inactive nil :family "Menlo" :height 110)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
;;(setq doom-modeline-major-mode-color-icon nil)
;;(setq doom-modeline-minor-modes nil)
;;(setq doom-modeline-env-version t)
;;(setq doom-modeline-lsp t)
;;(setq doom-modeline-persp-name t)
;;(setq doom-modeline-display-default-persp-name nil)

;; dim the parentheses
(require 'parenface-plus)
(set-face-foreground 'paren-face "#666")

;; these break launching of emacs-26 daemon -ep
;; customize company-mode's popup
;; (let ((bg (face-attribute 'default :background)))
;;     (custom-set-faces
;;      `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;      `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;      `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;      `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
;;      `(company-tooltip-common-selection ((t (:inherit font-lock-keyword-face))))
;;      `(company-tooltip-selection ((t (:inherit font-lock-keyword-face))))))

(provide '08-appearance)
;;; 08-appearance.el ends here

