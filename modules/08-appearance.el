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
 ;; redisplay-dont-pause t ;; obsolete as of 24.5

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

;; doom-modeline config
;;(setq doom-modeline-height 20)
(set-face-attribute 'mode-line nil :family "Meslo LG S DZ Regular" :height 110)
;;(set-face-attribute 'mode-line-inactive nil :family "Menlo" :height 110)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-vcs-max-length 25)
;;(setq doom-modeline-major-mode-color-icon nil)
;;(setq doom-modeline-minor-modes nil)
;;(setq doom-modeline-env-version t)
;;(setq doom-modeline-lsp t)
;;(setq doom-modeline-persp-name t)
;;(setq doom-modeline-display-default-persp-name nil)

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

;;(setq-default show-trailing-whitespace t)
;; (defun my-hide-trailing-whitespace-maybe ()
;;   "Disable `show-trailing-whitespace' in selected modes."
;;   (when (derived-mode-p 'shell-mode
;;                         'eshell-mode)
;;     (setq show-trailing-whitespace nil)))

(setq-default show-trailing-whitespace nil)
(defun me/show-trailing-whitespace ()
  "Just sets `show-trailing-whitespace'."
  (setq show-trailing-whitespace t))
(add-hook 'clojure-mode-hook 'me/show-trailing-whitespace)
(add-hook 'clojurescript-mode-hook 'me/show-trailing-whitespace)
(add-hook 'enh-ruby-mode-hook 'me/show-trailing-whitespace)
(add-hook 'emacs-lisp-mode-hook 'me/show-trailing-whitespace)
(add-hook 'markdown-mode-hook 'me/show-trailing-whitespace)

;; (add-hook 'after-change-major-mode-hook
;;           'my-hide-trailing-whitespace-maybe)



(provide '08-appearance)
;;; 08-appearance.el ends here
