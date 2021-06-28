;;; package --- Summary
;;; Commentary:

;;; Code:

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent 'yes-or-no-p' from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent 'y-or-n-p' from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))
(setq use-dialog-box nil)

;; I will not type 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; turn off safety mode
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; ido-mode is a work of beauty and magic
(ido-mode t)
(ido-grid-mode 1)
(ido-everywhere t)
(ido-ubiquitous-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-grid-mode-max-rows 4
      ido-grid-mode-start-collapsed t)

;; smex is "smart M-x"
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq smex-key-advice-ignore-menu-bar t)

;; nicer buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; chorded backward kill magnifiers
(global-set-key (kbd "C-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-paragraph)

;; chorded backward kill magnifiers
(global-set-key [C-M-kp-delete] 'kill-paragraph)

;; isearch
;; recenter forward search on match
(add-hook 'isearch-mode-end-hook 'recenter-top-bottom)
(setq isearch-lazy-count t)

;; prefer regexp in my backward search, inputrc-compatible binding
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; moving between windows, normalized with iTerm2 and (mod'd) tmux
(global-set-key [M-s-left]  'windmove-left)
(global-set-key [M-s-right] 'windmove-right)
(global-set-key [M-s-up]    'windmove-up)
(global-set-key [M-s-down]  'windmove-down)

;; enhanced completion library, same as inputrc binding
(global-set-key (kbd "M-/") 'hippie-expand)

;; expand-region is super handy! I like having expand- and contract-
;; side by side within easy reach.
(require 'expand-region)
(global-set-key (kbd "C-1") 'er/expand-region)
(global-set-key (kbd "C-2") 'er/contract-region)

;; I can't get today's kids interested in set-mark, so I've repurposed
;; C-SPC for ace-jump-mode.
;; (require 'ace-jump-mode)
;; (global-set-key (kbd "C-SPC") 'ace-jump-mode)

;; avy
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-timer)  ;avy-goto-char-2
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

;; Enable which-key for function discovery
(which-key-mode)

;; Multiple cursors magic
(global-set-key (kbd "C-,") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;; 05-more-bindings.el ends here
