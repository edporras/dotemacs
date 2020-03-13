;;; package --- Summary
;;; Commentary:
;;; Code:

(when (memq window-system '(mac ns x))
  ;; ssh-agent stuff.. seems like a constant battle
  ;; (exec-path-from-shell-variables '("SSH_AUTH_SOCK" "SSH_AGENT_PID"))
  (exec-path-from-shell-initialize))

;; undo-tree is pretty nifty
(require 'undo-tree)
(global-undo-tree-mode 1)

;; eyebrowse for saving buffer configs
(eyebrowse-mode t)

;; ma-git, as Sybs would say it
(defvar magit-mode-map)
(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-x g") 'magit-status)
(define-key magit-mode-map [remap previous-line] 'magit-previous-line)
(define-key magit-mode-map [remap next-line] 'magit-next-line)

;; docker
(require 'dockerfile-mode)
(eval-after-load 'dockerfile-mode
  '(progn
     (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))))

;; j2
(require 'jinja2-mode)
(eval-after-load 'jinja2-mode
  '(progn
     (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))))

;; Window splitting. Don't remember where I found this but it's useful
;; on wide displays where emacs tends to stubbornly split horizontally.
(defun my-sensible-window-split (&optional window)
  "Be better about splitting the WINDOW."
  (cond ((and (> (window-width window)
                 (window-height window))
              (window-splittable-p window 'horizontal))
         (with-selected-window window
           (split-window-right)))
        ((window-splittable-p window)
         (with-selected-window window
           (split-window-below)))))
(setq split-window-preferred-function #'my-sensible-window-split)

;; don't ask if I want to follow symlinks please
(setq vc-follow-symlinks t)

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
;;(add-hook 'prog-mode-hook 'clean-aindent-mode)
(defun my-pkg-init()
  "See https://github.com/pmarinov/clean-aindent-mode."
  (electric-indent-mode -1)  ; no electric indent, auto-indent is sufficient
  (clean-aindent-mode t)
  (setq clean-aindent-is-simple-indent t)
  (define-key global-map (kbd "RET") 'newline-and-indent))
(add-hook 'after-init-hook 'my-pkg-init)

;; http://emacsredux.com/blog/2013/03/27/indent-region-or-buffer/
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(provide '03-utils)
;;; 03-utils.el ends here
