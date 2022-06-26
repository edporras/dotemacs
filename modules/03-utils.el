;;; package --- Summary
;;; Commentary:
;;; Code:

(when (memq window-system '(mac ns x))
  ;; ssh-agent stuff.. seems like a constant battle
  ;; (exec-path-from-shell-variables '("SSH_AUTH_SOCK" "SSH_AGENT_PID"))
  (exec-path-from-shell-initialize))

;; from https://www.emacswiki.org/emacs/EshellPrompt
(defun fish-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(defun shk-eshell-prompt ()
  (let ((header-bg "#888"))
    (concat
     (with-face (concat (eshell/pwd) " ") :background header-bg)
     (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time)) :background header-bg :foreground "#888")
     (with-face
      (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) "")
      :background header-bg)
     (with-face "\n" :background header-bg)
     (with-face user-login-name :foreground "blue")
     "@"
     (with-face "localhost" :foreground "green")
     (if (= (user-uid) 0)
         (with-face " #" :foreground "red")
       " $")
     " ")))

(defun shortened-path (path max-len)
  "Return a modified version of `path', replacing some components
      with single characters starting from the left to try and get
      the path down to `max-len'"
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(defun rjs-eshell-prompt-function ()
  (concat (fish-path (eshell/pwd) 40)
          (if (= (user-uid) 0) " # " " $ ")))

(setq eshell-prompt-function 'rjs-eshell-prompt-function)
(setq eshell-highlight-prompt nil)

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

;; eshell ansi handling
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; undo-tree is pretty nifty
(require 'undo-tree)
(global-undo-tree-mode 1)
;; Prevent undo tree files from polluting your git repo
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; eyebrowse for saving buffer configs
(eyebrowse-mode t)

;; ma-git, as Sybs would say it
(defvar magit-mode-map)
(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-x g") 'magit-status)
(define-key magit-mode-map [remap previous-line] 'magit-previous-line)
(define-key magit-mode-map [remap next-line] 'magit-next-line)
(setq magit-refresh-status-buffer nil) ; for performance
(setq magit-push-current-set-remote-if-missing nil)
(setq magit-revision-insert-related-refs nil)
(setq magit-diff-highlight-trailing nil)
(setq magit-diff-paint-whitespace nil)
;; re-enable --set-upstream switch
;; (magit-define-popup-switch 'magit-push-popup
;;                            ?u "Set upstream" "--set-upstream")

;; change dired find-ls
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

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

;; es-mode
(add-to-list 'auto-mode-alist '("\\.es$" . es-mode))
(add-hook 'es-result-mode-hook 'hs-minor-mode)
(setq es-always-pretty-print 't)

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

;; search for a solution to this and found jack's :)
(defun json->edn ()
  (interactive)
  (shell-command-on-region (region-beginning)
                           (region-end)
                           "jet --pretty --keywordize keyword --from json --to edn"
                           (current-buffer)
                           t))

;; jq-mode
(autoload 'jq-mode "jq-mode.el"
  "Major mode for editing jq files" t)
(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))

;; discover-mode
;; https://www.masteringemacs.org/article/discoverel-discover-emacs-context-menus
(global-discover-mode 1)

;; https://github.com/raxod502/selectrum
;; (selectrum-mode +1)
;; (selectrum-prescient-mode +1)
;; (prescient-persist-mode +1)

;; extra settings
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")

(provide '03-utils)
;;; 03-utils.el ends here
