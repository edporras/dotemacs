
;; C++
;; C++ config based on https://tuhdo.github.io/c-ide.html
;; ======================================================

(require 'cc-mode)

;; gg-tags
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
(setq ggtags-executable-directory "/usr/local/bin")

;; semantic - language-aware editing commands
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)
(semantic-add-system-include "/usr/include/c++/4.2.1" 'c++-mode) ;; // hmm - path is very OS-X-y

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

(setq auto-mode-alist (append
		       '(
                 ("\\.cc\\'"  . c++-mode)
                 ("\\.h\\'"   . c++-mode)
                 ("\\.ipp\\'" . c++-mode)
                 ;;			 ("\\.pdf\\'" . pdf-mode)
                 ("\\.erb\\'" . ruby-mode)
                 ("\\.gemspec\\'" . ruby-mode)
                 ("\\Rakefile\\'" . ruby-mode)
                 ("\\.mak\\'" . makefile-mode)
                 )
		       auto-mode-alist))

;; my own tweaks from here on
(c-add-style "ep-c++" 
             '("linux"
               (indent-tabs-mode . nil)               ; use spaces rather than tabs
               (c-basic-offset . 4)                   ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . +)
                                   (case-label . 2)
                                   ))))


(defun my-cc-mode-hook ()
  ;; company is already set up; customize a few more things for c++
  (setq company-backends (delete 'company-semantic company-backends))
;;  (define-key c-mode-map  [(tab)] 'company-complete)
;;  (define-key c++-mode-map  [(tab)] 'company-complete)
  (add-to-list 'company-backends 'company-c-headers)
  ;; indentation & custom offsets
  (c-set-style "ep-c++")        ; use my-style defined above
;;  (setq c-default-style "linux" c-basic-offset 4)
;;  (setq c-offsets-alist (quote ((substatement-open . 0) (case-label . 2))))

  (setq compilation-scroll-output t)
  (setq compile-command "make -k -j 5")
  
  ;; ctrl-c ctrl-c to compile
  (define-key c++-mode-map "\C-c\C-c" 'compile)

  ;; don't use tabs, really.. make them spaces
  (setq-default indent-tabs-mode nil)
  )
;; apply this hook to C, C++ modes
(add-hook 'c-mode-hook 'my-cc-mode-hook)
(add-hook 'c++-mode-hook 'my-cc-mode-hook)
