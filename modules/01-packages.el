;;; package --- Summary
;;; Commentary:

;;; Code:

(add-to-list 'package-archives '("gnu"          . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;(add-to-list 'package-archives '("marmalade"    . "https://marmalade-repo.org/packages/"))

(unless (file-exists-p (expand-file-name "archives/melpa" package-user-dir))
  (package-refresh-contents))

(package-install 'use-package) ;; (if this fails, e.g. because it can't find such-and-such version, try manually executing `package-refresh-contents`)

(setq package-native-compile t)

;;;; Packages we require
;;;; - Listed alphabetically
;;;; - Line breaks
;;;;   - every 10 packages, or
;;;;   - if the package requires special config (e.g. pinning to a particular archive)
;;(use-package ace-jump-mode     :ensure t)
;;(use-package ack-and-a-half    :ensure t)
;;(use-package all-the-icons     :ensure t)
(use-package ag                :ensure t)
(use-package aggressive-indent :ensure t)
(use-package anakondo
  :ensure t
  :commands anakondo-minor-mode)
(use-package avy               :ensure t)
(use-package bundler           :ensure t)
(use-package caml              :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable
  ;; :config
  ;; (advice-add 'cider-ansi-color-string-p :override
  ;;             (lambda (string) (string-match "�\\[" string)))
  ;; (advice-add 'cider-font-lock-as
  ;;             :before
  ;;             (lambda (&rest r)
  ;;               (advice-add 'substring-no-properties :override #'identity)))
  ;; (advice-add 'cider-font-lock-as
  ;;             :after
  ;;             (lambda (&rest r)
  ;;               (advice-remove 'substring-no-properties #'identity)))
  )
(use-package cider-eval-sexp-fu
  :ensure t
  :config
  (require 'cider))

(use-package clean-aindent-mode   :ensure t)
(use-package clojure-mode         :ensure t)
(use-package coffee-mode          :ensure t)
(use-package company              :ensure t)
;; (use-package company-ghc          :ensure t)
(use-package company-inf-ruby     :ensure t)
;;(use-package company-go           :ensure t)
(use-package csv-mode             :ensure t)

(use-package dash                 :ensure t)
(use-package diminish             :ensure t)
(use-package discover             :ensure t)

(use-package dockerfile-mode      :ensure t)
(use-package doom-modeline ;;  Run M-x nerd-icons-install-fonts
  :ensure t
  :hook (after-init . doom-modeline-mode))
(use-package enh-ruby-mode        :ensure t)
(use-package elisp-slime-nav      :ensure t)
(use-package epl                  :ensure t)
(use-package es-mode              :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package expand-region        :ensure t)
(use-package eyebrowse            :ensure t)
(use-package find-file-in-project :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-clj-kondo   :ensure t)
(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(use-package flymake-coffee       :ensure t)
;; (use-package flymake-cursor       :ensure t)
;; (use-package flymake-easy         :ensure t)
;; (use-package flymake-ruby         :ensure t)
(use-package fringe-helper        :ensure t)
(use-package geiser               :ensure t)

(use-package git-commit           :ensure t)
(use-package ggtags               :ensure t)
(use-package go-mode              :ensure t)
(use-package haml-mode            :ensure t)
(use-package haskell-mode         :ensure t)
(use-package highlight            :ensure t)
(use-package ido-completing-read+ :ensure t)
(use-package ido-grid-mode        :ensure t)
(use-package inf-ruby             :ensure t)

(use-package jinja2-mode          :ensure t)
(use-package jq-mode              :ensure t)
(use-package js2-mode             :ensure t)
(use-package js2-refactor         :ensure t)
(use-package json-mode            :ensure t)

(use-package load-env-vars        :ensure t)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((clojure-mode . lsp-deferred)
         (clojurescript-mode . lsp-deferred)
         (clojurec-mode . lsp-deferred)

         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package magit                :ensure t)
;; (use-package magit-delta
;;   :hook (magit-mode . magit-delta-mode))
(use-package markdown-mode        :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(use-package motion-mode          :ensure t)
(use-package multiple-cursors     :ensure t)

(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package org
  :ensure t
  :pin org)

(use-package paredit         :ensure t)
(use-package paren-face      :ensure t
  :init (global-paren-face-mode))
(use-package pkg-info        :ensure t)
(use-package poly-ansible    :ensure t)
;;(use-package powerline       :ensure t)
(use-package pretty-symbols  :ensure t)
(use-package processing-mode :ensure t)
(use-package rainbow-mode    :ensure t)
(use-package robe            :ensure t)
(use-package rbenv           :ensure t)

(use-package restclient      :ensure t)
(use-package rubocop         :ensure t)
(use-package rspec-mode      :ensure t)

(use-package s               :ensure t)
(use-package selectrum       :ensure t)
(use-package simple-httpd    :ensure t)
(use-package skewer-mode     :ensure t)
(use-package slime-company   :ensure t)
(use-package smartparens     :ensure t)
(use-package smartscan       :ensure t)
(use-package smex            :ensure t)
(use-package sr-speedbar     :ensure t)

(use-package tuareg          :ensure t)
;;(use-package typopunct       :ensure t)
(use-package undo-tree       :ensure t)
;;(use-package uuid            :ensure t)
(use-package visual-regexp   :ensure t)
(use-package web-mode        :ensure t)
(use-package which-key       :ensure t)
(use-package yaml-mode       :ensure t)
(use-package yasnippet       :ensure t)

;;; 01-packages.el ends here
