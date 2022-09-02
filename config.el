;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Mohamad Sahid Rahman"
      user-mail-address "sahidrahman404@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
        doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 24)
      doom-variable-pitch-font (font-spec :family "Ubuntu Nerd Font" :size 14))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font Mono-14"))
;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)
(setq doom-themes-treemacs-theme "doom-colors")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")
(setq org-roam-directory "~/Documents/org/roam/")
(after! org
  (map! :map org-mode-map
        :n "M-J" #'org-metadown
        :n "M-K" #'org-metaup))

(setq org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org")

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)


;; (setq org-agenda-files
;;       '("~/Documents/org/Tasks.org"
;;         "~/Documents/org/Habits.org"))

(require 'org-habit)
(add-hook 'org-agenda-mode 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(evilem-default-keybindings "SPC")
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; prettier
(use-package prettier.js
  :ensure t
  :after(rjsx-mode)
  :hook (rjsx-mode . prettier-js-mode))
(add-hook 'tsx-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)

;; (setq prettier-js-args '(
;;   "--single-quote" "true"
;;   "--arrow-parens" "avoid"
;;   "--semi" "true"
;; ))

;; (defconst doom-frame-transparency 100)
;; (set-frame-parameter (selected-frame) 'alpha doom-frame-transparency)
;; (add-to-list 'default-frame-alist `(alpha . ,doom-frame-transparency))
;; (defun dwc-smart-transparent-frame ()
;;   (set-frame-parameter
;;     (selected-frame)
;;     'alpha (if (frame-parameter (selected-frame) 'fullscreen)
;;               100
;;              doom-frame-transparency)))

(setq org-image-actual-width nil)

;; css selector
(require 'counsel-css)
(add-hook 'css-mode-hook #'counsel-css-imenu-setup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; open-with
(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.mp4\\'" "mpv" (file))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; org-download
(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(setq-default org-download-image-dir "/home/rahman/Documents/org/Pictures")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evil-escape
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)
(global-set-key (kbd "C-c C-g") 'evil-escape)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Native comp
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)
    ))
(setq comp-speed 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-recur
(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :demand t
  :config
  (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)

  ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
  ;; (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)

  (setq org-recur-finish-done t
        org-recur-finish-archive t))

;; Refresh org-agenda after rescheduling a task.
(defun org-agenda-refresh ()
  "Refresh all `org-agenda' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-maybe-redo)))))

(defadvice org-schedule (after refresh-agenda activate)
  "Refresh org-agenda."
  (org-agenda-refresh))

;; Log time a task was set to Done.
(setq org-log-done (quote time))

;; Don't log the time a task was rescheduled or redeadlined.
(setq org-log-redeadline nil)
(setq org-log-reschedule nil)

(
setq org-read-date-prefer-future 'time)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emojify
(use-package emojify
  :hook (after-init . global-emojify-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hydra
(defhydra doom-window-resize-hydra (:hint nil)
  "
             _k_ increase height
_h_ decrease width    _l_ increase width
             _j_ decrease height
"
  ("h" evil-window-decrease-width)
  ("k" evil-window-increase-height)
  ("j" evil-window-decrease-height)
  ("l" evil-window-increase-width)

  ("q" nil))
(map!
    (:prefix "SPC"
      :desc "Hydra resize" :n "r" #'doom-window-resize-hydra/body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto-save
;; (use-package super-save
;;   :ensure t
;;   :config
;;   (super-save-mode +1))

;; epub
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat doom-cache-dir "nov-places")))
(setq nov-text-width 80)

;; TSI mode
(use-package! tsi
:mode ("\\.tsx\\'" . tsi-typescript))

;; ;; TSX mode
(use-package! tsx-mode
:mode ("\\.tsx\\'" . tsx-mode))

;; GTD
(setq org-agenda-files '("~/Documents/org/gtd/inbox.org"
                         "~/Documents/org/gtd/gtd.org"
                         "~/Documents/org/gtd/tickler.org"
                         "~/Documents/org/Tasks.org"
                         "~/Documents/org/Habits.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Documents/org/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Documents/org/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/Documents/org/gtd/gtd.org" :maxlevel . 3)
                           ("~/Documents/org/gtd/someday.org" :level . 1)
                           ("~/Documents/org/gtd/tickler.org" :maxlevel . 2)))

;; pdf tool
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-view-mode))
(add-hook 'doc-view-mode-hook #'pdf-tools-install)

;; push anki notes
 (use-package! anki-editor
  :after org-mode)
(map! :leader
      :desc "anki push" "n p" #'anki-editor-push-notes)
