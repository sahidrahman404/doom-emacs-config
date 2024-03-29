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
(set-face-attribute 'default nil
                    :font "FiraCode Nerd Font Mono"
                    :height 120
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Ubuntu Nerd Font"
                    :height 100
                    :weight 'light)
(set-face-attribute 'fixed-pitch nil
                    :font "FiraCode Nerd Font Mono"
                    :height 120
                    :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font Mono-12"))
;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)
(setq   org-hide-emphasis-markers  t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)
(setq doom-themes-treemacs-theme "doom-colors")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")
(setq org-roam-directory "~/Documents/org/roam/")
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-fold-core-style "overlays")
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
(setq display-line-numbers-type nil)
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

;; org-download
(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(setq-default org-download-image-dir "~/Documents/org/Pictures")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evil-escape
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)
(global-set-key (kbd "C-c C-g") 'evil-escape)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Native comp
;; Silence compiler warnings as they can be pretty disruptive
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
  (setq native-comp-deferred-compilation nil))
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)
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

;; ;; TSX mode
(use-package! tsx-mode
        :config
        (setq auto-mode-alist (delete '("\\.tsx\\'" . typescript-tsx-mode) auto-mode-alist))
        (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode)))

(add-hook 'tsx-mode-hook #'auto-rename-tag-mode)
(add-hook 'tsx-mode-hook #'emmet-mode)
(add-hook 'tsx-mode-hook #'centered-cursor-mode)

;; GTD
(setq org-agenda-files '("~/Documents/org/gtd/inbox.org"
                         "~/Documents/org/gtd/gtd.org"
                         "~/Documents/org/gtd/tickler.org"
                         "~/Documents/org/Tasks.org"
                         "~/Documents/org/Habits.org"
                         "~/Documents/org/roam/20221227235810-project.org"))


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
  :after org)
(map! :leader
      :desc "anki push" "n p" #'anki-editor-push-notes)

(map! :leader
      :desc "anki create" "n i" #'anki-editor-insert-note)

;; beacon
(beacon-mode 1)

;; lsp headerline mode
(use-package lsp-mode
  :custom
  (lsp-headerline-breadcrumb-enable t))

;; apheleia
(use-package apheleia)
(add-to-list 'apheleia-mode-alist '(json-mode . prettier))
(add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
(add-to-list 'apheleia-mode-alist '(tsx-mode . prettier))
(apheleia-global-mode t)

;; centered
;; (use-package centered-cursor-mode)
;; (global-centered-cursor-mode t)

;; multi vterm
(use-package multi-vterm
  :config
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (setq-local evil-insert-state-cursor 'box)
	      (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

;; peep dired
(setq peep-dired-cleanup-on-disable t)

(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq peep-dired-ignored-extensions '("mkv" "iso" "mp4"))

(use-package! jest-interactive-mode)

;; dired
(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file" "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))
;;Making deleted files go to trash can
(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ts-node . t)
   ))

(map! :i [remap newline] nil)

(setq org-fold-core-style 'overlays)

;; evil-textobj-tree-sitter
;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

;; tailwind
(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-major-modes '(rjsx-mode web-mode html-mode css-mode typescript-mode tsx-mode)))

;; ts workaround
(use-package lsp-mode
  :defer t
  :config
  (advice-add 'json-parse-string :around
              (lambda (orig string &rest rest)
                (apply orig (s-replace "\\u0000" "" string)
                       rest)))
  (advice-add 'json-parse-buffer :around
              (lambda (orig &rest rest)
                (while (re-search-forward "\\u0000" nil t)
                  (replace-match ""))
                (apply orig rest))))
;; Harpoon

;; You can use this hydra menu that have all the commands
(map! :n "C-SPC" 'harpoon-quick-menu-hydra)
(map! :n "C-s" 'harpoon-add-file)

;; And the vanilla commands
(map! :leader "z c" 'harpoon-clear)
(map! :leader "z f" 'harpoon-toggle-file)
(map! :leader "1" 'harpoon-go-to-1)
(map! :leader "2" 'harpoon-go-to-2)
(map! :leader "3" 'harpoon-go-to-3)
(map! :leader "4" 'harpoon-go-to-4)
(map! :leader "5" 'harpoon-go-to-5)
(map! :leader "6" 'harpoon-go-to-6)
(map! :leader "7" 'harpoon-go-to-7)
(map! :leader "8" 'harpoon-go-to-8)
(map! :leader "9" 'harpoon-go-to-9)
