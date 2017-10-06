;; This file is for loading general and which-key bindings
(use-package general
  :ensure t
  :config (progn
        ;; different states get different general-define-key blocks
        ;; eg, we dont want the , leader key to be active in insert mode
      ;; ============= GENERAL KEYS - MISC =============
        (general-define-key
          :states '(normal motion emacs insert)
          "C-h" 'evil-window-left
          "C-j" 'evil-window-down
          "C-k" 'evil-window-up
          "C-l" 'evil-window-right)
        ;; ============= GENERAL KEYS - VIM =============
        ;; COMMA LEADER
        (general-define-key
            :states '(normal motion emacs)
            :prefix ","
            ;; first-hand shortcuts
            "c" 'kill-this-buffer
            "l" 'linum-relative-toggle
            ;; DIRECTORIES!
            "dd" '(lambda () (interactive) (dired "~/")) ;; to be deleted once used to treemacs
            "de" '(lambda () (interactive) (dired "~/Programming/electron"))
            "dp" '(lambda () (interactive) (dired "~/Programming/python"))
            "ds" '(lambda () (interactive) (dired "~/Programming/school"))
            "dw" '(lambda () (interactive) (dired "~/Programming/web"))
            "d." '(lambda () (interactive) (dired "~/.emacs.d/"))
            ;; MENUS - <leader><menu key> enters a "menu"
            ;; b - BUFFERS
            "bc" 'kill-buffer
            "bb" 'switch-to-buffer
            "bn" 'next-buffer
            "bp" 'previous-buffer
            ;; f - FILES
            "ff" 'counsel-find-file
            "fc" '(lambda () (interactive) (load-directory "~/.emacs.d"))
            ;; w - WINDOW
            "wc" 'evil-window-delete  ; window close
            "wv" 'evil-window-vnew
            "wh" 'evil-window-new     ; horizontal
            ;; h - HELP
            ;; h d - HELP > DESCRIBE
            "hdv" 'counsel-describe-variable
            "hdf" 'counsel-describe-function
            "hdk" 'describe-key
            ;; PROJECTILE
            "pf" 'counsel-projectile-find-file
            "pd" 'counsel-projectile-find-dir
            "pb" 'counsel-projectile-switch-to-buffer
            "pp" 'counsel-projectile-switch-project
            ;; TREEMACS
            ;;"dd" 'treemacs-projectile-toggle
            )))

;; which-key menu
(use-package which-key
  :ensure t
  :init (progn
          (which-key-mode 1)
          (which-key-setup-side-window-right)
          (which-key-add-key-based-replacements ",w" "Window..")
          (which-key-add-key-based-replacements ",f" "Files..")
          (which-key-add-key-based-replacements ",b" "Buffers..")
          ;; HELP COMMANDS
          (which-key-add-key-based-replacements ",h" "Help..")
          (which-key-add-key-based-replacements ",hd" "Describe..")
          ;; FILES
          (which-key-add-key-based-replacements ",dd" "~")
          (which-key-add-key-based-replacements ",de" "electron")
          (which-key-add-key-based-replacements ",dp" "python")
          (which-key-add-key-based-replacements ",ds" "school")
          (which-key-add-key-based-replacements ",dw" "web")
          (which-key-add-key-based-replacements ",d." ".emacs.d")))

(provide 'key_bindings)
