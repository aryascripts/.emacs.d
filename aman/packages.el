;; define package repos
(defconst gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defconst melpa '("melpa" . "https://melpa.org/packages/"))
(defconst melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; add package repos to archives list
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

(unless (and (file-exists-p "~/.emacs.d/elpa/archives/gnu")
  (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (file-exists-p "~/.emacs.d/elpa/archives/melpa-stable"))
  (package-refresh-contents))
;; evaluate the package list and install missing packages
(defun packages-install (&rest packages)
  ; (message "running packages-install")
  (mapc (lambda (package)
    (let ((name (car package))
    (repo (cdr package)))
      (when (not (package-installed-p name))
        (let ((package-archives (list repo)))
    (package-initialize)
    (package-install name)))))
  packages)
  (package-initialize)
  (delete-other-windows))

;; install any packages if they're missing
(defun init--install-packages ()
  ; (message "installing packages")
  (packages-install (cons 'use-package melpa)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;---------------------------------------------------;
;                 INSTALL PACKAGES                  ;
;___________________________________________________;

(use-package evil
  :ensure t
  :init (evil-mode 1)
  :config (define-key evil-normal-state-map "," nil))

(use-package powerline
  :ensure t
  :init (progn
          (powerline-default-theme)
          ; previews of separators: http://spacemacs.org/doc/DOCUMENTATION.html#mode-line
          (setq powerline-default-separator 'slant)
          ))

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
            "dd" '(lambda () (interactive) (dired "~/"))
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
            )))

(use-package ivy
  :ensure t
  :defer t
  :init (progn
          (ivy-mode 1)
          (setq ivy-use-virtual-buffers t)
          (setq enable-recursive-minibuffers t)))

(use-package counsel
  :ensure t
  :defer t)

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
          ;; BUFFERS
          (which-key-add-key-based-replacements ",wh" "window-horizontal")))

(use-package linum-relative
  :ensure t)

(provide 'packages)
