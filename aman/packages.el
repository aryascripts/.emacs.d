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

(use-package linum-relative
  :ensure t)

;; projectile and counsel (ivy integration)
(use-package projectile
  :ensure t)
(use-package counsel-projectile
  :ensure t
  :init (progn
          (counsel-projectile-on)))

(use-package evil-commentary
  :ensure t
  :init (progn
          (evil-commentary-mode)))

(use-package tabbar
  :ensure t
  :init (progn
          (tabbar-mode t))
          (setq tabbar-use-images nil)
          (dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
          (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))

          (defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
            `(defun ,name (arg)
                (interactive "P")
                ,do-always
                (if (equal nil arg)
                    ,on-no-prefix
                ,on-prefix)))

            (defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
            (defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))

            (global-set-key [(control tab)] 'shk-tabbar-next)
            (global-set-key [(control shift tab)] 'shk-tabbar-prev))

(provide 'packages)
