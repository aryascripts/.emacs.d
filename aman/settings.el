;; change window name and cursor type
(setq frame-title-format "emacs")
(set-default 'cursor-type 'bar)
(ido-mode)

;; enable cursor line highlight
(global-hl-line-mode)

;; window mode
(winner-mode t)

;; use shift+arrows to move windows
(windmove-default-keybindings)

;; ignore the bell (visual)
(setq ring-bell-function 'ignore)

;;Enable line numbers
(global-linum-mode)

;; Show braces and highlight the pairs
(electric-pair-mode)
(show-paren-mode)

;; spaces over tabs
(setq-default indent-tabs-mode nil)

;; scrolling settings
(setq mouse-wheel-scroll-amount '(3)) ;; three lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; set name
(setq user-full-name "Aman Bhimani")

;; These functions are useful. Activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
(setq-default indent-tabs-mode nil)

(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

(global-visual-line-mode)

(setq uniquify-buffer-name-style 'forward)

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (thing-at-point 'symbol))
        regexp-history)
  (call-interactively 'occur))

(provide 'settings)