; -- Custom config --

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(setq package-selected-packages '(flycheck company helm-xref irony lsp-treemacs helm-lsp python-mode cmake-project))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))


; -- Company/LSP --
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))


(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

; -- C/C++ --
(defun custom-c++-mode-hook ()
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t))

(which-key-mode)
(add-hook 'c-mode-common-hook 'custom-c++-mode-hook)
(add-hook 'c++-mode-hook 'treemacs)

; C-hook
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t))  ; use spaces only if nil

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

; -- Python --
(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  :config
  (require 'dap-python))

(use-package company-jedi
  :ensure t
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  (setq jedi:complete-on-dot t))

; -- Misc --

; Theme
(setq doom-theme 'doom-dracula)

; Sets the transparency
(set-frame-parameter (selected-frame) 'alpha 92)
(add-to-list 'default-frame-alist `(alpha . ,92))

; Creates a new file
(defun generate-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

; Remove some truely evil keybindings
(define-key evil-insert-state-map (kbd "C-w") nil)
(define-key evil-replace-state-map (kbd "C-w") nil)
(define-key evil-motion-state-map (kbd "C-y") nil)
(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key +doom-dashboard-mode-map (kbd "C-p") nil)

; Custom keybindings
(global-set-key (kbd "C-c n") 'generate-buffer)         ; new buffer
(global-set-key (kbd "C-w") 'clipboard-kill-region)     ; cut
(global-set-key (kbd "C-c x") 'clipboard-yank)          ; paste
