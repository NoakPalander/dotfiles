
# Table of Contents

1.  [Custom configuration for doom emacs](#org692b0a7)
    1.  [Packages](#orga7d4b4d)
    2.  [Dashboard](#org90484da)
    3.  [Treemacs](#org506d931)
    4.  [Company mode / LSP mode](#org368d17b)
    5.  [Miscellaneous](#org1daef1c)
2.  [Keybindings](#org5171a68)
    1.  [Remove previous evil-keybinds](#org8e51b78)
    2.  [Custom global keybindings](#org950db59)
    3.  [Frame tabs](#org6d3f078)
3.  [Language configurations](#orgf2fed9e)
    1.  [Elixir](#org3011646)



<a id="org692b0a7"></a>

# Custom configuration for doom emacs


<a id="orga7d4b4d"></a>

## Packages

    (require 'package)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
    (package-initialize)
    
    ; Packages this configuration uses
    (setq package-selected-packages '(dashboard
                                      fira-code-mode
                                      flycheck
                                      company
                                      helm-xref
                                      irony
                                      lsp-treemacs
                                      treemacs-all-the-icons
                                      helm-lsp
                                      python-mode
                                      cmake-project
                                      elixir-mode mix
                                      move-text))
    
    ; If these packages aren't loaded, install them
    (when (cl-find-if-not #'package-installed-p package-selected-packages)
      (package-refresh-contents)
      (mapc #'package-install package-selected-packages))


<a id="org90484da"></a>

## Dashboard

    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
    
    (use-package dashboard
      :ensure t
      :init
      (setq dashboard-banner-logo-title "Welcome back to Emacs")
      (setq dashboard-footer-messages '("alias vim = \'emacs\'"))
      (setq dashboard-set-heading-icons t)
      (setq dashboard-set-file-icons t)
      (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                       :height 1.1
                                                       :v-adjust -0.05
                                                       :face 'font-lock-keyword-face))
      (setq dashboard-items '((bookmarks . 5) (projects . 5)))
      (setq dashboard-center-content t)
      (setq dashboard-startup-banner "~/.emacs.d/logo.png")
    
      (setq dashboard-page-separator "\n\f\n")
      (setq dashboard-set-navigator t)
      (setq dashboard-navigator-buttons
          `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
             "Github"
             "Browse github"
             (lambda (&rest _) (browse-url "github.com/NoakPalander")))
    
             (,(all-the-icons-octicon "gear" :height 1.1 :v-adjust 0.0)
             "Dotfiles"
             "Browse dotfiles"
             (lambda (&rest _) (browse-url "github.com/NoakPalander/dotfiles")))
    
             (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
             "Doom"
             "Doom emacs"
             (lambda (&rest _) (browse-url "https://github.com/doomemacs/doomemacs"))))))
    
      :config
      (dashboard-setup-startup-hook)
      (page-break-lines-mode))


<a id="org506d931"></a>

## Treemacs

    (use-package treemacs-all-the-icons
      :after treemacs)


<a id="org368d17b"></a>

## Company mode / LSP mode

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
      (require 'dap-cpptools))


<a id="org1daef1c"></a>

## Miscellaneous

Some helper functions used

    ; Creates a new file
    (defun generate-buffer ()
      (interactive)
      (switch-to-buffer (make-temp-name "scratch")))
    
    ; Saves a file by sudo
    (defun sudo-save ()
      (interactive)
      (if (not buffer-file-name)
          (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
        (write-file (concat "/su
    do:root@localhost:" buffer-file-name))))
    
    ; Marks the current line
    (defun select-current-line ()
        "Select the current line"
      (interactive)
      (end-of-line)
      (set-mark (line-beginning-position)))

Disable automatic tracking by projectile and load the dracula theme

    (setq projectile-track-known-projects-automatically nil)
    (setq doom-theme 'dracula)

Tabbing

    (setq-default indent-tabs-mode nil)
    (setq-default tab-width 4)
    (setq indent-line-function 'insert-tab)

Font

    (setq disabled-ligatures '("[]" "::" ":" "lambda" "x" "#{" "#_" "#_(" "+" "<<" ">>"))
    (use-package fira-code-mode
      :custom (fira-code-mode-disabled-ligatures disabled-ligatures)
      :hook prog-mode)

Transparency

    (set-frame-parameter (selected-frame) 'alpha 92)
    (add-to-list 'default-frame-alist `(alpha . ,92))


<a id="org5171a68"></a>

# Keybindings


<a id="org8e51b78"></a>

## Remove previous evil-keybinds

    (define-key evil-insert-state-map (kbd "C-w") nil)
    (define-key evil-replace-state-map (kbd "C-w") nil)
    (define-key evil-motion-state-map (kbd "C-y") nil)
    (define-key evil-normal-state-map (kbd "C-p") nil)


<a id="org950db59"></a>

## Custom global keybindings

    (global-set-key (kbd "C-c n") 'generate-buffer)         ; new buffer
    (global-set-key (kbd "C-w") 'clipboard-kill-region)     ; cut
    (global-set-key (kbd "C-c x") 'clipboard-yank)          ; paste
    (global-set-key (kbd "C-c t") 'treemacs)                ; start treemacs
    (global-set-key (kbd "C-c l") 'select-current-line)     ; Selects the current line


<a id="org6d3f078"></a>

## Frame tabs

    (global-set-key (kbd "M-s M-f") 'tab-bar-new-tab)                       ; Creates a new tab
    (global-set-key (kbd "M-s M-d") 'tab-bar-close-tab)                     ; Closes the tab
    (global-set-key (kbd "M-s M-r") 'tab-bar-rename-tab)                    ; Renames the tab
    (global-set-key (kbd "M-s M-<right>") 'tab-bar-switch-to-next-tab)      ; Move to the next tab (right)
    (global-set-key (kbd "M-s M-<left>") 'tab-bar-switch-to-prev-tab)       ; Move to the previous tab (left)


<a id="orgf2fed9e"></a>

# Language configurations


<a id="org3011646"></a>

## Elixir

LSP mode for elixir, requires elixir-ls to be installed (e.g from the AUR)

    (use-package lsp-mode
      :commands lsp
      :ensure t
      :diminish lsp-mode
      :hook
      (elixir-mode . lsp)
      :init
      (add-to-list 'exec-path "/usr/lib/elixir-ls/")
      :config (setq lsp-elixir-suggest-specs nil))

Elixir MIX configurations

    (defun mix-run (&optional prefix use-umbrella-subprojects)
      "Run the mix escript.build command.
    If PREFIX is non-nil, prompt for additional params.  See `mix--prompt`
    IF USE-UMBRELLA-SUBPROJECTS is t, prompt for umbrells subproject."
      (interactive "P")
      (let ((project-root (if use-umbrella-subprojects (mix--umbrella-subproject-prompt) (mix--project-root))))
        (mix--start nil "App" project-root prefix)))
    
    (add-hook 'elixir-mode-hook
              (lambda () (setenv "LC_ALL" "en_US.UTF-8")))
    
    (add-hook 'elixir-mode-hook
              (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

Keybindings for compiling and running mix

    ; Build hook
    (add-hook 'elixir-mode-hook
              (lambda () (local-set-key (kbd "C-c b") 'mix-compile)))
    
    ; Run hook
    (add-hook 'elixir-mode-hook
              (lambda () (local-set-key (kbd "C-c r") 'mix-run)))

