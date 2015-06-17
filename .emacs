;;; Emacs -- Init
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Formato para números de línea
(require 'linum)
(global-linum-mode t)
(setq linum-format "%4d\u2502 ")

;; Soportar mouse en xterm
(xterm-mouse-mode 1)

;; From https://snarfed.org/gnu_emacs_backup_files
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)


;; ;; IDO mode.
;; (require 'ido)
;; (setq ido-enable-flex-matching 1)
;; (setq ido-everywhere t)
;; (ido-mode 1)

;; Helm
(require 'helm-config)
(setq helm-split-window-in-side-p          t ; open Helm buffer inside current window
      helm-move-to-line-cycle-in-source    t
      helm-autoresize-mode                 t
      )

(helm-mode t)

;; Speedbar in the same frame
;; (require 'sr-speedbar)
;; (global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Color para los identificadores
(require 'rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

;; Flycheck
(require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;;
(require 'powerline)
; Replaced by powerline-moe-theme
;(powerline-default-theme )

;; Treat .h files as c++ files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; C style
(setq c-default-style "k&r"
      c-basic-offset 4)

;; CEDET - semantic
(require 'semantic)
(require 'semantic/bovine/gcc)
(semantic-mode 1)
; Automatic reparsing of open buffers
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 0)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(global-semantic-idle-local-symbol-highlight-mode 1)

;; EDE
; (global-ede-mode 1)

;; ECB
(require 'ecb)
(global-set-key (kbd "C-e") 'ecb-activate)
(setq ecb-show-sources-in-directories-buffer 'always)
(setq ecb-auto-activate t)
(setq ecb-layout-name "right2")
(setq ecb-options-version "2.40")


;; Projectile
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Integrate ecb - projectile
(defvar default-ecb-source-path (list  '("~/" "~/")
                                       '("/" "/")))
(add-hook 'ecb-basic-buffer-sync-hook
          (lambda ()
            (when (functionp 'projectile-get-project-directories)
              (when (projectile-project-p)
                (dolist (path-dir (projectile-get-project-directories))
                  (unless (member (list path-dir path-dir) default-ecb-source-path)
                    (push (list path-dir path-dir) default-ecb-source-path)
                    (customize-set-variable 'ecb-source-path default-ecb-source-path)
                    ))))))

;; clang-format
(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)

;; Completion, company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Company c-headers
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-c-headers))

; Backend for irony
(require 'irony)
(eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Irony 
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Irony-eldoc
(add-hook 'irony-mode-hook 'irony-eldoc)

;; Integración de irony con flycheck
(require 'flycheck)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(global-flycheck-mode t)

;;
(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)

;; Cmake-project-mode
(require 'cmake-project)
; Usar cmake-project-mode en cmakelists.txt
(add-hook 'cmake-mode-hook 'cmake-project-mode)
; Usar automáticamente cmake-project-mode si hay un CMakeLists.txt en este directorio
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'prog-mode-hook 'maybe-cmake-project-hook)

(add-hook 'cmake-mode-hook 'cmake-project-mode)

;; Doxymacs
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
 (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
     (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; PHP mode
(autoload 'php-mode "php-mode.el" "Php mode." t)
(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))

;; Python mode
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/.*\.py\'" . python-mode)) auto-mode-alist))

;; git-gutter (marca diferencias en la línea)
(require 'git-gutter)
(global-git-gutter-mode 1)
(git-gutter:linum-setup)
(setq git-gutter:update-interval 2)

;; Gnus
(setq gnus-select-method '(nntp "news.gmane.org"))

;; Indentar cuando presione enter
;(define-key global-map (kbd "RET") 'newline-and-indent)

;; My shortcuts
(global-set-key [f8] 'compile)
(global-set-key [(control d)] 'comment-region)
; no funciona en terminal 
(global-set-key [(control D)] 'uncomment-region)
(global-set-key [(control e)] 'ecb-toggle-ecb-windows)

;; EMMS
;; (require 'emms)
;; (emms-standard)
;; (emms-default-players)
;; (setq emms-source-file-default-directory "/home/saknussemm/Misc/musica")

;; Compilation window
(setq compilation-scroll-output t)
(setq ecb-compile-window-height-lines 10)
(setq ecb-compile-window-temporally-enlarge 'after-selection)
;;
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Theme
(require 'moe-theme)
(moe-dark)
(moe-theme-set-color 'w/b)
(powerline-moe-theme)

;;
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 %b))))

;=================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote aggressive))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(cmake-project-default-build-dir-name "build/")
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (moe-dark)))
 '(doxymacs-doxygen-style "C++!")
 '(ecb-auto-activate t)
 '(ecb-compile-window-height 6)
 '(ecb-layout-name "right2")
 '(ecb-options-version "2.40")
 '(ecb-source-path
   (quote
    (("/home/saknussemm/Misc/home/" "/home/saknussemm/Misc/home/")
     ("~/" "~/")
     ("/" "/"))))
 '(ecb-tip-of-the-day nil)
 '(ede-project-directories (quote ("/home/saknussemm/projects/backend")))
 '(electric-pair-mode t)
 '(fci-dash-pattern 0.05)
 '(fci-rule-character-color "black")
 '(fci-rule-color "gray21")
 '(fci-rule-use-dashes t)
 '(global-hl-line-mode nil)
 '(global-linum-mode t)
 '(global-semantic-decoration-mode nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(paradox-github-token t)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(tab-always-indent (quote complete)))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal)))))
