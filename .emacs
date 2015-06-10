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


;; Speedbar in the same frame
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;
(require 'powerline)
(powerline-default-theme )

;; Treat .h files as c++ files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; CEDET - semantic
(require 'semantic)
(require 'semantic/bovine/gcc)
(semantic-mode 1)
; Automatic reparsing of open buffers
(global-semantic-idle-scheduler-mode 1)
;(global-semantic-idle-summary-mode 1)
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
; Company quickhelp
(require 'company-quickhelp)
(company-quickhelp-mode 1)

; Backend for irony
(require 'irony)
(eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Integración de irony con flycheck
(require 'flycheck)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;
(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)

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

;; Cmake-project-mode
(require 'cmake-project)
(add-hook 'cmake-mode-hook 'cmake-project-mode)
; Usar automáticamente cmake-project-mode si hay un CMakeLists.txt
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

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

;; IDO mode
(require 'ido)
(setq ido-enable-flex-matching 1)
(setq ido-everywhere t)
(ido-mode 1)

;; git-gutter (marca diferencias en la línea)
(require 'git-gutter)
(global-git-gutter-mode 1)
(git-gutter:linum-setup)

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
(require 'emms)
(emms-standard)
(emms-default-players)
(setq emms-source-file-default-directory "/home/saknussemm/Misc/musica")

;;
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

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
 '(cmake-project-default-build-dir-name "build/")
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "108b3724e0d684027c713703f663358779cc6544075bc8fd16ae71470497304f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "da7fa7211dd96fcf77398451e3f43052558f01b20eb8bee9ac0fd88627e11e22" "282606e51ef2811142af5068bd6694b7cf643b27d63666868bc97d04422318c1" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(doxymacs-doxygen-style "C++!")
 '(ecb-auto-activate t)
 '(ecb-layout-name "right2")
 '(ecb-options-version "2.40")
 '(ecb-tip-of-the-day nil)
 '(ede-project-directories (quote ("/home/saknussemm/projects/backend")))
 '(electric-pair-mode t)
 '(fci-dash-pattern 0.05)
 '(fci-rule-character-color "black")
 '(fci-rule-color "gray21")
 '(fci-rule-use-dashes t)
 '(font-use-system-font t)
 '(git-gutter:update-interval 2)
 '(global-hl-line-mode nil)
 '(global-linum-mode t)
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
