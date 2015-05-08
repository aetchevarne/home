(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Formato para números de línea
(setq linum-format "%3d\u2502 ")

;; Soportar mouse en xterm
(xterm-mouse-mode 1)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; CEDET - semantic
(semantic-mode 1)
; Automatic reparsing of open buffers
(global-semantic-idle-scheduler-mode 1)

;; EDE
(global-ede-mode 1)

;; clang-format
(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)

;; Completion, company
(add-hook 'after-init-hook 'global-company-mode)

;; Company c-headers
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-c-headers))

; Backend for irony
(eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Integración de irony con flycheck
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Irony 
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; Cmake-project-mode
(require 'cmake-project)
; Usar automáticamente cmake-project-mode si hay un CMakeLists.txt
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)


;; 
(autoload 'php-mode "php-mode.el" "Php mode." t)
(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))

;;
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/.*\.py\'" . python-mode)) auto-mode-alist))


;; My shortcuts
(global-set-key [f8] 'compile)
(global-set-key [(control d)] 'comment-region)
; no funciona en terminal 
(global-set-key [(control D)] 'uncomment-region)

;=================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("282606e51ef2811142af5068bd6694b7cf643b27d63666868bc97d04422318c1" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(ede-project-directories (quote ("/home/saknussemm/projects/backend")))
 '(electric-pair-mode t)
 '(font-use-system-font t)
 '(global-hl-line-mode nil)
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(tab-always-indent (quote complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal)))))
