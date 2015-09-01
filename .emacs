;;; Emacs -- Init
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Para saber por qué tarda tanto en iniciar ( M-x benchmark-init )
(require 'benchmark-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cua mode
(cua-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recent files
(require 'recentf)
(recentf-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deshabilitar toolbar
(if window-system
    (tool-bar-mode -1)
)

;; Deshabilitar menubar (sigue siendo accesible)
;;(menu-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formato para números de línea
(require 'linum)
(defun my-activate-linum ()
  (linum-mode t))

(add-hook 'text-mode-hook 'my-activate-linum)
(add-hook 'prog-mode-hook 'my-activate-linum)
(add-hook 'cmake-mode-hook 'my-activate-linum)

;(global-linum-mode t)
(setq linum-format "%4d\u2502 ")

;; Whitespace cleaner
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Soportar mouse en xterm
(xterm-mouse-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trailing whitespace
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Follow symlinks (and display a warning)
(setq vc-follow-symlinks nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From https://snarfed.org/gnu_emacs_backup_files
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; IDO mode.
;; (require 'ido)
;; (setq ido-enable-flex-matching 1)
;; (setq ido-everywhere t)
;; (ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
(require 'helm-config)
(setq helm-split-window-in-side-p          t ; open Helm buffer inside current window
      helm-move-to-line-cycle-in-source    t
      helm-autoresize-mode                 t
      )

(helm-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speedbar in the same frame
;; (require 'sr-speedbar)
;; (global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color para los identificadores
(require 'rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
(require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(require 'powerline)
; Replaced by powerline-moe-theme
;(powerline-default-theme )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treat .h files as c++ files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; C style
(setq c-default-style "k&r"
      c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET - semantic
(require 'semantic)
(require 'semantic/bovine/gcc)
(semantic-mode 1)
; Automatic reparsing of open buffers
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 0)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(global-semantic-idle-local-symbol-highlight-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDE
; (global-ede-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECB
(require 'ecb)
(global-set-key (kbd "C-e") 'ecb-activate)
(setq ecb-show-sources-in-directories-buffer 'always)
; (setq ecb-auto-activate t)
(setq ecb-layout-name "right2")
(setq ecb-options-version "2.40")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format
(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion, company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company c-headers
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-c-headers))


;; Quickhelp
(require 'company-quickhelp)
(company-quickhelp-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Backend for irony
(require 'irony)
(eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; Irony headers
(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integración de irony con flycheck
(require 'flycheck)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(global-flycheck-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; No está andando (1jul15)
;(require 'flycheck-tip)
;(flycheck-tip-use-timer 'verbose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rtags
(require 'rtags)
(add-hook 'find-file-hook 'rtags-start-process-maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cmake-project-mode
(require 'cmake-project)
; Usar cmake-project-mode en cmakelists.txt
(add-hook 'cmake-mode-hook 'cmake-project-mode)
; Usar automáticamente cmake-project-mode si hay un CMakeLists.txt en este directorio
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt")
        (cmake-project-mode)
    ))

(add-hook 'prog-mode-hook 'maybe-cmake-project-hook)
(add-hook 'cmake-mode-hook 'cmake-project-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doxymacs
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
 (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
     (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(setq doxymacs-doxygen-style "C++!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP mode
(autoload 'php-mode "php-mode.el" "Php mode." t)
(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/.*\.py\'" . python-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git-gutter (marca diferencias en la línea)
(require 'git-gutter)
(global-git-gutter-mode 1)
(git-gutter:linum-setup)
(setq git-gutter:update-interval 2)

;; Mover entre ventanas
(require 'windmove)
(windmove-default-keybindings 'meta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnus
;; parts from https://github.com/dertuxmalwieder/My-Emacs-config/blob/master/.gnus.el
(require 'gnus)
(setq
      gnus-thread-sort-functions '(gnus-thread-sort-by-number (not gnus-thread-sort-by-date)) ; inverted sorting: newest threads on top
     
      gnus-mime-view-all-parts t    ; View all the MIME parts in current
      gnus-always-read-dribble-file 1 ; always read auto-save file
      gnus-use-cache t

      gnus-treat-buttonize t           ; Add buttons
      gnus-treat-buttonize-head 'head  ; Add buttons to the head
      gnus-treat-emphasize t           ; Emphasize text
      gnus-treat-display-smileys t     ; Use Smilies
      gnus-treat-strip-cr 'last        ; Remove carriage returns
      gnus-treat-hide-headers 'head    ; Hide headers

      gnus-boring-article-headers '(empty followup-to newsgroups many-to reply-to)
      gnus-treat-hide-boring-headers 'head ; -Hide boring headers
      ;gnus-fetch-old-headers 'some        ; prevent teared threads by loading older but read postings
      gnus-fetch-old-headers 250       ; this should achieve the same result, without the excessive waiting for some groups

                                       ; better summary line in the group overview
                                        ; inspired by http://eschulte.github.io/emacs-starter-kit/starter-kit-gnus.html
      gnus-sum-thread-tree-single-indent "* "
      gnus-sum-thread-tree-single-leaf "+-> "
      gnus-summary-display-arrow t
      gnus-select-method '(nntp "news.gmane.org")

      )

;; Eye candy 1

;; (setq
;;        gnus-summary-line-format "%0{%U%R%z%}%3{│%} %1{%d%} %3{│%}  %4{%-20,20f%}  %3{│%} %1{%B%}%s\n"
;;       ;;                                   ; better group lines too (no news server display & stuff)
;;       ;;                                   ; inspired by http://www.sopos.org/olli/?gnus
;;        gnus-group-line-format "%M\%S\%p\%P\%5y: %(%-40,40G%)\n"

;;       ;;                                   ; better topic lines too, similar to group lines
;;       ;;                                   ; inspired by http://ichimusai.org/pub/dot-gnus
;;       gnus-topic-line-format "%i %A: %(%{%n%}%) %v\n"
;; )

;; Eye Candy 2

(copy-face 'font-lock-variable-name-face 'gnus-face-6)
(setq gnus-face-6 'gnus-face-6)
(copy-face 'font-lock-constant-face 'gnus-face-7)
(setq gnus-face-7 'gnus-face-7)
(copy-face 'gnus-face-7 'gnus-summary-normal-unread)
(copy-face 'font-lock-constant-face 'gnus-face-8)
(set-face-foreground 'gnus-face-8 "gray50")
(setq gnus-face-8 'gnus-face-8)
(copy-face 'font-lock-constant-face 'gnus-face-9)
(set-face-foreground 'gnus-face-9 "gray70")
(setq gnus-face-9 'gnus-face-9)
(setq gnus-summary-make-false-root 'dummy)
(setq gnus-summary-make-false-root-always nil)
(defun oxy-unicode-threads () 
  (interactive)
  (setq gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{│%}%) %6{□%}  %S\n"
        gnus-summary-line-format "%0{%U%R%z%}%3{│%} %1{%d%} %3{│%}  %4{%-20,20f%}  %3{│%} %1{%B%}%s\n"
;	gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{│%} %6{%B%} %s\n"
	gnus-sum-thread-tree-indent " "
	gnus-sum-thread-tree-root "■ "
	gnus-sum-thread-tree-false-root "□ "
	gnus-sum-thread-tree-single-indent "▣ "
	gnus-sum-thread-tree-leaf-with-other "├─▶ "
	gnus-sum-thread-tree-vertical "│"
	gnus-sum-thread-tree-single-leaf "└─▶ "))

(defun oxy-unicode-threads-heavy () 
  (interactive)
  (setq gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
	gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
	gnus-sum-thread-tree-indent " "
	gnus-sum-thread-tree-root "┏● " 
	gnus-sum-thread-tree-false-root " ○ "
	gnus-sum-thread-tree-single-indent " ● "
	gnus-sum-thread-tree-leaf-with-other "┣━━❯ " 
	gnus-sum-thread-tree-vertical "┃"
	gnus-sum-thread-tree-single-leaf "┗━━❯ "))

(oxy-unicode-threads)


(add-hook 'gnus-article-display-hook 'gnus-article-highlight-citation t) ; highlight quotes
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)                        ; use topic separation in the Group overview

;; reconfigure buffer positions for a wider screen
(gnus-add-configuration  ; summary view
 '(summary
   (horizontal 1.0
               (vertical 1.0 (group 0.25) (summary 1.0 point)))))
(gnus-add-configuration  ; article view
 '(article
   (horizontal 1.0
               (vertical 0.45 (group 0.25) (summary 1.0 point) ("*BBDB*" 0.15))
               (vertical 1.0 (article 1.0)))))
(gnus-add-configuration  ; post new stuff
 '(edit-form
   (horizontal 1.0
               (vertical 0.45 (group 0.25) (edit-form 1.0 point) ("*BBDB*" 0.15))\\\\
               (vertical 1.0 (article 1.0)))))

;; (gnus-add-configuration  ; score editing
;;  '(edit-score
;;    (horizontal 1.0
;;                (vertical 0.45 (group 0.25) (edit-score 1.0 point) ("*BBDB*" 0.15))
;;                (vertical 1.0 (article 1.0)))))
;; (gnus-add-configuration  ; score tracing
;;  '(score-trace
;;    (horizontal 1.0
;;                (vertical 0.45 (group 0.25) (score-trace 1.0 point) ("*BBDB*" 0.15))
;;                (vertical 1.0 (article 1.0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentar cuando presione enter
;(define-key global-map (kbd "RET") 'newline-and-indent)

;; My shortcuts
(global-set-key [f8] 'compile)
(global-set-key [(control d)] 'comment-region)
; no funciona en terminal 
(global-set-key [(control D)] 'uncomment-region)
; (global-set-key [(control e)] 'ecb-toggle-ecb-windows)
(global-set-key [(shift f3)] 'helm-projectile-ag)
(global-set-key [f3] 'helm-occur-from-isearch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMS
;; (require 'emms)
;; (emms-standard)
;; (emms-default-players)
;; (setq emms-source-file-default-directory "/home/saknussemm/Misc/musica")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation window
(setq compilation-scroll-output t)
(setq ecb-compile-window-height-lines 10)
(setq ecb-compile-window-temporally-enlarge 'after-selection)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
(require 'moe-theme)
(moe-dark)
(moe-theme-set-color 'green)
(powerline-moe-theme)

;;
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-map)
;; (sublimity-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company-web
(add-to-list 'company-backends 'company-web-html)
(add-to-list 'company-backends 'company-web-jade)
(add-to-list 'company-backends 'company-web-slim)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9" ))
(set-face-attribute 'default t :font "DejaVu Sans Mono-9")

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
 '(custom-safe-themes
   (quote
    ("ab8033276aa563bc7373f78aeefef69e1e25083266b44116341f0a8096657463" "5d4ac3ecdba50acac7738e9718447c1f7aaa530e9100f84d966ee63d7d3cf760" "092c4b8fedd5b82fe906571b5e7a7e32730e22b04e1848a2b6fed1599fcbd410" "40b7687853f3d6921eba3afed50c532bbf4a66959554d32adf1899a675926b2d" "15bd21e2ca1741a213b6c2488437b44250e8049e6fbcd1ffbacda8cbde295dcb" "602d25a1dda761706a79a1705fe5af70ab69e91c75bda5d0449ac4e11d886374" "e33ddbd957dfe0a2e64732e4491adaf2a7508aa6ea2da193ac5a3feca3c7aeec" "bb55fede752d2b7280219c1a8d2399aa1b35166ae4f1119583bbf4af0d9a26d4" "1a85c54c4e6b310d530a1b6e71fe658cc3b7aac62a12146062418b5dc7da126c" "eaf4cb94ad96e1659f9254db8efb799deb1885e97884f8f971ff1e6a4114500a" "5d139820639cd941c60033dcdd462bf5fffa76da549e6bdf1d83945803d30f01" "5d8caed7f4ed8929fd79e863de3a38fbb1aaa222970b551edfd2e84552fec020" "75c0b1d2528f1bce72f53344939da57e290aa34bea79f3a1ee19d6808cb55149" "cc495c40747ae22dd2de6e250cbd9a408e3588b59989368af565deeeff334126" "d72836155cd3b3e52fd86a9164120d597cbe12a67609ab90effa54710b2ac53b" "3328e7238e0f6d0a5e1793539dfe55c2685f24b6cdff099c9a0c185b71fbfff9" "6184465774e662dc5f3ddb0751b20d97aaff2ae95d5cf3c885b6c1944ddcb371" "17f35b689dd41e49cb740bfb810ac8a53d13292cbebf68f41f772787d8b3aebf" "e7ec0cc3ce134cc0bd420b98573bbd339a908ac24162b8034c98e1ba5ee1f9f6" default)))
 '(doxymacs-doxygen-style "C++!")
 '(ecb-compile-window-height 6)
 '(ecb-layout-name "right2")
 '(ecb-options-version "2.40")
 '(ecb-source-path
   (quote
    (("/home/saknussemm/projects/backend/" "/home/saknussemm/projects/backend/")
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
 '(global-semantic-decoration-mode nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(rainbow-delimiters-max-face-count 1)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(tab-always-indent (quote complete))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
