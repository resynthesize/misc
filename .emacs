 ;; basic stuff before we get started
(add-to-list 'load-path "~/lisp")

;; for windows
;;(setenv "HOME" "d:/emacs")

;; shortcut keys
;;
;; fix delete and backspace to work properly
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [?\C-h] 'delete-backward-char)
(global-set-key [enter] 'newline-and-indent)

;; put function keys to good use!
(global-set-key [f1] 'help-command)
(global-set-key [f2] 'previous-multiframe-window)
(global-set-key [f3] 'next-multiframe-window)
(global-set-key [f4] 'font-lock-mode)
(global-set-key [f5] 'goto-line)
(global-set-key [f7] 'previous-error)
(global-set-key [f8] 'next-error)
(global-set-key [(shift f9)] 'dos2unix)
(global-set-key [f9] 'compile)
;; Open .emacs in buffer
(global-set-key [(shift f10)] 'open-dot-emacs)

;; Remap Home and End keys to move within current line, and C-Home and
;; C-End keys to beginning and end of buffer
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)

;; (global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region) 
(global-set-key "\C-h" 'perltidy-dwim)
;; Remap shift+up/down to scroll one line at a time
;;(global-set-key [\S-up] 'scroll-down-1)
;;(global-set-key [\S-down] 'scroll-up-1)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(define-key global-map '[(alt right)] 'my-next-buffer)
(define-key global-map '[(alt left)] 'my-previous-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings

;; Frame title bar formatting to show full path of file
(setq-default
 frame-title-format
 (list '((buffer-file-name "Emacs:  %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                  ("%b - Dir:  " default-directory)))))))
(setq-default
 icon-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                  ("%b - Dir:  " default-directory)))))))

;; don't add newlines at end of file
(setq next-line-add-newlines nil)

;; keep backups in home folder
(setq
 backup-by-copying t                    ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs_backups"))          ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)                     ; use versioned backups

(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))

(setq inhibit-startup-message t)

(require 'tramp)
(setq tramp-default-method "scp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font/Display Stuff
;; (global-font-lock-mode t)
(transient-mark-mode t)
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)
;; Don't wrap long lines when viewing
;;(hscroll-global-mode t)

(require 'color-theme)
;;(color-theme-tty-dark)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming stuff

;; turn on minibuffer history saving across sessions
(require 'savehist-20+)
(savehist-mode 1)

(defun my-c-mode-hook ()
  (setq c-basic-offset 4)
  (setq tab-width 4
        indent-tabs-mode nil)  ;; don't use tabs when indenting
  )
  
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(autoload 'php-mode "php-mode" "PHP editing mode" t)

(autoload 'perl-lint "perl-lint-mode" nil t)
(autoload 'perl-lint-mode "perl-lint-mode" nil t)

(defun drupal-mode ()
  (interactive)
  (php-mode)
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (c-set-offset 'case-label 2)
  (c-set-offset 'arglist-close 0))

(setq auto-mode-alist
      (append '(("\\.C$"    . c++-mode)
        ("\\.cc$"    . c++-mode)
        ("\\.cpp$"   . c++-mode)
        ("\\.cxx$"   . c++-mode)
        ("\\.hxx$"   . c++-mode)
        ("\\.h$"     . c++-mode)
        ("\\.hh$"    . c++-mode)
        ("\\.idl$"   . c++-mode)
        ("\\.ipp$"   . c++-mode)
        ("\\.c$"     . c-mode)
        ("\\.html$"  . html-mode)
        ("\\.tpl$"   . html-mode) ;; smarty template files
        ("\\.thtml$" . html-mode) ;; PHPLib template files
        ("\\.php$"   . drupal-mode) 
        ("\\.module$" . drupal-mode) 
        ("\\.inc$"   . drupal-mode)
        ("\\.js$"    . java-mode) 
        ("\\.pl$"    . perl-mode)
        ("\\.pm$"    . perl-mode)
        ("\\.PM$"    . perl-mode)
        ("\\.pmu$"    . perl-mode)
        ("\\.java$"  . java-mode)
        ("\\.txt$"   . text-mode))
          auto-mode-alist))
;;       cperl-indent-level 4
;; use cperl mode instead of perl 
(defalias 'perl-mode 'cperl-mode)
(setq cperl-invalid-face nil
      cperl-indent-level 4 
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t
      cperl-close-paren-offset -4)

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(display-time-mode t nil (time))
 '(hl-line-face (quote highlight))
 '(save-place t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(truncate-lines t))

;; turn off spaces

;; =====================================================================
;; MODE LINE
;; =====================================================================

;; I like to know what time it is. These lines show the clock in the
;; status bar. Comment out first line if you prefer to show time in 12
;; hour format
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; show column number in status bar
(setq column-number-mode t)

;; =====================================================================
;; custom variables set within emacs
;; =====================================================================

 '(global-visual-line-mode nil)

;; ==========================================================================================
;; functions
;; ==========================================================================================

(defun reload ()
  "reload ~/.emacs"
  (interactive)
  (load-file (expand-file-name "~/.emacs")))

(defun open-dot-emacs ()
  "opening-dot-emacs"
  (interactive) ; this makes the function a command too
  (find-file "~/.emacs"))

(defun dos2unix ()
  "Convert a buffer from dos to unix"
  (interactive)
  (let ((deactivate-mark-before deactivate-mark))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\r" nil t)(replace-match "")))
    (setq deactive-mark deactivate-mark-before)))

(defun scroll-up-1 ()
  (interactive)
  (scroll-up 1))

(defun scroll-down-1 ()
  (interactive)
  (scroll-down 1))

(defun my-previous-buffer ()
  "Cycle to the previous buffer with keyboard."
  (interactive)
  (bury-buffer))

(defun clear-shell-output ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer))) 

(defun w32-maximize-frame ()
  (interactive)
  "Maximize the current frame"
  (w32-send-sys-command ?\xf030))

;; (defun my-next-buffer ()
;;   "Cycle to the next buffer with keyboard."
;;   (interactive)
;;   (let* ((bufs (buffer-list))
;; 	 (entry (1- (length bufs)))
;; 	 val)
;;     (while (not (setq val (nth entry bufs)
;; 		      val (and (/= (aref (buffer-name val) 0)
;; 				   ?)
;; 			       val)))
;;       (setq entry (1- entry)))
;;     (switch-to-buffer val)))

(require 'recentf)
(recentf-mode 1)
(require 'csv-mode)
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )


(require 'perltidy)

;; show line numbers
(setq linum-format "%d| ")
(global-linum-mode 1)

(setq tidyall-cmd "/usr/bin/tidyall")

(defun tidyall-buffer ()
  "Run tidyall on the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (cond (file
           (if (buffer-modified-p)
               (save-buffer))
           (let* ((cmd (concat tidyall-cmd " --refresh-cache --output-suffix .tdy -m editor " file))
                  (tidyall-buffer (get-buffer-create "*tidyall-output*"))
                  (result (shell-command cmd tidyall-buffer))
                  (tidied-file (concat file ".tdy"))
                  (output (with-current-buffer tidyall-buffer (buffer-string)))
                  (window-positions (mapcar (lambda (w) (window-start w)) (window-list)))
                  (orig-point (point)))
             (when (string-match "[\t\n ]*$" output)
               (replace-match "" nil nil output))
             (cond ((zerop result)
                    (cond ((string-match "\\[tidied\\]" output)
                           (cond ((file-exists-p tidied-file)
                                  (erase-buffer)
                                  (insert-file-contents tidied-file)
                                  (delete-file tidied-file)
                                  (mapcar (lambda (w) (set-window-start w (pop window-positions))) (window-list))
                                  (goto-char orig-point)
                                  (save-buffer))
                                 (t
                                  (message (concat "Could not find '" tidied-file "'!")))))))
                   (t
                    (message nil)
                    (split-window-vertically)
                    (set-window-buffer (next-window) tidyall-buffer))))))))
