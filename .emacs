;; basic stuff before we get started
(add-to-list 'load-path "~/lisp")
(let ((default-directory "~/.emacs.d"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; keyboard bindings
;;
;; fix delete and backspace to work properly
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [?\C-h] 'delete-backward-char)
(global-set-key [enter] 'newline-and-indent)

(global-set-key [f1] 'help-command)
(global-set-key [f4] 'font-lock-mode)
(global-set-key [f5] 'goto-line)

;; window splitting/switching
(global-set-key [f15] 'next-user-buffer)
(global-set-key [f16] 'split-window-vertically)
(global-set-key [f17] 'delete-window)
(global-set-key [f18] 'delete-other-windows)

;; Remap Home and End keys to move within current line, and C-Home and
;; C-End keys to beginning and end of buffer
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)

(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region) 
(global-set-key "\C-f" 'tidyall-buffer)
(global-set-key "\C-h" 'perltidy-dwim)
(global-set-key "\C-d" 'dired)
(global-set-key "\C-j" 'query-replace)
(global-set-key "\C-l" 'global-linum-mode)

(require 'color-theme)
(require 'color-theme-solarized)
(require 'savehist)
(require 'recentf)
(require 'perltidy)

(recentf-mode 1)
(ido-mode 1)
(global-linum-mode 1)
(savehist-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)


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

;; general settings
(setq 
 ;; don't add newlines at end of file
 next-line-add-newlines nil
 column-number-mode t
 ;; dont create backup files
 make-backup-files nil
 redisplay-dont-pause t
 inhibit-startup-message t
 tramp-default-method "scp"
 linum-format "%d ")

(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))

;; font stuff
(transient-mark-mode t)
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

(color-theme-solarized-dark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming stuff

(defun my-c-mode-hook ()
  (setq c-basic-offset 4)
  (setq tab-width 4
        indent-tabs-mode nil)  ;; don't use tabs when indenting
  )

(add-hook 'c-mode-common-hook 'my-c-mode-hook)
(autoload 'php-mode "php-mode" "PHP editing mode" t)

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

;; use cperl mode instead of perl 
(defalias 'perl-mode 'cperl-mode)
(setq cperl-invalid-face nil
      cperl-indent-level 4 
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t
      cperl-close-paren-offset -4)

;; reset the annoying reverse bold coloring of cperl 
;; arrays/hashes
(custom-set-faces
 '(cperl-array-face ((t (:weight normal))))
 '(cperl-hash-face ((t (:weight normal))))
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
 ;; Your init file should contain only one such instance.
 '(display-time-mode t nil (time))
 '(hl-line-face (quote highlight))
 '(save-place t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(truncate-lines t))

;; mode line
(setq 
 display-time-24hr-format t
 display-time-day-and-date t
 column-number-mode t)

(display-time)

;; functions
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

(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

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

(defun named-shell (name)
  "Open up a shell buffer with the requested name (delimited by '*'). If blank string given, defaults to *shell*."
  (interactive
   (let ((string (read-string "Shell buffer name: " nil)))
     (list string)))
  (if (string= name "")
      (setq name "*shell*")
    (setq name (concat "*" name "*")))
  (let ((original-shell-buffer (get-buffer "*shell*")))
    (cond (original-shell-buffer
           (rename-other-buffer original-shell-buffer "*temp*" t)))
    (let ((new-shell-buffer (shell)))
      (cond ((and (string= name "*shell*") original-shell-buffer)
             (rename-other-buffer new-shell-buffer "*temp*" t)
             (rename-other-buffer original-shell-buffer "*shell*")
             (rename-other-buffer new-shell-buffer "*shell*" t))
            (t
             (if (not (string= name "*shell*"))
                 (rename-other-buffer new-shell-buffer name t))
             (if original-shell-buffer
                 (rename-other-buffer original-shell-buffer "*shell*"))))
      new-shell-buffer)))

(fset 'clear-shell-buffer
      "\C-[ \C-w\C-m\C-p\C-k\C-e")

(fset 'clear-shell-buffer-and-repeat-command
      "\C-[ \C-w\C-m\C-p\C-k\C-e\C-[Pcu\C-m")

(global-set-key "\M-s"       'named-shell)

(add-hook 'dired-mode-hook
          '(lambda ()
             (local-unset-key "\M-s")
             ))

(setq comint-mode-hook
      '(lambda ()
	 (local-set-key "\M-c" 'clear-shell-buffer)
	 (local-set-key "\M-r" 'clear-shell-buffer-and-repeat-command)
	 (local-unset-key "\M-s")
	 (set-variable 'scroll-conservatively 0)
	 ))
