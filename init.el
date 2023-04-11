;;; -*- lexical-binding: t -*-
;;; Code:
(require 'package)
(defun do-after-load-evaluation (abs-file)
  (dolist (a-l-element after-load-alist)
    (when (and (stringp (car a-l-element))
               (string-match-p (car a-l-element) abs-file))
      (mapc #'funcall (cdr a-l-element))))
  (run-hook-with-args 'after-load-functions abs-file))

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/Projects/crdt")

(tool-bar-mode -1)
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(scroll-bar-mode -1)
(setq gc-cons-threshold 8000000 gc-cons-percentage 0.25)
(setq-default garbage-collection-messages t)
(setq-default inhibit-startup-message t)

(setq kill-ring-max 5000 kill-whole-line t)
(setq-default indent-tabs-mode nil)
(global-subword-mode)
(setq-default isearch-lazy-count t)
(save-place-mode)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(delete-selection-mode)

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)

;; Sudo edit better default dir
(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory
                  (replace-regexp-in-string "^/sudo:root@localhost:" "" default-directory))))

(when (eq window-system 'ns)
  (setq default-frame-alist (append
                             '((ns-transparent-titlebar . t)
                               (ns-appearance . dark))
                             default-frame-alist))
  (setq frame-title-format nil)
  (setq ns-use-proxy-icon nil)
  (set-fontset-font t 'symbol "Apple Color Emoji" nil 'append))

;; Bootstrap required packages
(defvar k-packages
  '(embark org-contrib exwm company-posframe orderless
           pyim-cangjiedict pyim projectile stripes vertico consult
           embark-consult haskell-mode selectrum-prescient
           marginalia selectrum slime-company slime crdt impatient-mode
           comment-dwim-2 sudo-edit csv-mode zygospore yasnippet ws-butler
           volatile-highlights vlf use-package undo-tree
           system-packages smtpmail-multi showtip
           rainbow-mode racket-mode pyim-basedict proof-general posframe
           pdf-tools paxedit ox-reveal org-static-blog org-present
           nasm-mode multi-vterm mmm-mode magit iedit highlight-parentheses highlight-indent-guides
           goto-last-change gnu-apl-mode geiser flycheck emms-soundcloud
           elnode dtrt-indent ctable comment-or-uncomment-sexp
           clean-aindent-mode cdlatex lsp-ltex bug-hunter buffer-move
           auto-highlight-symbol auctex anzu aggressive-indent topsy
           adjust-parens ace-link 2048-game ytel all-the-icons cider
           poly-org engrave-faces enwc))
(let ((to-install (cl-remove-if #'package-installed-p k-packages)))
  (when to-install
    (message "%s packages to be installed." (length to-install))
    (package-refresh-contents)
    (dolist (package to-install)
      (ignore-errors (package-install package)))))

(setq custom-file "~/.emacs.d/custom/custom.el")
(load custom-file)

;;; company

(require 'company)
(setq-default company-backends '(company-capf company-files))
(setq-default company-dabbrev-downcase nil)
(setq-default company-tooltip-align-annotations t
              company-format-margin-function nil)

;; Zebra strips, to look consistent with vertico
;; Patch `company--create-lines' and `company-fill-propertize'
(defvar k--company-current-index)
;; (pkg-info-version-info 'company)
;; "0.9.13 (package: 20220825.1044)"
(defun company--create-lines (selection limit)
  (let ((len company-candidates-length)
        (window-width (company--window-width))
        left-margins
        left-margin-size
        lines
        width
        lines-copy
        items
        previous
        remainder
        scrollbar-bounds)

    ;; Maybe clear old offset.
    (when (< len (+ company-tooltip-offset limit))
      (setq company-tooltip-offset 0))

    (let ((selection (or selection 0)))
      ;; Scroll to offset.
      (if (eq company-tooltip-offset-display 'lines)
          (setq limit (company-tooltip--lines-update-offset selection len limit))
        (company-tooltip--simple-update-offset selection len limit))

      (cond
       ((eq company-tooltip-offset-display 'scrollbar)
        (setq scrollbar-bounds (company--scrollbar-bounds company-tooltip-offset
                                                          limit len)))
       ((eq company-tooltip-offset-display 'lines)
        (when (> company-tooltip-offset 0)
          (setq previous (format "...(%d)" company-tooltip-offset)))
        (setq remainder (- len limit company-tooltip-offset)
              remainder (when (> remainder 0)
                          (setq remainder (format "...(%d)" remainder)))))))

    (when selection
      (cl-decf selection company-tooltip-offset))

    (setq width (max (length previous) (length remainder))
          lines (nthcdr company-tooltip-offset company-candidates)
          len (min limit len)
          lines-copy lines)

    (when scrollbar-bounds (cl-decf window-width))

    (when company-format-margin-function
      (let ((lines-copy lines-copy)
            res)
        (dotimes (i len)
          (push (funcall company-format-margin-function
                         (pop lines-copy)
                         (equal selection i))
                res))
        (setq left-margins (nreverse res))))

    ;; XXX: format-function outputting shorter strings than the
    ;; default margin is not supported (yet?).
    (setq left-margin-size (apply #'max company-tooltip-margin
                                  (mapcar #'length left-margins)))

    (cl-decf window-width company-tooltip-margin)
    (cl-decf window-width left-margin-size)

    (dotimes (_ len)
      (let* ((value (pop lines-copy))
             (annotation (company-call-backend 'annotation value))
             (left (or (pop left-margins)
                       (company-space-string left-margin-size))))
        (setq value (company--clean-string value))
        (when annotation
          (setq annotation (company--clean-string annotation))
          (when company-tooltip-align-annotations
            ;; `lisp-completion-at-point' adds a space.
            (setq annotation (string-trim-left annotation))))
        (push (list value annotation left) items)
        (setq width (max (+ (length value)
                            (if (and annotation company-tooltip-align-annotations)
                                (1+ (length annotation))
                              (length annotation)))
                         width))))

    (setq width (min window-width
                     company-tooltip-maximum-width
                     (max company-tooltip-minimum-width
                          (if company-show-quick-access
                              (+ 2 width)
                            width))))

    (when company-tooltip-width-grow-only
      (setq width (max company--tooltip-current-width width))
      (setq company--tooltip-current-width width))

    (let ((items (nreverse items))
          (row (if company-show-quick-access 0 99999))
          new)
      (when previous
        (push (company--scrollpos-line previous width left-margin-size) new))

      (dotimes (i len)
        (let* ((item (pop items))
               (str (car item))
               (annotation (cadr item))
               (left (nth 2 item))
               (right (company-space-string company-tooltip-margin))
               (width width)
               (selected (equal selection i)))
          (when company-show-quick-access
            (let ((quick-access (gv-ref (if (eq company-show-quick-access 'left)
                                            left right)))
                  (qa-hint (company-tooltip--format-quick-access-hint
                            row selected)))
              (cl-decf width (string-width qa-hint))
              (setf (gv-deref quick-access)
                    (concat qa-hint (gv-deref quick-access))))
            (cl-incf row))
          (setq k--company-current-index i) ;; OUR CHANGE
          (push (concat
                 (company-fill-propertize str annotation
                                          width selected
                                          left
                                          right)
                 (when scrollbar-bounds
                   (company--scrollbar i scrollbar-bounds)))
                new)))

      (when remainder
        (push (company--scrollpos-line remainder width left-margin-size) new))

      (cons
       left-margin-size
       (nreverse new)))))

(defun company-fill-propertize (value annotation width selected left right)
  (let* ((margin (length left))
         (company-common (and company-common (company--clean-string company-common)))
         (common (company--common-or-matches value))
         (_ (setq value (company-reformat (company--pre-render value))
                  annotation (and annotation (company--pre-render annotation t))))
         (ann-ralign company-tooltip-align-annotations)
         (ann-truncate (< width
                          (+ (length value) (length annotation)
                             (if ann-ralign 1 0))))
         (ann-start (+ margin
                       (if ann-ralign
                           (if ann-truncate
                               (1+ (length value))
                             (- width (length annotation)))
                         (length value))))
         (ann-end (min (+ ann-start (length annotation)) (+ margin width)))
         (line (concat left
                       (if (or ann-truncate (not ann-ralign))
                           (company-safe-substring
                            (concat value
                                    (when (and annotation ann-ralign) " ")
                                    annotation)
                            0 width)
                         (concat
                          (company-safe-substring value 0
                                                  (- width (length annotation)))
                          annotation))
                       right)))
    (setq width (+ width margin (length right)))

    (font-lock-append-text-property 0 width 'mouse-face
                                    'company-tooltip-mouse
                                    line)
    (when (< ann-start ann-end)
      (add-face-text-property ann-start ann-end
                              (if selected
                                  'company-tooltip-annotation-selection
                                'company-tooltip-annotation)
                              t line))
    (cl-loop
     with width = (- width (length right))
     for (comp-beg . comp-end) in common
     for inline-beg = (+ margin comp-beg)
     for inline-end = (min (+ margin comp-end) width)
     when (< inline-beg width)
     do (add-face-text-property inline-beg inline-end
                                (if selected
                                    'company-tooltip-common-selection
                                  'company-tooltip-common)
                                nil line))
    (when (let ((re (funcall company-search-regexp-function
                             company-search-string)))
            (and (not (string= re ""))
                 (string-match re value)))
      (pcase-dolist (`(,mbeg . ,mend) (company--search-chunks))
        (let ((beg (+ margin mbeg))
              (end (+ margin mend))
              (width (- width (length right))))
          (when (< beg width)
            (add-face-text-property beg (min end width)
                                    (if selected
                                        'company-tooltip-search-selection
                                      'company-tooltip-search)
                                    nil line)))))
    (when selected
      (add-face-text-property 0 width 'company-tooltip-selection t line))

    (when (company-call-backend 'deprecated value)
      (add-face-text-property margin
                              (min
                               (+ margin (length value))
                               (- width (length right)))
                              'company-tooltip-deprecated t line))

    ;; OUR CHANGE
    (if (cl-evenp k--company-current-index)
        (add-face-text-property 0 width 'company-tooltip t line)
      (add-face-text-property 0 width 'k-zebra t line))
    line))

(byte-compile 'company--create-lines)
(byte-compile 'company-fill-propertize)

;; Use posframe so that company works in minibuffer...
(require 'company-posframe)
(setq-default company-posframe-show-indicator nil
              company-posframe-show-metadata nil
              company-posframe-quickhelp-show-header nil
              company-posframe-quickhelp-delay nil
              company-posframe-show-params '(:internal-border-width 1)
              company-posframe-quickhelp-show-params
              '(:poshandler company-posframe-quickhelp-right-poshandler
                            :internal-border-width 1))
(advice-add 'company-posframe-show :after (lambda () (company-posframe-quickhelp-show)))

(company-posframe-mode)
(global-company-mode)

;; Don't let `company-elisp' quickhelp hijack `*Help*' buffer
(defvar k-help-buffer-override nil)
(defun k--company-help-buffer-advice (orig &rest args)
  (let ((k-help-buffer-override "*company-documentation*"))
    (apply orig args)))
(advice-add 'company-capf :around 'k--company-help-buffer-advice)
(defun k--help-buffer-advice (orig)
  (or (when k-help-buffer-override
        (get-buffer-create k-help-buffer-override))
      (funcall orig)))
(advice-add 'help-buffer :around 'k--help-buffer-advice)

;;; Projectile

(require 'projectile)
(projectile-mode)
(setq projectile-enable-caching t)
(defun k--projectile-find-file ()
  "Find file in project.
Prompt for project if not currently in one."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (projectile-switch-project)))

;; show whitespace in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode)))

;;; Util functions

(defmacro globalize (mode)
  "Define and enable a global minor mode from minor MODE."
  (let ((%global-mode-symbol (intern (concat "global-" (symbol-name mode)))))
    `(progn
       (define-globalized-minor-mode ,%global-mode-symbol ,mode
         (lambda () (,mode)))
       (,%global-mode-symbol))))

(defun k-exwm-enabled-p ()
  (member #'exwm--server-stop kill-emacs-hook))

(defun delete-from-list (list-var element)
  (set list-var (delete element (symbol-value list-var))))

(defmacro k-quote (&rest args) `',args)

;;; Generic stripes
;; I prefer using text-property to color stuff,
;; but when I don't feel like trying I use `stripes' overlays.
(require 'stripes)
(require 'hl-line)
(setq-default stripes-unit 1 stripes-overlay-priority 0
              hl-line-overlay-priority 5)
(add-hook 'emms-playlist-mode-hook 'stripes-mode)
(add-hook 'emms-playlist-mode-hook 'hl-line-mode)
;; Patch `hl-line-make-overlay' so that front advance is T
(defun hl-line-make-overlay ()
  (let ((ol (make-overlay (point) (point) nil t nil)))
    (overlay-put ol 'priority hl-line-overlay-priority) ;(bug#16192)
    (overlay-put ol 'face hl-line-face)
    ol))

;;; Theme

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setenv "GREP_COLOR" "31")
(setq-default k-color-style 'bright)
;; (setq-default k-color-style 'dark)
(load-theme 'k t)
(defun k-theme-switch (style)
  "Elegantly switch to k-theme with STYLE."
  (interactive
   (list (intern (completing-read "Style: " '(bright dark) nil t))))
  (let (fix-highlight-indent-guides)
    (setq k-color-style style)
    (highlight-tail-mode 0)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when highlight-indent-guides-mode
          (highlight-indent-guides-mode 0)
          (push buffer fix-highlight-indent-guides))))
    (load-theme 'k t)
    (load-theme 'k t)
    (dolist (buffer fix-highlight-indent-guides)
      (with-current-buffer buffer
        (highlight-indent-guides-mode)))
    (highlight-tail-mode)))

(require 'shr)
(let ((fringe-width (/ (* (shr-string-pixel-width "o") 4) 3)))
  (setq default-frame-alist (append
                             `((left-fringe . ,fringe-width)
                               (right-fringe . ,fringe-width))
                             default-frame-alist)))

;;; Misc packages

(require 'all-the-icons)
(setq-default all-the-icons-faicon-scale-factor 0.7
              all-the-icons-default-faicon-adjust 0.35
              all-the-icons-material-scale-factor 0.8
              all-the-icons-default-material-adjust 0.1)

(require 'volatile-highlights)
(volatile-highlights-mode)

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

(require 'dtrt-indent)
(add-hook 'prog-mode-hook 'dtrt-indent-mode)
(setq dtrt-indent-verbosity 0)

(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)
(add-hook 'text-mode 'ws-butler-mode)
(add-hook 'fundamental-mode 'ws-butler-mode)

(global-set-key (kbd "M-;") 'comment-dwim-2)

(require 'iedit)
(global-set-key (kbd "C-;") #'iedit-mode)

(require 'outline)
(define-key outline-minor-mode-map (kbd "C-<tab>") 'outline-toggle-children)
(setq-default outline-minor-mode-buttons '(("▶" "▼" outline--valid-char-p)))

(require 'vlf-setup)
(setq vlf-application 'dont-ask)

(require 'highlight-parentheses)
(setq hl-paren-colors '(nil))
(set-face-attribute 'hl-paren-face nil :inherit 'show-paren-match)
(show-paren-mode)
(globalize highlight-parentheses-mode)

(require 'topsy)
(setq topsy-header-line-format `(:eval (k-pad-mode-line-format (funcall topsy-fn))))
(add-hook 'prog-mode-hook #'topsy-mode)

;; Customized functions
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Killed line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; kill a line, including whitespace characters until next non-whiepsace character
;; of next line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; taken from prelude-editor.el
;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode slime-repl-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

;; prelude-core.el
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; prelude-editing.el
(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)
(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (if (member major-mode prelude-indent-sensitive-modes)
      (error "Buffer is indent-sensitive")
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;;; Echo per window

(defvar-local k--top-separator-ov nil)

(define-minor-mode k-echo-area-mode "Per window echo area."
  :lighter nil
  (if k-echo-area-mode
      (save-excursion
        (setq-local overline-margin 0)
        (goto-char (point-min))
        (vertical-motion (cons (1- (window-text-width)) 0))
        (if k--top-separator-ov
            (move-overlay k--top-separator-ov (point-min) (point))
          (setq k--top-separator-ov (make-overlay (point-min) (point) nil t t))
          (overlay-put k--top-separator-ov 'face
                       '(:overline "#000000"))
          (overlay-put k--top-separator-ov 'after-string
                       #(" " 0 1 (display (space :align-to right) face (:overline "#000000"))))))))

(defun k-window-echo-area--map (function &optional buffer)
  (dolist (frame (frame-list))
    (dolist (window (window-list frame 'none))
      (let ((buf (window-buffer window)))
        (when (and (or (not buffer) (eq (get-buffer buffer) buf))
                   (buffer-local-value 'k-echo-area-mode buf))
          (funcall function window
                   (when-let ((root (window-atom-root window))
                              (parent (window-child root)))
                     (and (window-live-p parent) parent))))))))

(defun k-window-echo-area-clear (&optional buffer)
  (save-selected-window
    (k-window-echo-area--map
     (lambda (window parent)
       (when parent
         (set-window-parameter parent 'mode-line-format nil))
       (set-window-parameter window 'window-atom nil)
       (delete-window window))
     buffer)))

(defun k-window-echo-area-display (buf)
  (k-window-echo-area-clear)
  (save-selected-window
    (when (minibufferp (window-buffer))
      (select-window (minibuffer-selected-window)))
    (let (height window)
      (set-window-parameter nil 'mode-line-format 'none)
      (with-current-buffer buf
        (k-echo-area-mode)
        (setq-local mode-line-format
                    (s-replace "%" "%%" (format-mode-line (buffer-local-value 'mode-line-format (window-buffer)))))
        (setq height (count-screen-lines)))
      (setq window
            (display-buffer buf
                            `(display-buffer-in-atom-window
                              (window-height . ,(lambda (window)
                                                  (set-window-text-height window (max height 1))))
                              (dedicated . t))))
      window)))

(defvar k-message nil)
(defun k-message (format-string &rest args)
  (if (minibufferp (window-buffer))
      (apply #'message format-string args)
    (if format-string
        (setq k-message (apply #'format format-string args))
      (setq k-message nil))))
(defun k-message-display ()
  (with-current-buffer (get-buffer-create " *echo per window*")
    (if k-message
        (progn
          (setq-local cursor-type nil)
          (delete-region (point-min) (point-max))
          (insert k-message)
          (set-window-parameter
           (k-window-echo-area-display (current-buffer))
           'no-other-window t))
      (k-window-echo-area-clear (current-buffer)))))
(add-hook 'post-command-hook #'k-message-display)
(add-hook 'echo-area-clear-hook '(lambda () (k-message nil)))
(add-hook 'window-configuration-change-hook '(lambda () (k-message nil)))
(setq eldoc-message-function 'k-message)

;;; Flycheck and LSP

(require 'flycheck)
(defun k-flycheck-display-error-messages (errors)
  (k-message (flycheck-help-echo-all-error-messages errors)))
(advice-remove 'flycheck-hide-error-buffer
               (lambda ()
                 (unless (flycheck-overlays-at (point))
                   (k-message-hide))))
(setq flycheck-display-errors-function #'k-flycheck-display-error-messages)
(global-flycheck-mode)
(setq-default flycheck-indication-mode nil)
(advice-add 'flycheck-jump-to-error :before
            (lambda (_error)
              (unless (get-char-property (point) 'flycheck-error)
                (push-mark))))

;;; Languagetool LSP

(setq-default lsp-headerline-breadcrumb-enable nil
              lsp-keymap-prefix "<f2>")
(require 'lsp)
(require 'lsp-ltex)
(setq lsp-ltex-version "15.2.0"
      lsp-ltex-latex-environments '(("mathpar" . "ignore"))
      lsp-ltex-latex-commands '(("\\lstset{}" . "ignore")))

;;; LaTeX

(require 'latex)
;; to use pdfview with auctex
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t) ;; not sure if last line is neccessary

;; to have the buffer refresh after compilation
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(add-hook 'LaTeX-mode-hook 'init-latex)
(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
(add-to-list 'TeX-command-list '("PDFLaTeX" "%`pdflatex -shell-escape%(mode)%' %t" TeX-run-TeX nil t))
(add-to-list 'TeX-command-list '("Preview" "" (lambda (&rest args) (preview-document)) nil t))
(add-to-list 'TeX-command-list '("Unpreview" "" (lambda (&rest args) (preview-clearout-document)) nil t))
(setq TeX-command-default "PDFLaTeX")
(setq TeX-save-query  nil )
(defun init-latex ()
  (LaTeX-add-environments
   '("mathpar" LaTeX-env-label))
  (cdlatex-mode)
  (visual-line-mode)
  (unless polymode-mode (lsp-deferred)))
(setq-default TeX-master "main")

(setq-default visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(require 'texmathp)
(add-to-list 'texmathp-tex-commands '("mathpar" env-on))
(texmathp-compile)

(require 'cdlatex)
(setq cdlatex-math-symbol-alist '((42 ("\\times" "\\product")) (43 ("\\cup" "\\sum"))))
(turn-on-cdlatex)

;;; Completion

;; (pkg-info-version-info 'vertico)
;; "0.27"
(require 'vertico)

(require 'vertico-buffer)
(defvar-local vertico--buffer-window nil)
(defun vertico--buffer-resize-window (height)
  (when k--top-separator-ov
    (overlay-put k--top-separator-ov 'after-string nil))
  (let ((string (overlay-get vertico--candidates-ov 'after-string)))
    (put-text-property 0 1 'display '(space :align-to right) string)
    (put-text-property 0 1 'face '(:overline "#000000") string))
  (let ((string (overlay-get vertico--count-ov 'before-string)))
    (add-face-text-property 0 (length string) '(:overline "#000000") nil string))
  (set-window-text-height vertico--buffer-window (+ 1 height)))

(defun vertico--format-count ()
  "Format the count string."
  (concat
   (when (> (recursion-depth) 1)
     (propertize (format "%s " (recursion-depth)) 'face 'warning))
   (format "%-6s "
           (format #("%s/%s" 0 2 (face success))
                   (cond ((>= vertico--index 0) (1+ vertico--index))
                         ((vertico--allow-prompt-selection-p) "*")
                         (t "!"))
                   vertico--total))))
(defun k-minibuffer-message-advice (orig-func message &rest args)
  (when vertico--input
    (setq message (substring message))
    (add-face-text-property 0 (length message) '(:overline "#000000") nil message))
  (apply orig-func message args))
(advice-add 'minibuffer-message :around #'k-minibuffer-message-advice)
(defun vertico-buffer--setup ()
  "Setup buffer display."
  (setq k-message nil)
  (add-hook 'pre-redisplay-functions 'vertico-buffer--redisplay nil 'local)
  (setq-local overline-margin 0
              fringe-indicator-alist '((truncation nil nil)))
  (let* (win (buf (current-buffer))
             (_ (unwind-protect
                    (setf
                     win (with-minibuffer-selected-window
                           (k-window-echo-area-display buf))
                     vertico--buffer-window win)))
             (sym (make-symbol "vertico-buffer--destroy"))
             (depth (recursion-depth))
             (now (window-parameter win 'no-other-window))
             (ndow (window-parameter win 'no-delete-other-windows)))
    (fset sym (lambda ()
                (k-window-echo-area-clear buf)
                (when (= depth (recursion-depth))
                  (with-selected-window (active-minibuffer-window)
                    (when (window-live-p win)
                      (set-window-parameter win 'no-other-window now)
                      (set-window-parameter win 'no-delete-other-windows ndow))
                    (when vertico-buffer-hide-prompt
                      (set-window-vscroll nil 0))
                    (remove-hook 'minibuffer-exit-hook sym)))))
    ;; NOTE: We cannot use a buffer-local minibuffer-exit-hook here.
    ;; The hook will not be called when abnormally exiting the minibuffer
    ;; from another buffer via `keyboard-escape-quit'.
    (add-hook 'minibuffer-exit-hook sym)
    (set-window-parameter win 'no-other-window t)
    (set-window-parameter win 'no-delete-other-windows t)
    (overlay-put vertico--candidates-ov 'window win)
    (when (and vertico-buffer-hide-prompt vertico--count-ov)
      (overlay-put vertico--count-ov 'window win))
    (setq-local show-trailing-whitespace nil
                truncate-lines t
                cursor-in-non-selected-windows 'box)))
(vertico-buffer-mode)
(advice-remove 'vertico--resize-window #'ignore)
(advice-add 'vertico--resize-window :override #'vertico--buffer-resize-window)

(require 'marginalia)
;; (pkg-info-version-info 'marginalia)
;; "20220914.945"
;; Automatically give more generous field width
(setq-default marginalia-field-width 48)
(defun marginalia--affixate (metadata annotator cands)
  "Affixate CANDS given METADATA and Marginalia ANNOTATOR."
  ;; reset `marginalia--candw-max'
  (let* ((width (cl-loop for win in (get-buffer-window-list) minimize (window-width win)))
         ;; estimate width
         (marginalia-field-width
          (- (floor (* width 0.8))
             (let ((max (cl-loop for cand in cands
                                 maximize (string-width cand))))
               (* (ceiling (or max 0) marginalia--candw-step) marginalia--candw-step))))
         (marginalia--metadata metadata))
    (setq-local marginalia--candw-max (default-value 'marginalia--candw-max))
    (marginalia--align
     (with-selected-window (or (minibuffer-selected-window) (selected-window))
       (cl-loop for cand in cands collect
                ;; don't use `marginalia--cache' because we change width dynamically
                (let ((ann (or (funcall annotator cand) "")))
                  (cons cand (if (string-blank-p ann) "" ann))))))))
(byte-compile 'marginalia--affixate)

;; Multiline candidates
;; Don't collapse multiline into single line.
;; I find this reads much better for, say, `yank-pop'

;; (emacs-version)
;; "GNU Emacs 28.2 (build 1, x86_64-apple-darwin21.5.0, NS appkit-2113.50 Version 12.4 (Build 21F79))
;;  of 2022-09-18"
;; Patch `read-from-kill-ring' so that it doesn't collapse entries to single line
(defun read-from-kill-ring (prompt)
  "Read a `kill-ring' entry using completion and minibuffer history.
PROMPT is a string to prompt with."
  ;; `current-kill' updates `kill-ring' with a possible interprogram-paste
  (current-kill 0)
  (let* ((history-add-new-input nil)
         (history-pos (when yank-from-kill-ring-rotate
                        (- (length kill-ring)
                           (length kill-ring-yank-pointer))))
         ;; Remove keymaps from text properties of copied string,
         ;; because typing RET in the minibuffer might call
         ;; an irrelevant command from the map of copied string.
         (read-from-kill-ring-history
          (mapcar (lambda (s)
                    (remove-list-of-text-properties
                     0 (length s)
                     '(
                       keymap local-map action mouse-action
                       button category help-args)
                     s)
                    s)
                  kill-ring))
         (completions read-from-kill-ring-history))
    (minibuffer-with-setup-hook
        (lambda ()
          ;; Allow ‘SPC’ to be self-inserting
          (use-local-map
           (let ((map (make-sparse-keymap)))
             (set-keymap-parent map (current-local-map))
             (define-key map " " nil)
             (define-key map "?" nil)
             map)))
      (completing-read
       prompt
       (lambda (string pred action)
         (if (eq action 'metadata)
             ;; Keep sorted by recency
             '(metadata (display-sort-function . identity))
           (complete-with-action action completions string pred)))
       nil nil nil
       (if history-pos
           (cons 'read-from-kill-ring-history
                 (if (zerop history-pos) history-pos (1+ history-pos)))
         'read-from-kill-ring-history)))))
(byte-compile 'read-from-kill-ring)

;; Patch `vertico--truncate-multiline'
(defcustom k-vertico-multiline-max-lines 10
  "Maximum number of lines displayed for a multi-line candidate."
  :type 'number :group 'vertico)
(defun vertico--truncate-multiline (cand _max-width)
  "Truncate multiline CAND.
Ignore MAX-WIDTH, use `k-vertico-multiline-max-lines' instead."
  (let ((lines (string-lines cand)))
    (when (> (length lines) k-vertico-multiline-max-lines)
      (let ((tail (nthcdr (1- k-vertico-multiline-max-lines) lines)))
        (setcdr tail nil)
        (setcar tail (concat (car tail) (truncate-string-ellipsis))))
      (setq cand (mapconcat #'identity lines "\n"))))
  cand)
(byte-compile 'vertico--truncate-multiline)

;; Zebra strips, for better visualization of multiline candidates
;; Patch `vertico--display-candidates'
(defun vertico--display-candidates (lines)
  "Update candidates overlay `vertico--candidates-ov' with LINES."
  (move-overlay vertico--candidates-ov (point-max) (point-max))
  (cl-loop for line in lines
           for i from 0
           when (cl-evenp i)
           do (add-face-text-property 0 (length line) 'k-zebra 'append line))
  (overlay-put vertico--candidates-ov 'after-string
               (apply #'concat #(" " 0 1 (cursor t)) (and lines "\n") lines))
  (vertico--resize-window (* (ceiling (length lines) 2) 2)))
(byte-compile 'vertico--format-candidate)
(setq-default vertico-count 20)

;; Actual completion system
(require 'orderless)
(setq-default orderless-matching-styles '(orderless-literal orderless-flex orderless-regexp))
(setq-default completion-styles ;; '(orderless)
              '(flex orderless))
(setq-default completion-ignore-case t)
(setq-default read-buffer-completion-ignore-case t)
(setq-default read-file-name-completion-ignore-case t)
(setq-default enable-recursive-minibuffers t)
(define-key vertico-map (kbd "s-f") 'vertico-next-group)
(define-key vertico-map (kbd "s-b") 'vertico-previous-group)
(vertico-mode)
(marginalia-mode)

;;; Key bindings

(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))

;; Embark
(require 'consult)
(require 'embark)
(require 'embark-consult)
(setq embark-prompter #'embark-completing-read-prompter)
(setq embark-indicators
      (k-quote embark--vertico-indicator embark-minimal-indicator
               embark-highlight-indicator embark-isearch-highlight-indicator))
(global-set-key (kbd "C-M-h") 'backward-kill-sexp)
;; (define-key isearch-mode-map (kbd "s-s") 'helm-swoop-from-isearch)
(global-set-key (kbd "s-m") 'magit-status)
(setq-default consult-preview-key (kbd "C-h"))
(global-set-key (kbd "s-w") 'save-buffer)
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-h") 'consult-imenu)
(global-set-key (kbd "s-;") 'consult-goto-line)
(global-set-key (kbd "C-c C-SPC") 'consult-mark)
(global-set-key (kbd "C-c C-c C-SPC") 'consult-global-mark)
(global-set-key (kbd "s-s") 'consult-line)
(global-set-key (kbd "C-z") 'embark-act)

(cl-flet ((global-set-key (key command)
            (when (k-exwm-enabled-p)
              (define-key exwm-mode-map key command))
            (global-set-key key command)))
  (global-set-key (kbd "s-0") 'delete-window)
  (global-set-key (kbd "s-1") 'zygospore-toggle-delete-other-windows)
  (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
  (global-set-key (kbd "s-2") 'split-window-below)
  (global-set-key (kbd "s-3") 'split-window-right)
  (global-set-key (kbd "s-p") 'windmove-up)
  (global-set-key (kbd "s-n") 'windmove-down)
  (global-set-key (kbd "s-r") 'windmove-right)
  (global-set-key (kbd "s-l") 'windmove-left)
  (global-set-key (kbd "s-k") 'bury-buffer)
  (global-set-key (kbd "s-i") 'find-file)
  (global-set-key (kbd "s-q") 'consult-buffer))
;; (define-key helm-find-files-map (kbd "s-d") 'helm-ff-run-delete-file)
;; (define-key helm-find-files-map (kbd "s-w") 'helm-ff-run-copy-file)
;; (define-key helm-find-files-map (kbd "s-l") 'helm-ff-run-symlink-file)
;; (define-key helm-find-files-map (kbd "s-r") 'helm-ff-run-rename-file)
;; (define-key helm-buffer-map (kbd "s-d") 'helm-buffer-run-kill-buffers)
;; (define-key helm-buffer-map (kbd "M-D") nil)
(global-set-key (kbd "s-v") #'k--projectile-find-file)
;; (global-set-key (kbd "s-a") #'helm-projectile-switch-to-buffer)
;; (global-set-key (kbd "s-V") #'helm-projectile-switch-project)
(global-set-key (kbd "s-SPC") 'fixup-whitespace)

(global-set-key (kbd "s-g") 'eww-new-buffer)
(global-set-key (kbd "s-a") 'emms)

(when (k-exwm-enabled-p)
  (setq exwm-input-global-keys
        `((,(kbd "s-<escape>") . exwm-reset)))
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete]))))

(defun k-grep-in (filename)
  "Grep in FILENAME."
  (if (file-directory-p filename)
      (consult-grep filename)
    (let ((buffer (find-file-noselect filename)))
      (with-current-buffer buffer
        (consult-line)))))
(define-key embark-file-map (kbd "g") 'k-grep-in)
(define-key vertico-map (kbd "C-s")
            (lambda ()
              (interactive)
              (embark--act 'k-grep-in (car (embark--targets)) embark-quit-after-action)))

(define-key indent-rigidly-map (kbd "C-b") 'indent-rigidly-left)
(define-key indent-rigidly-map (kbd "C-f") 'indent-rigidly-right)
(define-key indent-rigidly-map (kbd "M-b") 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map (kbd "M-f") 'indent-rigidly-right-to-tab-stop)

(define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
(define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)
(define-key lsp-mode-map (kbd "s-d") 'lsp-execute-code-action)

(ace-link-setup-default "o")

(define-key emacs-lisp-mode-map (kbd "C-c C-p") #'eval-print-last-sexp)

;;; Lisp development

;; General mode setup
(require 'slime)
(require 'slime-repl)
(mapc (lambda (h)
        (add-hook h #'paredit-mode)
        (add-hook h (lambda () (setq outline-regexp "(section-start")))
        (add-hook h #'highlight-indent-guides-mode))
      '(emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook
        clojure-mode-hook))
(mapc (lambda (h)
        (add-hook h #'paredit-mode))
      '(slime-repl-mode-hook
        geiser-repl-mode-hook
        cider-repl-mode-hook))
(add-hook 'emacs-lisp-mode-hook #'rainbow-mode)
(add-hook 'scheme-mode-hook #'paredit-mode)
(add-hook 'slime-repl-mode-hook #'paredit-mode)
(add-hook 'slime-repl-mode-hook #'k-pad-header-line-after-advice)
(add-hook 'lisp-mode-hook #'slime-mode)
(add-hook 'lisp-mode-hook #'slime-editing-mode)
(add-hook 'lisp-mode-hook 'ensure-slime)
(mapc (lambda (h)
        (add-hook h (lambda () (setq-local lisp-indent-function 'common-lisp-indent-function))))
      '(lisp-mode-hook slime-repl-mode-hook))
(font-lock-add-keywords 'lisp-mode '(("(\\(setf\\)" 1 font-lock-keyword-face)
                                     ("(\\(setq\\)" 1 font-lock-keyword-face)
                                     ("(\\(psetf\\)" 1 font-lock-keyword-face)
                                     ("(\\(psetq\\)" 1 font-lock-keyword-face)))
(add-to-list 'lisp-imenu-generic-expression
             (list "Section" "^;;;\\([^#].*\\)$" 1) t)
(setq auto-mode-alist (cons '("\\.ss" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sls" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.scm" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (delq (assoc "\\.rkt\\'" auto-mode-alist) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lisp" . lisp-mode) auto-mode-alist))

(slime-setup '(slime-company slime-fancy slime-quicklisp slime-asdf slime-media slime-parse))
(require 'slime-company)
(setq-default slime-company-completion 'fuzzy)
(setq slime-lisp-implementations
      `((sbcl (,inferior-lisp-program "--dynamic-space-size" "4096"))
        (mega-sbcl (,inferior-lisp-program "--dynamic-space-size" "16384" "--control-stack-size" "2"))
        (ccl ("/opt/local/bin/ccl64"))))

;; Handy slime commands and key bindings
(defun ensure-slime ()
  (unless slime-default-connection
    (save-excursion (slime))))
(defun slime-repl-sync ()
  "Switch to Slime REPL and synchronize package/directory."
  (interactive)
  (slime-sync-package-and-default-directory)
  (slime-repl))
(define-key slime-mode-map (kbd "s-x") 'slime-repl-sync)
(define-key slime-repl-mode-map (kbd "M-r") nil)
(define-key slime-editing-map (kbd "<f2>")
            (define-keymap
              "v" #'slime-inspect
              "o" #'slime-describe-symbol
              "i" #'slime-documentation-lookup))
(defun slime-undefine ()
  "Undefine toplevel definition at point."
  (interactive)
  (cl-flet
      ((run-with (f p symbol)
         (message "%s %s => %s" f symbol
                  (slime-eval
                   `(cl:let ((symbol (cl:find-symbol ,(upcase (symbol-name symbol)))))
                            (cl:cond ((cl:null symbol) "No such symbol")
                                     ((cl:not (,p symbol)) (cl:unintern symbol) "Uninterned")
                                     (t (,f symbol))))))))
    (slime-dcase
        (slime-parse-toplevel-form)
      (((:defun :defgeneric :defmacro) name) (run-with 'cl:fmakunbound 'cl:fboundp name))
      (((:defvar :defparameter) name) (run-with 'cl:makunbound 'cl:boundp name))
      (((:defconstant) name) (run-with 'cl:unintern 'cl:identity name))
      (((:defstruct :defclass) name) (run-with 'cl:make-instances-obsolete 'cl:find-class name)))))
(define-key slime-mode-map (kbd "C-M-g") 'slime-undefine)

;; #+nil structural comment for Common Lisp
(require 'paredit)
(define-key paredit-mode-map (kbd "C-j") nil) ;; Don't clash with `eval-print-last-sexp'
(defmacro advance-save-excursion (&rest body)
  `(let ((marker (point-marker)))
     (set-marker-insertion-type marker t)
     (unwind-protect
         (progn ,@body)
       (goto-char marker))))
(defmacro structured-comment-maybe (thing string)
  `(advance-save-excursion
    (let ((floating (not (ignore-errors (beginning-of-thing ,thing)))))
      (when floating (skip-chars-forward "^("))
      (or (when (looking-at "#\\+nil")
            (let ((start (point)))
              (forward-char 5)
              (skip-chars-forward "\r\n[:blank:]")
              (delete-region start (point))
              t))
          (save-excursion
            (let ((end (point)))
              (skip-chars-backward "\r\n[:blank:]")
              (backward-char 5)
              (when (looking-at-p "#\\+nil")
                (delete-region (point) end)
                t)))
          (progn
            (when floating (goto-char marker))
            (insert ,string))))))
(defun structured-comment-advice (orig-fun &optional n)
  (if slime-mode
      (structured-comment-maybe 'sexp "#+nil ")
    (funcall orig-fun n)))
(advice-add 'comment-or-uncomment-sexp :around 'structured-comment-advice)
(define-key paredit-mode-map (kbd "M-;") #'comment-or-uncomment-sexp)
(defun structured-comment-defun ()
  "Use #+nil to comment a top-level form for Common Lisp."
  (interactive)
  (if slime-mode
      (structured-comment-maybe 'defun "#+nil
")
    (save-excursion
      (beginning-of-line)
      (if (eq (char-after) ?\;)
          (comment-or-uncomment-sexp)
        (beginning-of-defun)
        (comment-or-uncomment-sexp)))))
(define-key paredit-mode-map (kbd "C-M-;") #'structured-comment-defun)

;; *slime-scratch*
(defun switch-to-scratch ()
  "Switch to scratch buffer."
  (interactive)
  (if slime-editing-mode
      (slime-scratch)
    (switch-to-buffer-other-window "*scratch*")))
(global-set-key (kbd "s-o") 'switch-to-scratch)

;; Slime mode line
(defun slime-mode-line ()
  (concat (slime-connection-name) " "
          (propertize (downcase (string-trim (slime-current-package) "#?:\\|\"" "\""))
                      'face 'k-proper-name)))

;; Hacks to make slime-autodoc works better
(setq auto-save-no-message t) ;; Slime auto-saves like crazy for some reason...
(setq eldoc-idle-delay 0)

;; Paredit enhancements

(define-key paredit-mode-map (kbd "M-c") #'paredit-convolute-sexp)
(require 'paxedit)
(define-key paxedit-mode-map (kbd "M-q") nil)
(defun paxedit-copy-1 ()
  (interactive)
  (if mark-active (call-interactively #'kill-ring-save) (paxedit-copy)))
(define-key paxedit-mode-map (kbd "C-w") #'paxedit-kill)
(define-key paxedit-mode-map (kbd "M-w") #'paxedit-copy-1)
(define-key paxedit-mode-map (kbd "s-f") #'paxedit-transpose-forward)
(define-key paxedit-mode-map (kbd "s-b") #'paxedit-transpose-backward)
(defun k--paxedit-kill-advice (orig-fun &rest args)
  "Call KILL-REGION instead if mark is active.
Otherwise call ORIG-FUN with ARGS."
  (if mark-active
      (kill-region 0 0 'region)
    (apply orig-fun args)))
(advice-add 'paxedit-kill :around #'k--paxedit-kill-advice)
(add-hook 'paredit-mode-hook #'paxedit-mode)
(define-key paxedit-mode-map (kbd "M-j") #'paxedit-compress)
(define-key paxedit-mode-map (kbd "M-k") #'paxedit-format-1)

;; Enable Paredit and Company in Lisp related minibuffers
(defun sexp-minibuffer-hook ()
  (when (and (symbolp this-command)
             (or (eq this-command 'eval-expression)
                 (string-prefix-p "sldb" (symbol-name this-command))
                 (string-prefix-p "slime" (symbol-name this-command))))
    (paredit-mode)
    (company-mode)))
(add-hook 'minibuffer-setup-hook 'sexp-minibuffer-hook)

;; Slime debug window non-prolifiration
(add-to-list 'display-buffer-alist '("\\`*sldb" (display-buffer-reuse-mode-window)))

;;; Clojure
(require 'cider)

;;; Magit

(require 'magit)
(defun cloc-magit-root ()
  "Run Count Line Of Code for current Git repo."
  (interactive)
  (term (concat "cloc " (magit-toplevel))))

;;; window/buffer/frame/workspaces movement

(require 'zygospore)
(require 'windmove)

;; Moving between window/buffer/frame/workspaces in 4 directions
(defun next-workspace (direction)
  (cl-case direction
    (left (exwm-workspace-switch (1- exwm-workspace-current-index)))
    (right (exwm-workspace-switch (1+ exwm-workspace-current-index)))))
(if (not (k-exwm-enabled-p))
    (progn
      (require 'framemove)
      (setq framemove-hook-into-windmove t))
  (defun windmove-select-advice (orig-func dir &rest args)
    "If there is an error, try framemove in that direction."
    (condition-case nil
        (apply orig-func dir args)
      (error (next-workspace dir))))
  (advice-add 'windmove-do-window-select :around #'windmove-select-advice))
(require 'buffer-move)
;; Intuitively, this works like windmove but move buffer together with cursor.
(global-set-key (kbd "C-s-p") #'buf-move-up)
(global-set-key (kbd "C-s-n") #'buf-move-down)
(global-set-key (kbd "C-s-r") #'buf-move-right)
(global-set-key (kbd "C-s-l") #'buf-move-left)
;; Override buffer-move to support inter-frame/inter-exwm-workspace buffer movement.
(defun buf-move-to (direction)
  "Helper function to move the current buffer to the window in the given
   direction (with must be `up', `down', `left' or `right').  An error is
   thrown, if no window exists in this direction."
  (let* ((this-win (selected-window))
         (buf-this-buf (window-buffer this-win))
         (other-win
          (let ((buf-this-window (windmove-find-other-window direction)))
            (if (null buf-this-window)
                (progn
                  (if (k-exwm-enabled-p)
                      (next-workspace direction)
                    (fm-next-frame direction))
                  (selected-window))
              buf-this-window))))
    (if (null other-win)
        (error "No window in this direction")
      (if (window-dedicated-p other-win)
          (error "The window in this direction is dedicated"))
      (if (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win)))
          (error "The window in this direction is the Minibuf"))
      (if (eq buffer-move-behavior 'move)
          ;; switch selected window to previous buffer (moving)
          (switch-to-prev-buffer this-win)
        ;; switch selected window to buffer of other window (swapping)
        (set-window-buffer this-win (window-buffer other-win)))

      ;; switch other window to this buffer
      (set-window-buffer other-win buf-this-buf)

      (when (or (null buffer-move-stay-after-swap)
                (eq buffer-move-behavior 'move))
        (select-window other-win)))))

;;; Avy jump

(require 'avy)
(defconst hyper-mask (- ?\H-a ?a))
(defun hyper-ace ()
  (interactive)
  (avy-goto-word-1 (- last-command-event hyper-mask)))
(dolist (x (number-sequence ?a ?z))
  (global-set-key (vector (+ hyper-mask x)) #'hyper-ace))
(setq avy-keys (number-sequence ?a ?z))

;;; goto-last-change

(require 'goto-last-change)
(global-set-key (kbd "s-e") #'goto-last-change)

;;; EMMS

(require 'emms-setup)
(emms-all)
(emms-mode-line-mode 0)
(require 'emms-player-mpv)
(setq emms-player-list '(emms-player-mpv))
(setq emms-source-file-default-directory "~/Music/EMMS/")
(defun k-emms-mode-line ()
  (concat
   (propertize
    (cond ((or emms-player-stopped-p emms-player-mpv-stopped)
           (all-the-icons-material "stop"))
          (emms-player-paused-p
           (all-the-icons-material "pause"))
          (emms-player-playing-p
           (all-the-icons-material "play_arrow"))
          (t (all-the-icons-material "sync"))))
   (if emms-player-playing-p
       (concat " " (propertize (format-seconds "%.2h:%z%.2m:%.2s" emms-playing-time)
                               'face 'mode-line-highlight)
               "/" (propertize
                    (let ((total (emms-track-get
                                  (emms-playlist-current-selected-track)
                                  'info-playing-time)))
                      (if total (format-seconds "%.2h:%z%.2m:%.2s" total) "unknown"))
                    'face 'bold))
     "")))

(require 'emms-info-tinytag)
(defvar k-python3
  (cond ((executable-find "python") "python")
        ((executable-find "python3") "python3")
        (t (warn "Unable to guess python3 path."))))
(setq emms-info-tinytag-python-name k-python3)
(add-to-list 'emms-info-functions 'emms-info-tinytag)

(defun k-emms-toggle-video (&rest args)
  "TELL MPV player to switch to video/no-video mode."
  (interactive)
  (let* ((no-video-now (member "--no-video" emms-player-mpv-parameters))
         (no-video-wanted (if args (car args) (not no-video-now))))
    (if no-video-wanted
        (add-to-list 'emms-player-mpv-parameters "--no-video")
      (setq emms-player-mpv-parameters (delete "--no-video" emms-player-mpv-parameters)))
    (when (process-live-p emms-player-mpv-proc)
      (if no-video-wanted
          (emms-player-mpv-cmd '(set vid no))
        (emms-player-mpv-cmd '(set vid auto))))))

(defun emms-playing-time-display ()
  "Display playing time on the mode line."
  ;; (setq emms-playing-time (round (1+ emms-playing-time)))
  (emms-player-mpv-event-playing-time-sync)
  (force-mode-line-update))

(defun k-emms-player-mpv-event-function (json-data)
  (pcase (alist-get 'event json-data)
    ("video-reconfig"
     (emms-player-mpv-event-playing-time-sync))))
(add-to-list 'emms-player-mpv-event-functions 'k-emms-player-mpv-event-function)

;; Patch `emms-playlist-mode-overlay-selected' so that overlay extend to full line
;; Also set a `priority'
(defun emms-playlist-mode-overlay-selected ()
  "Place an overlay over the currently selected track."
  (when emms-playlist-selected-marker
    (save-excursion
      (goto-char emms-playlist-selected-marker)
      (let ((reg (emms-property-region (point) 'emms-track)))
        (if emms-playlist-mode-selected-overlay
            (move-overlay emms-playlist-mode-selected-overlay
                          (car reg)
                          (1+ (cdr reg)))
          (setq emms-playlist-mode-selected-overlay
                (make-overlay (car reg)
                              (1+ (cdr reg))
                              nil t nil))
          (overlay-put emms-playlist-mode-selected-overlay
                       'face 'emms-playlist-selected-face)
          (overlay-put emms-playlist-mode-selected-overlay
                       'evaporate t)
          (overlay-put emms-playlist-mode-selected-overlay
                       'priority 1))))))

(define-key emms-playlist-mode-map (kbd "p") #'emms-pause)
(define-key emms-playlist-mode-map (kbd "n") nil)
(define-key emms-playlist-mode-map (kbd "M-p") #'emms-previous)
(define-key emms-playlist-mode-map (kbd "M-n") #'emms-next)
(define-key emms-playlist-mode-map (kbd "C-M-p") #'emms-playlist-mode-previous)
(define-key emms-playlist-mode-map (kbd "C-M-n") #'emms-playlist-mode-next)

;; Eye candies
(add-hook 'emms-playlist-mode 'stripes-mode)
(add-hook 'emms-playlist-mode 'hl-line-mode)

;;; Mode line

(defun k-pad-mode-line-format (format)
  (unless (stringp format)
    (setq format (format-mode-line format)))
  `(#(" " 0 1 (face default display (space :width left-fringe)))
    ,(truncate-string-to-width
      format
      (window-text-width (get-buffer-window (current-buffer)))
      nil nil (truncate-string-ellipsis))
    #(" " 0 1 (display (space :align-to right)))
    #(" " 0 1 (face default display (space :width right-fringe)))))
(byte-compile 'k-pad-mode-line-format)

(defvar k-selected-window nil)
(defun k-set-selected-window ()
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq k-selected-window (frame-selected-window))))
(defun k-mode-line-selected-p ()
  (eq (selected-window) k-selected-window))
(add-hook 'window-state-change-hook 'k-set-selected-window)
(setq-default mode-line-misc-info
              '((slime-mode (:eval (slime-mode-line)))
                (:eval (if (eq major-mode 'emms-playlist-mode) (k-emms-mode-line) "")))
              mode-line-format
              '(:eval
                (k-pad-mode-line-format
                 '("" mode-line-mule-info mode-line-client mode-line-modified
                   mode-line-remote " "
                   (14 (:eval (if (k-mode-line-selected-p) #("%c" 0 2 (face mode-line-emphasis))
                                "%c"))
                       (#(" %l/" 0 3 (face mode-line-highlight))
                        (:propertize (:eval (number-to-string (line-number-at-pos (point-max))))
                                     face bold)))
                   "  " (:propertize "%b" face mode-line-buffer-id)
                   " \t"
                   (mode-line-process ("(" mode-name ":" mode-line-process  ")")
                                      mode-name)
                   "  " mode-line-misc-info))))

(defvar-local k-pad-last-header-line-format nil)
(defun k-pad-header-line-after-advice (&optional object &rest args)
  (cond ((framep object)
         (dolist (window (window-list object 'no-minibuf))
           (k-pad-header-line-after-advice (window-buffer window))))
        ((bufferp object) (with-current-buffer object (k-pad-header-line-after-advice)))
        (t (unless (equal header-line-format k-pad-last-header-line-format)
             (setq-local header-line-format `(:eval (k-pad-mode-line-format ',header-line-format)))
             (setq-local k-pad-last-header-line-format header-line-format)))))
(add-hook 'Info-mode-hook #'k-pad-header-line-after-advice)
;; (add-hook 'window-buffer-change-functions 'k-pad-header-line-after-advice)

;;; Cute and useless visuals!

(require 'highlight-tail)
(setq highlight-tail-timer 0.1)
(setq highlight-tail-steps 20)
(defvar blink-cursor-colors nil)
(defvar blink-highlight-colors nil)
(setq highlight-tail-colors `((,(car blink-highlight-colors) . 0)))
(highlight-tail-mode)
(setq blink-cursor-count 0)
(blink-cursor-mode)
(defun blink-cursor-timer-function ()
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (let ((color (nth blink-cursor-count blink-cursor-colors))
          (hl-color (nth blink-cursor-count blink-highlight-colors)))
      (set-cursor-color color)
      (setq highlight-tail-colors `((,hl-color . 0)))
      (setq highlight-tail-colors-fade-list nil
            highlight-tail-nonhtfaces-bgcolors nil
            highlight-tail-const-overlays-list nil
            highlight-tail-update-const-overlays-to-this-list nil
            highlight-tail-face-max nil)
      (let* ((background-color-name (face-background 'default))
             (background-color-hex (highlight-tail-hex-from-colorname
                                    background-color-name)))
        (setq highlight-tail-default-background-color background-color-name))
      (setq highlight-tail-colors-with-100
            (if (= (cdr (nth (1- (length highlight-tail-colors))
                             highlight-tail-colors))
                   100)
                highlight-tail-colors
              (append highlight-tail-colors (list '(null . 100)))))
      (setq highlight-tail-face-max highlight-tail-steps)
      (highlight-tail-add-colors-fade-table 'start)
      (highlight-tail-add-colors-fade-table 'default)
      (highlight-tail-make-faces
       (highlight-tail-get-colors-fade-table-with-key 'default))
      (setq blink-cursor-count (+ 1 blink-cursor-count))))
  (internal-show-cursor nil (not (internal-show-cursor-p))))

;; Patch `highlight-tail' to handle non-default background correctly
;; on GNU Emacs (it now works for inherited background)
(defun highlight-tail-get-bgcolor-hex (point)
  "Get the background color of point.

Do not take highlight-tail's overlays into consideration.  This means
that if there is ht's overlay at at the top then return 'default"
  (let ((point-face (get-char-property point 'face))
        point-face-from-cache
        point-face-bgcolor
        point-face-bgcolor-hex)
    (if point-face
        (progn
          (when (listp point-face)
            (setq point-face
                  (or (cl-find-if (lambda (x) (and x (symbolp x))) point-face)
                      'default)))
          (setq point-face-from-cache
                (assoc point-face highlight-tail-nonhtfaces-bgcolors))
          (if point-face-from-cache
              (setq point-face-bgcolor-hex (cdr point-face-from-cache))
            (setq point-face-bgcolor (face-background point-face nil t))
            (when (or (eq point-face-bgcolor nil)
                      (eq point-face-bgcolor 'unspecified))
              (setq point-face-bgcolor 'default))))
      (setq point-face-bgcolor 'default))
    (when (not point-face-bgcolor-hex)  ; not read from cache
      (if (eq point-face-bgcolor 'default)
          (setq point-face-bgcolor-hex 'default)
        ;; else
        (setq point-face-bgcolor-hex
              (highlight-tail-hex-from-colorname point-face-bgcolor))
        (setq highlight-tail-nonhtfaces-bgcolors
              (cons (cons point-face point-face-bgcolor-hex)
                    highlight-tail-nonhtfaces-bgcolors))
        (highlight-tail-add-colors-fade-table point-face-bgcolor-hex)
        (highlight-tail-make-faces
         (highlight-tail-get-colors-fade-table-with-key
          point-face-bgcolor-hex))))
    ;; return value
    point-face-bgcolor-hex))
(byte-compile 'highlight-tail-get-bgcolor-hex)

(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'bitmap)
(clrhash highlight-indent-guides--bitmap-memo)
(setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line)
(setq highlight-indent-guides-responsive 'top)
(setq highlight-indent-guides-auto-enabled nil)
(setq highlight-indent-guides-delay 0)

;;; Scheme

(require 'scheme)
(defun geiser-mode-maybe ()
  (unless (eq major-mode 'scheme-interaction-mode)
    (geiser-mode)))
(add-hook 'scheme-mode-hook 'geiser-mode-maybe)
(define-key scheme-mode-map (kbd "M-l") 'geiser-load-current-buffer)
(require 'racket-mode)
(add-hook 'racket-mode-hook 'geiser-mode)
(define-key racket-mode-map (kbd "M-l")
            (lambda ()
              (interactive)
              (geiser-load-file (buffer-file-name (current-buffer)))
              (switch-to-geiser-module (geiser-eval--get-module) (current-buffer))))
(define-key scheme-mode-map (kbd "C-h h") 'geiser-doc-look-up-manual)

(setq geiser-mode-start-repl-p t)

;;; Vterm

(require 'vterm)
(require 'multi-vterm)

;; Ad-hoc workaround: interaction with wide fringe/padding
(defun vterm--get-margin-width () 1)

(global-set-key (kbd "s-x") 'multi-vterm-next)
(global-set-key (kbd "s-X") 'multi-vterm)
(define-key vterm-mode-map (kbd "C-c C-t") nil)
(define-key vterm-mode-map (kbd "C-c C-j") 'vterm-copy-mode)
(define-key vterm-mode-map (kbd "C-c M-o") 'vterm-clear)
(define-key vterm-mode-map (kbd "C-d") (lambda () (interactive) (vterm-send-key "d" nil nil t)))
(define-key vterm-copy-mode-map (kbd "C-c C-k") (lambda () (interactive) (vterm-copy-mode -1)))
(defun multi-vterm-set-custom-keys ()
  ""
  (local-set-key (kbd "s-x") 'multi-vterm)
  (local-set-key (kbd "s-f") 'multi-vterm-next)
  (local-set-key (kbd "s-b") 'multi-vterm-prev))
(setq vterm-max-scrollback 1000000)
(advice-add 'multi-vterm :after  (lambda ()
                                   (interactive)
                                   (multi-vterm-set-custom-keys)))
(advice-add 'vterm-copy-mode :after  (lambda (&optional on)
                                       (interactive)
                                       (when (or (not on) (> on 0))
                                         (multi-vterm-set-custom-keys))))
(advice-add 'term-char-mode :after (lambda () (local-set-key (kbd "C-c C-j") 'term-line-mode)))

;;; Web browsing

(require 'eww)
(setq browse-url-browser-function 'eww-browse-url)
(add-hook 'eww-after-render-hook 'k-pad-header-line-after-advice)
(defvar k-eww-history (make-hash-table :test 'equal)
  "Global history for eww. A EQUAL hash that maps title strings to URL.")
(defun k-eww-after-render-hook ()
  "Update EWW buffer title and save `k-eww-history'."
  (let ((title (plist-get eww-data :title))
        (url (plist-get eww-data :url)))
    (rename-buffer (format "*eww: %s*" title) t)
    (unless (> (length title) 0) (setq title "<no title>"))
    (puthash (concat (truncate-string-to-width title 40 nil nil (truncate-string-ellipsis))
                     #(" " 0 1 (display (space :align-to center)))
                     (propertize url 'face 'completions-annotations))
             url k-eww-history)))
(add-hook 'eww-after-render-hook 'k-eww-after-render-hook)
(defun k-eww-read-url ()
  (let* ((cand
          (completing-read "Enter URL or keywords: " k-eww-history)))
    (or (gethash cand k-eww-history) cand)))
(defun eww-new-buffer (url)
  (interactive (list (k-eww-read-url)))
  (with-temp-buffer
    (if current-prefix-arg
        (let ((eww-search-prefix "https://scholar.google.com/scholar?q="))
          (eww url))
      (eww url))))
(define-key eww-mode-map (kbd "G") 'eww-new-buffer)

(when (k-exwm-enabled-p)
  (defun k-browse-url-chromium (url &rest args)
    (start-process "chromium" " *chromium*" "chromium"
                   (concat "--app=" url)))
  (setq-default browse-url-secondary-browser-function 'k-browse-url-chromium)
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (exwm-workspace-rename-buffer (concat "EXWM: " exwm-title))))
  (defun k-eww-reload-in-chromium ()
    (interactive)
    (k-browse-url-chromium (plist-get eww-data :url)))
  (define-key eww-mode-map (kbd "f") 'k-eww-reload-in-chromium))

(when (featurep 'xwidget-internal)
  (add-to-list 'load-path "~/.emacs.d/lisp/xwwp")
  (require 'xwwp-full)
  (define-key xwidget-webkit-mode-map (kbd "o") 'xwwp-ace-toggle)
  (define-key xwidget-webkit-mode-map (kbd "s-h") 'xwwp-section)
  (setq-default xwwp-ace-label-style
                `(("z-index" . "2147483647")
                  ("color" . ,k-dk-blue)
                  ("font-family" . "monospace")
                  ("background-color" . ,"rgba(255,255,255,0.5)")
                  ("font-size" . "1.5em")
                  ("padding" . "0.1em")
                  ("border-width" . "0.1em")
                  ("border-style" . "solid")))
  (define-key xwidget-webkit-mode-map (kbd "l") 'xwidget-webkit-back)
  (define-key xwidget-webkit-mode-map (kbd "r") 'xwidget-webkit-forward)
  (define-key xwidget-webkit-mode-map (kbd "g") 'xwidget-webkit-reload))

(require 'ytel)
(setq-default ytel-invidious-api-url "https://vid.puffyan.us"
              ytel-title-video-reserved-space 40
              ytel-author-name-reserved-space 20)
(defun ytel-play (&optional no-video)
  "Play video at point with EMMS."
  (interactive "P")
  (k-emms-toggle-video no-video)
  (ytel-add t)
  (with-current-emms-playlist
    (emms-playlist-previous)
    (emms-playlist-mode-play-current-track)))
(defun ytel-add (&optional here)
  "Add video at point to EMMS playlist."
  (interactive "P")
  (let* ((video (ytel-get-current-video))
         (id    (ytel-video-id video))
         (url (concat "https://www.youtube.com/watch?v=" id))
         (track (emms-track 'url url)))
    (emms-track-set track 'info-title (ytel-video-title video))
    (emms-track-set track 'info-playing-time (ytel-video-length video))
    (with-current-emms-playlist
      (if here
          (emms-playlist-insert-track track)
        (save-excursion
          (goto-char (point-max))
          (emms-playlist-insert-track track))))))
(define-key ytel-mode-map (kbd "RET") 'ytel-play)
(define-key ytel-mode-map (kbd "p") (kbd "C-u RET"))
(define-key ytel-mode-map (kbd "a") 'ytel-add)
(add-hook 'ytel-mode-hook 'stripes-mode)
(add-hook 'ytel-mode-hook
          '(lambda ()
             (setq-local hl-line-face '(:inherit match :extend t))
             (hl-line-mode)))

;;; PDF Tools

(require 'pdf-tools)
(pdf-tools-install)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;; EXWM
(when (eq window-system 'x)
  (defun k-screenshot ()
    (interactive)
    (let ((path (concat "~/Documents/Screenshot-" (format-time-string "%Y-%m-%d,%H:%M:%S") ".png")))
      (start-process-shell-command
       "import" nil (concat "import -window root " path))
      (message (concat "Screenshot saved to " path))))
  (global-set-key (kbd "<print>") 'k-screenshot)

  (defun k-set-volume (volume)
    "Change volume."
    (interactive (list (read-from-minibuffer
                        (format "Change volume (current %s): "
                                (let ((output (shell-command-to-string "amixer get Master")))
                                  (string-match "\\[\\([0-9]+%\\)\\]" output)
                                  (match-string 1 output)))
                        nil nil t)))
    (cl-check-type volume number)
    (unless (= 0 (call-process-shell-command (format "amixer set Master %s%%" volume)))
      (error "Failed to set volume"))))

;;; Undo Tree

(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-limit 1000000)
(setq undo-strong-limit 10000000)
(setq undo-outer-limit 100000000)
(setq undo-tree-enable-undo-in-region t)
(setq undo-tree-visualizer-timestamps t
      undo-tree-auto-save-history nil) ;; To fucking slow!
(define-key undo-tree-visualizer-mode-map (kbd "M-n") #'undo-tree-visualize-redo-to-x)
(define-key undo-tree-visualizer-mode-map (kbd "M-p") #'undo-tree-visualize-undo-to-x)

;;; Org

(require 'org)
(require 'tex-mode)
(modify-syntax-entry ?< "w" org-mode-syntax-table)
(modify-syntax-entry ?> "w" org-mode-syntax-table)
(modify-syntax-entry ?$ "\"" org-mode-syntax-table)
(modify-syntax-entry ?$ "\"" tex-mode-syntax-table)
(defun check-latex-fragment ()
  (let ((datum (org-element-context)))
    (when (memq (org-element-type datum) '(latex-environment latex-fragment))
      (org-latex-preview)
      t)))
(add-hook 'org-ctrl-c-ctrl-c-hook 'check-latex-fragment)
(require 'org-tempo)
(require 'ox-latex)
(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
(add-to-list 'org-latex-classes
             '("popl" "\\documentclass[acmsmall]{acmart}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("pldi" "\\documentclass[sigplan,10pt,review,anonymous]{acmart}\\settopmatter{printfolios=true,printccs=false,printacmref=false}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(setq org-latex-create-formula-image-program 'dvisvgm)
(setf (getf org-format-latex-options :scale) 2.0)
(setq-default org-html-with-latex 'dvisvgm)
(setq-default org-link-descriptive t
              org-hide-emphasis-markers t
              org-src-fontify-natively t)
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))
(org-babel-do-load-languages 'org-babel-load-languages '((lisp . t)))
(setq org-babel-lisp-eval-fn 'slime-eval)
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))
(defun k-org-mode-hook ()
  (visual-line-mode)
  (org-variable-pitch-minor-mode)
  ;; (org-appear-mode)
  (org-superstar-mode)
  ;; (org-indent-mode)
  )
(add-hook 'org-mode-hook #'k-org-mode-hook)

(setq-default org-superstar-item-bullet-alist
              '((?* . ?•)
                (?+ . ?➤)
                (?- . ?•))
              org-superstar-headline-bullets-list
              '(;; Original ones nicked from org-bullets
                ?◉
                ?○
                ?▷))

(setq polymode-prefix-key "\s-b")
(require 'poly-org)
(defun k-polymode-init-inner-hook ()
  (oset pm/chunkmode adjust-face 'org-block)
  (topsy-mode -1))
(add-hook 'polymode-init-inner-hook 'k-polymode-init-inner-hook)

(require 'engrave-faces-latex)
(setq-default engrave-faces-themes
              '((default
                 (default :short "default" :slug "D" :foreground "#000000" :background "#ffffff")
                 (shadow :short "shadow" :slug "h" :foreground "#7f7f7f")
                 (success :short "success" :slug "sc" :foreground "#228b22" :weight bold)
                 (warning :short "warning" :slug "w" :foreground "#ff8e00" :weight bold)
                 (error :short "error" :slug "e" :foreground "#ff0000" :weight bold)
                 (font-lock-comment-face :short "fl-comment" :slug "c" :foreground "#7f7f7f")
                 (font-lock-comment-delimiter-face :short "fl-comment-delim" :slug "cd" :foreground "#7f7f7f")
                 (font-lock-string-face :short "fl-string" :slug "s" :foreground "#8b2252")
                 (font-lock-doc-face :short "fl-doc" :slug "d" :foreground "#8b2252")
                 (font-lock-keyword-face :short "fl-keyword" :slug "k" :weight bold)
                 (font-lock-builtin-face :short "fl-builtin" :slug "b" :foreground "#8b2252")
                 (font-lock-function-name-face :short "fl-function" :slug "f" :foreground "#0000ff")
                 (font-lock-doc-markup-face :short "fl-doc-markup" :slug "m" :inherit font-lock-function-name-face)
                 (font-lock-variable-name-face :short "fl-variable" :slug "v" :inherit font-lock-function-name-face)
                 (font-lock-type-face :short "fl-type" :slug "t" :inherit font-lock-function-name-face)
                 (font-lock-constant-face :short "fl-constant" :slug "o" :foreground "#0000ff")
                 (font-lock-warning-face :short "fl-warning" :slug "wr" :foreground "#ff0000" :weight bold)
                 (font-lock-negation-char-face :short "fl-neg-char" :slug "nc")
                 (font-lock-preprocessor-face :short "fl-preprocessor" :slug "pp" :foreground "#483d8b")
                 (font-lock-regexp-grouping-construct :short "fl-regexp" :slug "rc" :weight bold)
                 (font-lock-regexp-grouping-backslash :short "fl-regexp-backslash" :slug "rb" :weight bold)
                 (org-block :short "org-block" :slug "ob")
                 (highlight-numbers-number :short "hl-number" :slug "hn" :foreground "#008b8b")
                 (highlight-quoted-quote :short "hl-qquote" :slug "hq" :foreground "#9370db")
                 (highlight-quoted-symbol :short "hl-qsymbol" :slug "hs" :foreground "#008b8b")
                 (rainbow-delimiters-depth-1-face :short "rd-1" :slug "rda" :foreground "#707183")
                 (rainbow-delimiters-depth-2-face :short "rd-2" :slug "rdb" :foreground "#7388d6")
                 (rainbow-delimiters-depth-3-face :short "rd-3" :slug "rdc" :foreground "#909183")
                 (rainbow-delimiters-depth-4-face :short "rd-4" :slug "rdd" :foreground "#709870")
                 (rainbow-delimiters-depth-5-face :short "rd-5" :slug "rde" :foreground "#907373")
                 (rainbow-delimiters-depth-6-face :short "rd-6" :slug "rdf" :foreground "#6276ba")
                 (rainbow-delimiters-depth-7-face :short "rd-7" :slug "rdg" :foreground "#858580")
                 (rainbow-delimiters-depth-8-face :short "rd-8" :slug "rdh" :foreground "#80a880")
                 (rainbow-delimiters-depth-9-face :short "rd-9" :slug "rdi" :foreground "#887070"))))
(setq-default org-latex-src-block-backend 'engraved)
(engrave-faces-use-theme 'default)

;;; BGEX patch: image background

;; (when (k-exwm-enabled-p)
;;   (require 'bgex)
;;   (bgex-set-image-default "~/Resources/wallpaper-blurred.jpg")
;;   (mapc (lambda (buf-name)
;;           (bgex-set-color buf-name 'bgex-identifier-type-buffer-name "#2e1e57"))
;;         '(" *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*"))
;;   (add-to-list 'default-frame-alist '(alpha . 70)))

;;; ERC

(advice-add 'erc-update-mode-line-buffer :after 'k-pad-header-line-after-advice)
(require 'erc)
(setq-default erc-track-enable-keybindings nil
              erc-prompt-for-password t)
(setq erc-server "localhost"
      erc-port 6670)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(advice-add #'erc-login :before
            (lambda ()
              (erc-server-send "CAP REQ :znc.in/self-message")
              (erc-server-send "CAP END")))

;;; GNUS

(require 'gnus)
(require 'gnus-topic)
(setq-default gnus-large-newsgroup nil
              gnus-use-cache t
              gnus-cache-enter-articles '(ticked dormant unread read)
              gnus-cache-remove-articles nil
              gnus-article-sort-functions '((not gnus-article-sort-by-date))
              gnus-thread-sort-functions '((not gnus-thread-sort-by-date))
              gnus-permanently-visible-groups "")
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq-default gnus-select-method '(nnnil nil)
              gnus-secondary-select-methods
              '((nnimap "Stanford"
                        (nnimap-address "localhost")
                        (nnimap-server-port 1143)
                        (nnimap-stream plain))))
(add-to-list 'gnus-parameters '("" (display . all)))


;; Correct IMAP Email count, adapted from
;; https://www.emacswiki.org/emacs/GnusNiftyTricks
(require 'imap)

(defun gnus-nnimap-count-format (n)
  (let ((method (or gnus-tmp-method gnus-select-method)))
    (when (eq (car method) 'nnimap)
      (let ((counts (nnimap-request-message-counts gnus-tmp-group method)))
        (if counts (format "%d" (nth n counts)) "?")))))

(defun gnus-user-format-function-t (dummy)
  (or (gnus-nnimap-count-format 0)
      gnus-tmp-number-total))

(defun gnus-user-format-function-y (dummy)
  (or (gnus-nnimap-count-format 1)
      gnus-tmp-number-of-unread))

(defvar nnimap-message-count-cache-alist nil)

(defun nnimap-message-count-cache-clear ()
  (setq nnimap-message-count-cache-alist nil))

(defun nnimap-message-count-cache-get (group)
  (cadr (assoc group nnimap-message-count-cache-alist)))

(defun nnimap-message-count-cache-set (group count)
  (push (list group count) nnimap-message-count-cache-alist))

(defun nnimap-request-message-counts (group method)
  (or (nnimap-message-count-cache-get group)
      (let ((counts (nnimap-fetch-message-counts group method)))
        (nnimap-message-count-cache-set group counts)
        counts)))

(defun nnimap-fetch-message-counts (group method)
  (let ((imap-group (car (last (split-string group ":"))))
        (server (cadr method)))
    (when (nnimap-change-group imap-group server)
      (message "Requesting message count for %s..." group)
      (with-current-buffer (nnimap-buffer)
        (let ((response
               (assoc "MESSAGES"
                      (assoc "STATUS"
                             (nnimap-command "STATUS %S (MESSAGES UNSEEN)"
                                             (utf7-encode imap-group t))))))
          (message "Requesting message count for %s...done" group)
          (and response
               (mapcar #'string-to-number
                       (list
                        (nth 1 response) (nth 3 response)))))))))

(add-hook 'gnus-after-getting-new-news-hook 'nnimap-message-count-cache-clear)

;; Eye candies

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)

;; FUCK THIS `gnus-spec' ABOMINATION!!!
;; I'm doing it properly myself
;; 把 `gnus-spec' 霍霍了！
(require 'gnus-spec)
(defun gnus-set-format (type &optional insertable)
  (gnus-update-format-specifications nil type))
(defun gnus-update-format-specifications (&optional force &rest types)
  "DON'T update format specifications.
Just grab them from `gnus-format-specs'."
  (let (new-format entry type val updated)
    (while (setq type (pop types))
      ;; Jump to the proper buffer to find out the value of the
      ;; variable, if possible.  (It may be buffer-local.)
      (save-current-buffer
        (let ((buffer (intern (format "gnus-%s-buffer" type))))
          (when (and (boundp buffer)
                     (setq val (symbol-value buffer))
                     (gnus-buffer-live-p val))
            (set-buffer val)))
        (setq new-format (symbol-value
                          (intern (format "gnus-%s-line-format" type))))
        (setq entry (cdr (assq type gnus-format-specs)))
        (set (intern (format "gnus-%s-line-format-spec" type))
             (cadr entry))))
    updated))
(setq-default gnus-use-full-window nil
              gnus-group-line-format nil
              gnus-summary-line-format nil
              gnus-article-mode-line-format nil
              gnus-summary-mode-line-format nil
              gnus-summary-dummy-line-format nil
              gnus-topic-line-format nil
              gnus-group-mode-line-format nil
              gnus-group-line-format nil)
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Hodie %H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Heri %H:%M")
        (604800 . "%a %H:%M")
        ((gnus-seconds-year) . "%b %d")
        (t . "%Y %b %d"))
      gnus-format-specs
      '((article-mode nil "")
        (summary-dummy nil
                       (progn
                         (insert "   ")
                         (put-text-property
                          (point)
                          (progn
                            (insert ":                             :")
                            (point))
                          'mouse-face gnus-mouse-face)
                         (insert " " gnus-tmp-subject "\n")))
        (summary-mode nil "")
        (summary nil
                 (add-face-text-property (point)
                                         (let (gnus-position)
                                           (insert (propertize (string gnus-tmp-unread gnus-tmp-replied gnus-tmp-score-char)
                                                               'face '(nil default) 'gnus-face t)
                                                   (propertize (truncate-string-to-width
                                                                (concat gnus-tmp-indentation
                                                                        (gnus-summary-from-or-to-or-newsgroups gnus-tmp-header gnus-tmp-from))
                                                                28 0 ?  (truncate-string-ellipsis))
                                                               'face '((:inherit (shadow k-proper-name)) default) 'gnus-face t)
                                                   #("  " 0 2 (face (nil default) gnus-face t)))
                                           (setq gnus-position (point))
                                           (insert (propertize (truncate-string-to-width
                                                                gnus-tmp-subject-or-nil
                                                                70 0 nil (truncate-string-ellipsis))
                                                               'face '((:inherit (variable-pitch bold)) default) 'gnus-face t)
                                                   #(" " 0 1 (face (nil default) gnus-face t display (space :align-to (- text 14))))
                                                   (propertize (format "%12s \n" (gnus-user-date (mail-header-date gnus-tmp-header)))
                                                               'face `((:inherit (shadow k-quote)) default) 'gnus-face t))
                                           (put-text-property gnus-position (1+ gnus-position) 'gnus-position t)
                                           (point))
                                         (if (cl-evenp (line-number-at-pos (point))) 'stripes nil) t))
        (topic nil
               (progn
                 (insert indentation "[ ")
                 (put-text-property
                  (point)
                  (progn
                    (add-text-properties
                     (point)
                     (progn (insert name) (point))
                     (cons 'face (cons (list 'bold 'default) '(gnus-face t))))
                    (point))
                  'mouse-face gnus-mouse-face)
                 (insert (format " -- %d ]%s\n" total-number-of-articles visible))))
        (group-mode nil "")
        (group nil
               (progn
                 (insert (format "%c%c%c%s%5s/%-5s %c"
                                 gnus-tmp-marked-mark
                                 gnus-tmp-subscribed
                                 gnus-tmp-process-marked
                                 gnus-group-indentation
                                 (gnus-user-format-function-y gnus-tmp-header)
                                 (gnus-user-format-function-t gnus-tmp-header)
                                 gnus-tmp-summary-live))
                 (put-text-property (point) (progn (insert gnus-tmp-qualified-group) (point)) 'mouse-face gnus-mouse-face)
                 (insert "\n")))
        (version . "28.2") (gnus-version . 5.13)))

;; Key bindings for trauma repression
(defun k-gnus-toggle-show-thread ()
  (interactive)
  (or (gnus-summary-show-thread)
      (gnus-summary-hide-thread)))
(defun k-gnus-toggle-show-all-threads ()
  (interactive)
  (if (cl-find-if (lambda (ov) (eq (overlay-get ov 'invisible) 'gnus-sum))
                  (overlays-in (point-min) (point-max)))
      (gnus-summary-show-all-threads)
    (gnus-summary-hide-all-threads)))
(defun k-gnus-toggle-show-topic ()
  (interactive)
  (unless (gnus-topic-fold)
    (error "Not at a topic")))
(defun k-gnus-topic-up-topic ()
  (interactive)
  (if (gnus-group-topic-name)
      (gnus-topic-goto-topic (gnus-topic-parent-topic (gnus-current-topic)))
    (gnus-topic-goto-topic (gnus-current-topic))))

(define-key gnus-topic-mode-map (kbd "<tab>") 'k-gnus-toggle-show-topic)
(define-key gnus-topic-mode-map (kbd "C-M-u") 'k-gnus-topic-up-topic)
(define-key gnus-summary-mode-map (kbd "<tab>") 'k-gnus-toggle-show-thread)
(define-key gnus-summary-mode-map (kbd "C-<tab>") 'k-gnus-toggle-show-all-threads)
(define-key gnus-article-mode-map (kbd "q") 'delete-window)
(define-key gnus-summary-mode-map (kbd "q") '(lambda () (interactive) (switch-to-buffer gnus-group-buffer)))
(define-key gnus-summary-mode-map (kbd "Q") 'gnus-summary-exit)
(define-key gnus-article-mode-map (kbd "o") 'ace-link)
(define-key gnus-summary-mode-map (kbd "t") 'gnus-summary-toggle-threads)
(define-key gnus-summary-mode-map (kbd "M-n") 'gnus-summary-next-unread-article)
(define-key gnus-summary-mode-map (kbd "M-p") 'gnus-summary-prev-unread-article)
(define-key gnus-summary-mode-map (kbd "n") 'gnus-summary-next-article)
(define-key gnus-summary-mode-map (kbd "p") 'gnus-summary-prev-article)
(define-key gnus-summary-mode-map (kbd "g") 'gnus-summary-insert-new-articles)

;;; Input Method

(require 'pyim)
(require 'pyim-basedict)
(pyim-basedict-enable)
(pyim-cangjie6dict-enable)
(setq-default pyim-punctuation-translate-p '(auto no yes))
(set-input-method 'pyim)
(deactivate-input-method)

;;; Misc handy commands

(defvar lookup-word-buffer nil)
(defun lookup-word (word)
  (interactive (list (thing-at-point 'word t)))
  (let ((display-buffer-alist '((".*" display-buffer-below-selected))))
    (select-window (display-buffer
                    (or (and (buffer-live-p lookup-word-buffer) lookup-word-buffer)
                        (current-buffer)))))
  (setq lookup-word-buffer (browse-url (format "https://en.wiktionary.org/wiki/%s#Latin" word))))

(defun demolish-package (symbol)
  "Nuke everything under namespace SYMBOL."
  (ignore-errors (unload-feature symbol t))
  (let* ((prefix (concat (symbol-name symbol) "-"))
         (accused-p (lambda (x) (or (eq x symbol) (s-prefix-p prefix (symbol-name x))))))
    (dolist (symbol features)
      (when (funcall accused-p symbol)
        (unload-feature symbol t)))
    (mapatoms
     (lambda (symbol)
       (when (funcall accused-p symbol)
         (makunbound symbol)
         (fmakunbound symbol)
         (unintern symbol obarray))))))

;; Could use org-agenda, but I like this simple UI,
;; and I run myself in pull-mode rather than push-mode
;; (I hate plans and notifications!)
(defcustom candy-list nil "List of candies." :type '(list string) :group 'applications)
(defcustom candy-regimen-path nil "Path to save regimen log." :type 'file :group 'applications)
(defun get-candy-time (candy)
  (with-current-buffer (find-file-noselect candy-regimen-path)
    (condition-case
        nil
        (save-excursion
          (search-backward candy)
          (- (org-time-stamp-to-now (buffer-substring (line-beginning-position) (point)) t)))
      (search-failed nil))))
(defun candy-time! ()
  "Choose your poison!"
  (interactive)
  (let ((buffer (find-file-noselect candy-regimen-path)))
    (with-current-buffer buffer
      ;; Show regimen file
      (select-window (display-buffer (current-buffer)))
      (end-of-buffer)
      (unless (bolp) (insert "\n"))
      (let* ((completion-extra-properties
              (list :annotation-function
                    ;; Display elapsed time since last intake
                    (lambda (candy)
                      (let ((time (get-candy-time candy)))
                        (when time
                          (concat "(" (format-seconds "%dd %hh %z%mm" time) ")"))))))
             (candy (completing-read "Choose your posion: " candy-list)))
        (org-insert-time-stamp nil t)
        (insert " " (substring-no-properties candy) "\n")
        (save-buffer)))))

;; Vampire timezone
;; How much sun-protection-free time left?
(require 'solar)
(setq-default calendar-longitude -122.1697
              calendar-latitude 37.4275)
(defun time-to-vampire-time (&optional time)
  (let* ((today-sun (solar-sunrise-sunset (calendar-current-date)))
         (today-sunrise (* 3600 (caar today-sun)))
         (today-sunset (* 3600 (caadr today-sun)))
         (tomorrow-sunrise
          (* 3600 (caar (solar-sunrise-sunset (calendar-current-date 1)))))
         (time (pcase (decode-time time)
                 (`(,s ,m ,h . ,_) (+ s (* 60 (+ m (* 60 h))))))))
    (cond ((<= time today-sunrise) (list 'sunrise (- today-sunrise time)))
          ((<= time today-sunset) (list 'sunset (- today-sunset time)))
          (t (list 'sunrise (+ tomorrow-sunrise (- (* 24 3600) time)))))))
(defun vampire-time-update ()
  (let* ((time (time-to-vampire-time))
         (msg (format (concat "%s till %s"
                              (propertize " | " 'face `(:foreground ,k-bg-blue))
                              "%s")
                      (format-seconds "%h:%.2m:%.2s" (cadr time))
                      (car time)
                      (ignore-error (format-seconds "%h:%.2m:%.2s" (get-candy-time "Estradiol")))))
         (width (string-width msg))
         (msg (concat (propertize " " 'display
                                  `(space :align-to (- right-fringe ,width)))
                      msg)))
    (with-current-buffer " *Echo Area 0*"
      (remove-overlays (point-min) (point-max))
      (overlay-put (make-overlay (point-min) (point-max) nil nil t)
                   'after-string msg))
    (with-current-buffer " *Minibuf-0*"
      (delete-region (point-min) (point-max))
      (insert msg))
    (when-let (buffer (get-buffer " *Vampire Time Screensaver*"))
      (with-current-buffer buffer
        (delete-region (point-min) (point-max))
        (let ((l1 (propertize (concat " " (format-seconds "%h:%.2m:%.2s" (cadr time)))
                              'face '(:height 10.0 :weight normal)))
              (l2 (propertize (format "till %s" (car time))
                              'face '(:height 4.0 :weight normal))))
          (insert l1 (propertize " \n" 'face '(:height 10.0 :weight normal)))
          (insert (propertize " "
                              'display `(space :width (,(- (shr-string-pixel-width l1)
                                                           (shr-string-pixel-width l2)))))
                  l2)))
      (posframe-show buffer :poshandler 'posframe-poshandler-frame-center
                     :internal-border-width 3))))
(add-hook 'post-command-hook 'vampire-time-update)
(defvar vampire-time-timer (run-at-time t 1 'vampire-time-update))
(defun vampire-time-screensaver ()
  (if insecure-lock-mode
      (progn
        (get-buffer-create " *Vampire Time Screensaver*")
        (vampire-time-update))
    (posframe-delete " *Vampire Time Screensaver*")))

;;; insecure-lock

(add-to-list 'load-path "~/.emacs.d/lisp/insecure-lock")
(require 'insecure-lock)
(setq insecure-lock-mode-hook '(vampire-time-screensaver insecure-lock-blank-screen))

;;; Observe!
(defun twitter-locked-p (username)
  "Returns (tweets followers following locked-p)"
  (cl-flet ((get-number ()
              (re-search-forward "[,0-9]+")
              (string-to-number (cl-remove-if (lambda (c) (= c ?\,)) (match-string 0)))))
    (with-temp-buffer
      (shell-command
       (format "curl -s -k https://nitter.net/%s" username)
       (current-buffer))
      (goto-char (point-min))
      (list
       (progn
         (search-forward "Tweets")
         (get-number))
       (progn
         (search-forward "Following")
         (get-number))
       (progn
         (search-forward "Followers")
         (get-number))
       (progn (goto-char (point-min))
              (if (ignore-errors (search-forward "tweets are protected")) t nil))))))
(defvar twitter-observe-id-list nil)
(defvar twitter-observe-path nil)
(defun twitter-observe ()
  (when twitter-observe-id-list
    (with-current-buffer (find-file-noselect twitter-observe-path)
      (end-of-buffer)
      (unless (bolp) (insert "\n"))
      (org-insert-time-stamp nil t)
      (dolist (id twitter-observe-id-list)
        (insert " " id " " (format "%s" (twitter-locked-p id))))
      (insert "\n")
      (let ((save-silently t))
        (save-buffer)))))
(defvar twitter-observe-timer
  (when twitter-observe-id-list
    (run-at-time 0 300
                 (lambda () (make-thread 'twitter-observe "Twitter Observer")))))

;;; Ladder
(defun start-clash-proxy ()
  "Start clash proxy."
  (interactive)
  (when (get-process "Clash Proxy")
    (user-error "Clash proxy already started"))
  (make-process :name "Clash Proxy" :buffer "*clash proxy*"
                :command (list "clash" "-f" (expand-file-name "~/clash.yaml")))
  (let ((http-proxy "127.0.0.1:7890"))
    (setenv "http_proxy" http-proxy)
    (setenv "https_proxy" http-proxy)
    (setq url-proxy-services `(("http" . ,http-proxy)
                               ("https" . ,http-proxy)))))
(defun stop-clash-proxy ()
  "Start clash proxy."
  (interactive)
  (interrupt-process "Clash Proxy")
  (setenv "http_proxy")
  (setenv "https_proxy")
  (setq url-proxy-services nil))

(setq enwc-default-backend 'nm)

(provide 'init)
;;; init.el ends here
