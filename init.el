;;; -*- lexical-binding: t -*-
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
(add-to-list 'load-path "~/.emacs.d/lilypond")

(require 'cl)

(tool-bar-mode -1)
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(scroll-bar-mode -1)
(setq gc-cons-threshold 8000000 gc-cons-percentage 0.25)
(setq-default garbage-collection-messages t)
(setq-default inhibit-startup-message t)
(setq async-bytecomp-allowed-packages '(all))

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

(setq custom-file "~/.emacs.d/custom/custom.el")
(load custom-file)

;; Bootstrap required packages
(defvar k-packages
  '(embark org-contrib exwm company-posframe orderless
  pyim-cangjiedict pyim projectile stripes vertico consult
  embark-consult haskell-mode mini-frame selectrum-prescient
  marginalia selectrum slime-company slime crdt impatient-mode
  comment-dwim-2 sudo-edit csv-mode zygospore yasnippet ws-butler
  volatile-highlights vlf use-package undo-tree telega
  system-packages smtpmail-multi slack showtip sauron
  rainbow-mode racket-mode pyim-basedict proof-general posframe
  pdf-tools paxedit ox-reveal org-static-blog org-present
  nasm-mode multi-vterm mmm-mode magit languagetool langtool
  iedit highlight-parentheses highlight-indent-guides
  goto-last-change gnu-apl-mode geiser flycheck emms-soundcloud
  elnode dtrt-indent ctable comment-or-uncomment-sexp
  clean-aindent-mode cdlatex bug-hunter buffer-move
  auto-highlight-symbol auctex anzu aggressive-indent
  adjust-parens ace-link 2048-game ytel))
(let ((to-install (remove-if #'package-installed-p k-packages)))
  (when to-install
    (message "%s packages to be installed." (length to-install))
    (dolist (package to-install)
      (ignore-errors (package-install package)))))

;;; company

(require 'company)
(setq-default company-backends '(company-capf company-files))
(setq-default company-dabbrev-downcase nil)
(setq-default company-tooltip-align-annotations t
              company-format-margin-function nil)

;; Zebra strips, to look consistent with vertico
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
    (if (evenp k--company-current-index)
        (add-face-text-property 0 width 'company-tooltip t line)
        (add-face-text-property 0 width `(:background ,k-bg-1) t line))
    line))


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

(projectile-global-mode)
(setq projectile-enable-caching t)
(defun k--projectile-find-file ()
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
  (let ((%global-mode-symbol (intern (concat "global-" (symbol-name mode)))))
    `(progn
       (define-globalized-minor-mode ,%global-mode-symbol ,mode
         (lambda () (,mode)))
       (,%global-mode-symbol))))

(defun k-exwm-enabled-p ()
  (member #'exwm--server-stop kill-emacs-hook))

;;; Theme

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(aset ansi-color-map 31 '((:inherit match)))
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

(require 'volatile-highlights)
(volatile-highlights-mode 0)

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

(require 'vlf-setup)
(setq vlf-application 'dont-ask)
(ace-link-setup-default "t")

(require 'highlight-parentheses)
(setq hl-paren-colors '(nil))
(set-face-attribute 'hl-paren-face nil :inherit 'show-paren-match)
(show-paren-mode)
(globalize highlight-parentheses-mode)

(require 'which-func)
(setq mode-line-misc-info
      (assq-delete-all 'which-function-mode mode-line-misc-info))
(which-func-mode)
(setq-default which-func-format
              '(:eval (let ((x (gethash (selected-window) which-func-table)))
                        (if x (concat "/" (propertize x 'face 'k-proper-name))
                            ""))))

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
(setq lisp-indent-function 'common-lisp-indent-function)

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

;; add duplicate line function from Prelude
;; taken from prelude-core.el
(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

;; smart openline
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
      (progn
        (move-end-of-line nil)
        (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-o") 'prelude-smart-open-line)

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
   '("mathpar" LaTeX-env-label)))

(require 'texmathp)
(add-to-list 'texmathp-tex-commands '("mathpar" env-on))
(texmathp-compile)

(require 'cdlatex)
(setq cdlatex-math-symbol-alist '((42 ("\\times" "\\product")) (43 ("\\cup" "\\sum"))))
(turn-on-cdlatex)

;;; Completion

;; popup frame layout
(require 'mini-frame)
(require 'vertico)
(setq-default mini-frame-resize 'not-set
              mini-frame-standalone t)
(defun k--resize-mini-frame (frame)
  (let* ((parent mini-frame-selected-frame)
         (window (frame-selected-window parent))
         (this-window (frame-root-window frame))
         (edges (frame-edges parent))
         (inner-edges (frame-edges parent 'inner-edges))
         (pos (window-absolute-pixel-position (window-point window) window)))
    (fit-frame-to-buffer-1 frame nil nil (- (frame-width parent) 4) 80)
    (modify-frame-parameters
     frame
     `((left . ,(min
                 (car pos)
                 (- (caddr inner-edges)
                    (frame-outer-width frame))))
       (top . ,(if (< (frame-outer-height frame) (- (cadddr edges) (cdr pos)))
                   (+ (cdr pos)  (line-pixel-height))
                   (- (cadddr edges) (frame-outer-height))))))))
(defvar k--minibuffer-display-table (make-display-table))
(set-display-table-slot k--minibuffer-display-table 0 (aref (cdr vertico-multiline) 0))
(defun k--minibuffer-setup-hook ()
  (setq-local truncate-lines t)
  (setq-local fringe-indicator-alist '((truncation)))
  (setq-local buffer-display-table k--minibuffer-display-table))
(add-hook 'minibuffer-setup-hook 'k--minibuffer-setup-hook)
(setq-default resize-mini-frames 'k--resize-mini-frame)
(mini-frame-mode)
(setq-default mini-frame-show-parameters
              (lambda ()
                `((no-accept-focus . t) ;; workaround pre-emacs-29: vertico shows no candidates (0/!) on empty input
                  (left-fringe . 8)
                  (right-fringe . 8)
                  (background-color . ,(face-background 'default))
                  (internal-border-width . 1)
                  (child-frame-border-width . 1)
                  (z-group . above))))

(require 'marginalia)
(setq-default marginalia-field-width 80)
;; (pkg-info-version-info 'marginalia)
;; "20220914.945"

(defun marginalia--affixate (metadata annotator cands)
  "Affixate CANDS given METADATA and Marginalia ANNOTATOR."
  ;; OUR CHANGE: Removed adjustment of marginalia-field-width according to (window-width)
  ;; otherwise it sometimes causes exponential shrinkage of mini-frame width on input
  (let* ((marginalia--metadata metadata)
         (cache marginalia--cache))
    (marginalia--align
     (with-selected-window (or (minibuffer-selected-window) (selected-window))
       (cl-loop for cand in cands collect
                (let ((ann (or (marginalia--cached cache annotator cand) "")))
                  (cons cand (if (string-blank-p ann) "" ann))))))))

;; Multiline candidates
;; Don't collapse multiline into single line.
;; I find this reads much better for, say, `yank-pop'

;; (emacs-version)
;; "GNU Emacs 28.2 (build 1, x86_64-apple-darwin21.5.0, NS appkit-2113.50 Version 12.4 (Build 21F79))
;;  of 2022-09-18"
(defun read-from-kill-ring (prompt)
  "Read a `kill-ring' entry using completion and minibuffer history.
PROMPT is a string to prompt with."
  ;; `current-kill' updates `kill-ring' with a possible interprogram-paste
  (current-kill 0)
  (let* ((history-add-new-input nil)
         (history-pos (when yank-from-kill-ring-rotate
                        (- (length kill-ring)
                           (length kill-ring-yank-pointer))))
         (ellipsis (if (char-displayable-p ?…) "…" "..."))
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

(defcustom k-vertico-multiline-max-lines 10
  "Maximum number of lines displayed for a multi-line candidate."
  :group 'vertico)
;; (pkg-info-version-info 'vertico)
;; "0.27"
(defun vertico--truncate-multiline (cand max-width)
  "Truncate multiline CAND.
Ignore MAX-WIDTH, use `k-vertico-multiline-max-lines' instead."
  (let ((lines (string-lines cand)))
    (when (> (length lines) k-vertico-multiline-max-lines)
      (let ((tail (nthcdr (1- k-vertico-multiline-max-lines) lines)))
        (setcdr tail nil)
        (setcar tail (concat (car tail) (truncate-string-ellipsis))))
      (setq cand (mapconcat #'identity lines "\n"))))
  cand)
;; Zebra strips, for better visualization of multiline candidates
(defun vertico--format-candidate (cand prefix suffix index _start)
  "Format CAND given PREFIX, SUFFIX and INDEX."
  (setq cand (vertico--display-string (concat prefix cand suffix "\n")))
  (cond ((= index vertico--index)
         (add-face-text-property 0 (length cand) 'vertico-current 'append cand))
        ((evenp index)
         (add-face-text-property 0 (length cand) `(:background ,k-bg-1 :extend t) 'append cand)))
  cand)
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

(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "DEL") (kbd "C-h"))
(define-key key-translation-map (kbd "M-h") (kbd "M-DEL"))

(require 'consult)
(require 'embark)
(require 'embark-consult)
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
(defun k-root-frame (frame)
  "Find non-child frame containing FRAME."
  (if (frame-parent frame)
      (k-root-frame (frame-parent frame))
      frame))
(defun k-display-buffer-posframe (buffer alist)
  (with-selected-frame (k-root-frame (selected-frame))
      (posframe-show buffer :internal-border-color k-bg-1 :internal-border-width 1
                         :left-fringe 8 :right-fringe 8)))
(add-to-list 'display-buffer-alist '("*Embark Actions*" (k-display-buffer-posframe)))

(cl-flet ((global-set-key (a b)
                          (when (k-exwm-enabled-p)
                            (exwm-input-set-key a b))
                          (global-set-key a b)))
         (global-set-key (kbd "s-0") 'delete-window)
         (global-set-key (kbd "s-1") 'zygospore-toggle-delete-other-windows)
         (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
         (global-set-key (kbd "s-2") 'split-window-below)
         (global-set-key (kbd "s-3") 'split-window-right)
         (global-set-key (kbd "s-p") 'windmove-up)
         (global-set-key (kbd "s-n") 'windmove-down)
         (global-set-key (kbd "s-r") 'windmove-right)
         (global-set-key (kbd "s-l") 'windmove-left)
         (global-set-key (kbd "s-k") 'switch-to-prev-buffer)
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
  (interactive (list (cdr (embark--vertico-selected))))
  (if (file-directory-p filename)
      (consult-grep filename)
      (let ((buffer (find-file-noselect filename)))
        (with-current-buffer buffer
          (consult-line)))))
(define-key vertico-map (kbd "C-s") 'k-grep-in)

(define-key indent-rigidly-map (kbd "C-b") 'indent-rigidly-left)
(define-key indent-rigidly-map (kbd "C-f") 'indent-rigidly-right)
(define-key indent-rigidly-map (kbd "M-b") 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map (kbd "M-f") 'indent-rigidly-right-to-tab-stop)

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
        scheme-mode-hook))
(mapc (lambda (h)
        (add-hook h #'paredit-mode))
      '(slime-repl-mode-hook
        geiser-repl-mode-hook))
(add-hook 'emacs-lisp-mode-hook #'rainbow-mode)
(add-hook 'scheme-mode-hook #'paredit-mode)
(add-hook 'slime-repl-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'slime-editing-mode)
(add-hook 'lisp-mode-hook 'ensure-slime)
(remove-hook 'lisp-mode-hook 'slime)
(font-lock-add-keywords 'lisp-mode '(("(\\(setf\\)" 1 font-lock-keyword-face)
                                     ("(\\(setq\\)" 1 font-lock-keyword-face)
                                     ("(\\(psetf\\)" 1 font-lock-keyword-face)
                                     ("(\\(psetq\\)" 1 font-lock-keyword-face)))
(add-to-list 'lisp-imenu-generic-expression
             (list "Section" "^;;;\\([^#].*\\)$" 1) t)

(slime-setup '(slime-company slime-fancy slime-quicklisp slime-asdf
               slime-banner slime-media slime-parse))
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
  (interactive)
  (slime-sync-package-and-default-directory)
  (slime-repl))
(define-key slime-mode-map (kbd "s-x") 'slime-repl-sync)
(define-key slime-repl-mode-map (kbd "M-r") nil)
(dolist (map (list slime-editing-map slime-repl-mode-map))
  (define-key map (kbd "C-c I") nil)
  (define-key map (kbd "C-h i") 'slime-inspect)
  (define-key map (kbd "C-h v") 'slime-describe-symbol)
  (define-key map (kbd "C-h f") 'slime-describe-function))
(define-key slime-mode-map (kbd "C-h h") 'slime-hyperspec-lookup)
(defun slime-undefine ()
  (interactive)
  (cl-macrolet
   ((run-with (symbol)
              `(slime-eval `(cl:let ((symbol (cl:find-symbol ,(upcase (symbol-name name)))))
                              (cl:when symbol (,,symbol symbol))))))
   (slime-dcase
    (slime-parse-toplevel-form)
    (((:defun :defgeneric :defmacro) name) (run-with 'cl:fmakunbound))
    (((:defvar :defparameter) name) (run-with 'cl:makunbound))
    (((:defconstant) name) (run-with 'cl:unintern))
    (((:defstruct :defclass) name) (run-with 'cl:make-instance-obsolete)))))

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
  (interactive)
  (if slime-editing-mode
      (slime-scratch)
      (switch-to-buffer-other-window "*scratch*")))
(global-set-key (kbd "s-o") 'switch-to-scratch)

;; Slime mode line
(defun slime-mode-line ()
  (concat (slime-connection-name) " "
          (propertize (downcase (string-trim (slime-current-package) "#?:\\|\"" "\""))
                      'face 'mode-line-buffer-id)))

;; Hacks to make slime-autodoc works better
(setq auto-save-no-message t) ;; Slime auto-saves like crazy for some reason...
(setq eldoc-idle-delay 0)

;;; Paredit enhancements

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
  (when (or (eq this-command 'eval-expression)
            (string-prefix-p "sldb" (symbol-name this-command))
            (string-prefix-p "slime" (symbol-name this-command)))
    (paredit-mode)
    (company-mode)))
(add-hook 'minibuffer-setup-hook 'sexp-minibuffer-hook)

;; Slime debug window non-prolifiration
(add-to-list 'display-buffer-alist '("\\`*sldb" (display-buffer-reuse-mode-window)))

;;; Magit

(require 'magit)
(defun cloc-magit-root ()
  (interactive)
  (message (shell-command-to-string
            (concat "cloc " (magit-toplevel)))))
(add-hook 'magit-post-commit-hook 'cloc-magit-root)

;;; window/buffer/frame/workspaces movement

(require 'zygospore)
(require 'windmove)
(windmove-default-keybindings)
;; Moving between window/buffer/frame/workspaces in 4 directions
(defun next-workspace (direction)
  (case direction
    (left (exwm-workspace-switch (1- exwm-workspace-current-index)))
    (right (exwm-workspace-switch (1+ exwm-workspace-current-index)))))
(if (not (k-exwm-enabled-p))
    (require 'framemove)
    (defun windmove-select-advice (orig-func dir &rest args)
      "Let windmove do its own thing, if there is an error, try framemove in that direction."
      (condition-case err
                      (apply orig-func dir args)
                      (error (next-workspace dir))))
    (advice-add 'windmove-do-window-select :around #'windmove-select-advice))
(setq framemove-hook-into-windmove t)
(require 'buffer-move)
;; Intuitively, this works like windmove but move buffer together with cursor.
(global-set-key (kbd "C-s-p") #'buf-move-up)
(global-set-key (kbd "C-s-n") #'buf-move-down)
(global-set-key (kbd "C-s-r") #'buf-move-right)
(global-set-key (kbd "C-s-l") #'buf-move-left)
;; Override buffer-move to support inter-frame/inter-exwm-workspace buffer movement.
(defun buf-move-to (direction)
  "Helper function to move the current buffer to the window in the given
   direction (with must be 'up, 'down', 'left or 'right). An error is
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

(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Switch to interactive Scheme buffer." t)
(setq auto-mode-alist (cons '("\\.ss" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sls" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.scm" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (delq (assoc "\\.rkt\\'" auto-mode-alist) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lisp" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.z" . z3-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ly" . lilypond-mode) auto-mode-alist))

;;; EMMS

(require 'emms-setup)
(emms-all)
(require 'emms-player-mpv)
(setq emms-player-list '(emms-player-mpv))
(setq emms-source-file-default-directory "~/Music/EMMS/")
(setq emms-mode-line-format "%s")
(require 'emms-info-tinytag)
(executable-find "python2.7")
(cond ((executable-find "python"))
      ((executable-find "python3") (setq emms-info-tinytag-python-name "python3"))
      (t (warn "Unable to guess python3 path for emms-info-tinytag.")))
(setq global-mode-string (delete 'emms-mode-line-string (delete 'emms-playing-time-string global-mode-string)))

(defun k-emms-toggle-video (&rest args)
  "Tell MPV player to switch to video/no-video mode."
  (interactive)
  (let* ((no-video-now (member "--no-video" emms-player-mpv-parameters))
         (no-video-wanted (if args (car args) (not no-video-now))))
    (if no-video-wanted
        (add-to-list 'emms-player-mpv-parameters "--no-video")
        (setq emms-player-mpv-parameters (delete "--no-video" emms-player-mpv-parameters)))
    (when (process-live-p emms-player-mpv-proc)
      (if no-video-now
          (unless no-video-wanted
            (emms-stop)
            (emms-player-mpv-proc-stop)
            (while
             (process-live-p emms-player-mpv-proc)
             (sleep-for 0.1))
            (sleep-for 0.1)
            (emms-start))
          (emms-player-mpv-cmd '(set vid no))))))

;;; Mode line

(defun k-pad-mode-line-format (mode-line-format)
  (unless (stringp mode-line-format)
    (setq mode-line-format (format-mode-line mode-line-format)))
  `(#(" " 0 1 (face default display (space :width left-fringe)))
     ,(truncate-string-to-width
       mode-line-format
       (window-text-width (get-buffer-window (current-buffer)))
       nil nil (truncate-string-ellipsis))
     #(" " 0 1 (display (space :align-to right)))
     #(" " 0 1 (face default display (space :width right-fringe)))))
(defvar k-selected-window nil)
(defun k-set-selected-window ()
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq k-selected-window (frame-selected-window))))
(defun k-mode-line-selected-p ()
  (eq (selected-window) k-selected-window))
(add-hook 'window-state-change-hook 'k-set-selected-window)
(setq-default mode-line-format
              '(:eval
                (k-pad-mode-line-format
                 '("%e" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
                   mode-line-frame-identification
                   (14 (:eval (if (k-mode-line-selected-p) #("%c" 0 2 (face mode-line-emphasis))
                                  "%c"))
                    (#(" %l/" 0 3 (face mode-line-highlight))
                           (:propertize (:eval (number-to-string (line-number-at-pos (point-max))))
                            face bold))) " "
                   (:propertize "%b" face mode-line-buffer-id)
                   ((which-func-mode which-func-format)) " \t"
                   (mode-line-process ("(" mode-name ":" mode-line-process  ")")
                    mode-name)
                   mode-line-misc-info
                   " " (:eval (if (k-mode-line-selected-p)
                                  (concat (propertize (format-seconds "%.2h:%z%.2m:%.2s" emms-playing-time)
                                                      'face 'mode-line-highlight) "/"
                                          (propertize
                                           (let ((total (emms-track-get
                                                         (emms-playlist-current-selected-track)
                                                         'info-playing-time)))
                                             (if total (format-seconds "%.2h:%z%.2m:%.2s" total) "unknown"))
                                           'face 'bold) " "
                                          (propertize emms-mode-line-string 'face 'emms-mode-line-title))
                                  ""))
                   " " (slime-mode (:eval (slime-mode-line)))))))

(defvar-local k-pad-last-header-line-format nil)
(defun k-pad-header-line-after-advice (&optional object &rest args)
  (cond ((framep object)
         (dolist (window (window-list object 'no-minibuf))
           (k-pad-header-line-after-advice (window-buffer window))))
        ((bufferp object) (with-current-buffer object (k-pad-header-line-after-advice)))
        (t (unless (equal header-line-format k-pad-last-header-line-format)
             (setq-local header-line-format (k-pad-mode-line-format header-line-format))
             (setq-local k-pad-last-header-line-format header-line-format)))))
(add-hook 'window-buffer-change-functions 'k-pad-header-line-after-advice)

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
(define-key vterm-mode-map (kbd "C-c C-o") 'vterm-clear)
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
              (exwm-workspace-rename-buffer exwm-title)))
  (defun k-eww-reload-in-chromium ()
    (interactive)
    (k-browse-url-chromium (plist-get eww-data :url)))
  (define-key eww-mode-map (kbd "f") 'k-eww-reload-in-chromium))

(require 'ytel)
(setq-default ytel-invidious-api-url "https://vid.puffyan.us"
              ytel-title-video-reserved-space 40
              ytel-author-name-reserved-space 20)
(defun ytel-play (&optional no-video)
  "Play video at point with EMMS."
  (interactive "P")
  (k-emms-toggle-video no-video)
  (let* ((video (ytel-get-current-video))
     	 (id    (ytel-video-id video))
         (url (concat "https://www.youtube.com/watch?v=" id))
         (track (emms-track 'url url)))
    (emms-track-set track 'info-title (ytel-video-title video))
    (emms-track-set track 'info-playing-time (ytel-video-length video))
    (with-current-emms-playlist
        (emms-playlist-insert-track track)
      (emms-playlist-previous)
      (emms-playlist-mode-play-current-track))))
(define-key ytel-mode-map (kbd "RET") 'ytel-play)
(define-key ytel-mode-map (kbd "p") (kbd "C-u RET"))

;;; PDF Tools

(require 'pdf-tools)
(pdf-tools-install)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;; EXWM
(when (eq window-system 'x)
  (global-set-key (kbd "<print>")
                  (lambda ()
                    (interactive)
                    (let ((path (concat "~/Documents/Screenshot-" (format-time-string "%Y-%m-%d,%H:%M:%S") ".png")))
                      (start-process-shell-command
                       "import" nil (concat "import -window root " path))
                      (message (concat "Screenshot saved to " path)))))

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
(setq undo-tree-visualizer-timestamps t)
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
(setq-default org-link-descriptive nil)
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))
(org-babel-do-load-languages 'org-babel-load-languages '((lisp . t)))
(setq org-babel-lisp-eval-fn 'slime-eval)
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

;;; BGEX patch: image background

;; (when (k-exwm-enabled-p)
;;   (require 'bgex)
;;   (bgex-set-image-default "~/Resources/wallpaper-blurred.jpg")
;;   (mapc (lambda (buf-name)
;;           (bgex-set-color buf-name 'bgex-identifier-type-buffer-name "#2e1e57"))
;;         '(" *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*"))
;;   (add-to-list 'default-frame-alist '(alpha . 70)))

;;; Notifications

(require 'sauron)
(setq sauron-separate-frame nil)
(setq sauron-min-priority 3)
(setq sauron-log-buffer-max-lines most-positive-fixnum)
(defadvice notifications-notify
    (after sr-notifications-hook (&rest params) disable)
  "\"Hook\" `sauron-add-event' to `notifications-notify'"
  (let ((title (plist-get params :title))
        (body (plist-get params :body))
        (prio (sr-notifications-urgency-to-priority
               (plist-get params :urgency)))
        (callback (plist-get params :on-action)))
    (sauron-add-event
     'notify
     prio
     (concat title
             (if (and title body) " - ") body)
     callback)))
(ad-enable-advice 'notifications-notify 'after 'sr-notifications-hook)
(ad-activate 'notifications-notify)

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

;;; Input Method

(require 'pyim)
(require 'pyim-basedict)
(pyim-basedict-enable)
(pyim-cangjie6dict-enable)
(setq-default pyim-punctuation-translate-p '(auto no yes))
(set-input-method 'pyim)
(deactivate-input-method)

;;; Misc handy commands

(defun lookup-word (word)
  (interactive (list (thing-at-point 'word t)))
  (setq available-windows
        (delete (selected-window) (window-list)))
  (setq new-window
        (or (car available-windows)
            (split-window-sensibly)
            (split-window-right)))
  (select-window new-window)
  (browse-url (format "https://en.wiktionary.org/wiki/%s#Latin" word)))

(defun demolish-package (symbol)
  "Nuke everything under namespace SYMBOL."
  (unload-feature symbol t)
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
                       (with-current-buffer buffer
                         (condition-case
                          nil
                          (save-excursion
                           (search-backward candy)
                           (let ((time (format-seconds "%dd %hh %z%mm"
                                                       (- (org-time-stamp-to-now (buffer-substring (line-beginning-position) (point)) t)))))
                             (concat " (" time ")")))
                          (search-failed nil))))))
             (candy (completing-read "Choose your posion: " candy-list)))
        (org-insert-time-stamp nil t)
        (insert " " (substring-no-properties candy) "\n")
        (save-buffer)))))

(provide 'init)
;;; init.el ends here
