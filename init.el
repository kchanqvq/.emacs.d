;;; init.el --- kchan's emacs config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Preamble

;; Turn off GC during startup
(setq gc-cons-threshold (* 1024 1024 1024) gc-cons-percentage 1.0)

;; Reset some variables, so that hot loading via eval-buffer/load-file
;; has more faithful behavior.

(setq-default emacs-startup-hook nil
              default-frame-alist nil)

;; Misc libraries
(require 'nadvice)
(require 'subr-x)
(require 'mule-util)
(require 'color)

;;; Bootstrap straight.el

(setq straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq-default straight-use-package-by-default t
              use-package-always-defer t)

(straight-use-package 'use-package)

;;; Util functions

(use-package alist :straight apel :demand t)

(use-package s :demand t)

(defmacro globalize (mode)
  "Define a global minor mode from MODE, and add to `emacs-startup-hook'."
  (let ((%global-mode-symbol (intern (concat "global-" (symbol-name mode)))))
    `(progn
       (define-globalized-minor-mode ,%global-mode-symbol ,mode
         (lambda () (,mode)))
       (add-hook 'emacs-startup-hook ',%global-mode-symbol))))

(defvar k-exwm-enabled-p nil)

(defun k-guix-p ()
  (executable-find "guix"))

(defmacro k-use-guix-maybe (package)
  `(if (k-guix-p)
       (progn
         (use-package ,package :straight nil :demand t))
     (straight-use-package ',package)))

(defun delete-from-list (list-var element)
  (set list-var (delete element (when (boundp list-var) (symbol-value list-var)))))

(cl-defmacro with-advice ((symbol how lambda-list &body advice) &body body)
  "Temporarily add ADVICE to SYMBOL during evaluation of BODY."
  `(let ((k-advice (lambda ,lambda-list ,@advice)))
     (advice-add ',symbol ,how k-advice)
     (unwind-protect
         (progn ,@body)
       (advice-remove ',symbol k-advice))))

(defun k-run-helper-command (command name &optional continuation silent)
  "Run helper shell COMMAND in buffer with NAME.
Run CONTINUATION once the shell process exited.
If SILENT is non-nil, do not display the NAME buffer."
  (require 'comint)
  (with-current-buffer
      (let ((display-comint-buffer-action
             (if silent
                 '(display-buffer-no-window (allow-no-window . t))
               '(nil (inhibit-same-window . t)))))
        (save-selected-window
          (shell name)))
    (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
    (set-process-sentinel (get-buffer-process (current-buffer))
			  (lambda (_proc _status)
                            (when continuation
                              (funcall continuation))))
    (goto-char (point-max))
    (comint-send-string (get-buffer-process (current-buffer))
                        (concat command "\n"))))

(defun k-global-set-key (key command)
  "Bind KEY to COMMAND, also works in EXWM windows."
  (when k-exwm-enabled-p
    (define-key exwm-mode-map key command))
  (global-set-key key command))

(defun k-fill-right (string)
  "Prepend a variable space to STRING to make it right-aligned."
  (let* ((width (string-pixel-width string)))
    (concat (propertize " " 'display
                        `(space :align-to (- right-fringe (,width))))
            string)))
(byte-compile 'k-fill-right)

(defun k-insert-fill-right (string)
  "Insert STRING and make it right-aligned using a variable space.
This is more correct than (insert (k-fill-right STRING)) in some
cases, because it respect the current buffer settings,
e.g. invisibility spec."
  (let ((from (point)))
    (insert " " string)
    (save-restriction
      (narrow-to-region (1+ from) (point))
      (let ((width (car (buffer-text-pixel-size))))
        (widen)
        (put-text-property from (1+ from)
                           'display
                           `(space :align-to (- right-fringe (,width))))))
    nil))
(byte-compile 'k-insert-fill-right)

(defun k-truncate-string-to-width (string pixel-width)
  "Truncate STRING to PIXEL-WIDTH.
Use binary search."
  (if (> (string-pixel-width string) pixel-width)
      (let* ((a 1) a-result
             (b (length string)))
        (while (> b (+ a 1))
          (let* ((c (ceiling (+ a b) 2))
                 (result (concat (substring string 0 c) (truncate-string-ellipsis))))
            (if (> (string-pixel-width result) pixel-width)
                (setq b c)
              (setq a c a-result result))))
        a-result)
    string))
(byte-compile 'k-truncate-string-to-width)

(defun k-ensure-prefix-map (keymap key)
  (or (keymap-lookup keymap key)
      (keymap-set keymap key (make-sparse-keymap))))

;;; Misc config

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/custom")
(setq-default garbage-collection-messages nil)
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

(load (setq custom-file "~/.emacs.d/custom/custom.el"))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package vlf
  :config
  (setq-default vlf-application 'dont-ask))

(use-package which-key
  ;; Disabled for now because it seem to interact poorly with
  ;; window-management & recurisve-edit. If the invoked command
  ;; changes window configuration, sometimes it seem to interfere with
  ;; which-key's own windows.
  :disabled t
  :hook emacs-startup
  :config
  (setq-default which-key-idle-delay 0
                which-key-use-C-h-commands nil)
  ;; Make `which-key' popup use k-echo-area
  (setq-default which-key-popup-type 'custom
                which-key-custom-show-popup-function
                '(lambda (dim)
                   (k-echo-area-display (selected-window) which-key--buffer))
                which-key-custom-hide-popup-function
                '(lambda ()
                   (k-echo-area-clear-1 (get-buffer-window which-key--buffer)))
                which-key-custom-popup-max-dimensions-function
                '(lambda (w) (cons 20 w))))

;;; ⭐ Mode line

(defun k-pad-mode-line-format (format &optional right-format)
  "Format the mode line as a string according to FORMAT and RIGHT-FORMAT.
FORMAT is left-aligned and RIGHT-FORMAT is right-aligned.  Add
padding space at the left and right of the mode line so that the
edge of the mode line align with left and right fringe."
  (unless (stringp format)
    (setq format (format-mode-line format)))
  (when right-format
    (unless (stringp right-format)
      (setq right-format (format-mode-line right-format)))
    (setq format (concat format (k-fill-right right-format))))
  `(#(" " 0 1 (face default display (space :width left-fringe)))
    ;; ,(truncate-string-to-width
    ;;   format
    ;;   (window-text-width (get-buffer-window (current-buffer)))
    ;;   nil nil (truncate-string-ellipsis))
    ,(k-truncate-string-to-width format (window-text-width (get-buffer-window) t))
    #(" " 0 1 (display (space :align-to right)))
    #(" " 0 1 (face default display (space :width right-fringe)))))
(byte-compile 'k-pad-mode-line-format)

(defvar k-selected-window nil)

(defun k-set-selected-window ()
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq k-selected-window (frame-selected-window))))

(defsubst k-mode-line-selected-p ()
  (eq (selected-window) k-selected-window))

(add-hook 'window-state-change-hook 'k-set-selected-window)

(defvar-local k-mode-line-format-left
    '(""
      (:propertize "%b" face mode-line-buffer-id)
      " \t"
      mode-line-misc-info))

(defvar-local k-mode-line-format-right
    '(" "
      current-input-method-title " "
      mode-name mode-line-process
      "  "
      (:eval (if (k-mode-line-selected-p) #("%c" 0 2 (face mode-line-emphasis))
               "%c"))
      (#(" %l/" 0 3 (face mode-line-highlight))
       (:propertize (:eval (number-to-string (line-number-at-pos (point-max))))
                    face bold))))

(setq-default mode-line-misc-info
              '((slime-mode (:eval (slime-mode-line)))
                (:eval (if (eq major-mode 'emms-playlist-mode) (k-emms-mode-line) "")))
              mode-line-format
              `(:eval (k-pad-mode-line-format k-mode-line-format-left k-mode-line-format-right))
              tab-line-format nil)

(defvar-local k-pad-last-header-line-format nil)

(defun k-pad-header-line-after-advice (&optional object &rest _args)
  "Add padding to header line using `k-pad-mode-line-format'.
This is intended to be used as an :after advice or (normal or
abnormal) hook.  If OBJECT is not given, pad header line for
current buffer.  If OBJECT is a buffer, pad header line for it.
If OBJECT is a frame, pad header line for all buffers displayed
in it.  The function should be idempotent and suitable for
repeated invocation."
  (cond ((framep object)
         (dolist (window (window-list object 'no-minibuf))
           (k-pad-header-line-after-advice (window-buffer window))))
        ((bufferp object) (with-current-buffer object (k-pad-header-line-after-advice)))
        (t (when header-line-format
             (unless (equal header-line-format k-pad-last-header-line-format)
               (setq-local header-line-format `(:eval (k-pad-mode-line-format ',header-line-format)))
               (setq-local k-pad-last-header-line-format header-line-format))))))
(byte-compile 'k-pad-header-line-after-advice)

(add-hook 'Info-mode-hook #'k-pad-header-line-after-advice)
(add-hook 'window-buffer-change-functions 'k-pad-header-line-after-advice)

(defvar k-inhibit-tab-line nil)

(defun k-compute-tab-line (frame)
  "Add an empty tab line to windows in FRAME to simulate bottom dividers.
Tab lines are not added to windows at the top and windows whose
buffer has non-nill `k-inhibit-tab-line'.

This differs from bottom dividers because it does not add space
below window at the bottom (above echo area)."
  (dolist (w (window-list frame))
    (with-current-buffer (window-buffer w)
      (unless k-inhibit-tab-line
        (if (< (cadr (window-edges w)) 2)
            (set-window-parameter w 'tab-line-format nil)
          (set-window-parameter w 'tab-line-format " "))))))

(add-hook 'window-buffer-change-functions 'k-compute-tab-line)

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;;; In buffer completion (company)

(defvar k--company-current-index)

(use-package company
  :hook (emacs-startup . global-company-mode)
  :config
  (setq-default company-backends '(company-capf company-files))
  (setq-default company-dabbrev-downcase nil)
  (setq-default company-tooltip-align-annotations t
                company-format-margin-function nil)

  ;; Zebra strips, to look consistent with vertico
  ;; Patch `company--create-lines' and `company-fill-propertize'
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
  (byte-compile 'company--create-lines)

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
  (byte-compile 'company-fill-propertize)

  ;; ⭐ Don't let `company-elisp' quickhelp hijack `*Help*' buffer
  (defvar k-help-buffer-override nil)

  (define-advice company-capf
      (:around (orig &rest args) k-help-buffer-override)
    (let ((k-help-buffer-override "*company-documentation*"))
      (apply orig args)))

  (define-advice help-buffer
      (:around (orig) k-help-buffer-override)
    (or (when k-help-buffer-override
          (get-buffer-create k-help-buffer-override))
        (funcall orig))))

;; Use posframe so that company works in minibuffer
(use-package company-posframe
  :hook emacs-startup
  :config
  (setq-default company-posframe-show-indicator nil
                company-posframe-show-metadata nil
                company-posframe-quickhelp-show-header nil
                company-posframe-quickhelp-delay nil
                company-posframe-show-params '(:internal-border-width 1)
                company-posframe-quickhelp-show-params
                '(:poshandler company-posframe-quickhelp-right-poshandler
                              :internal-border-width 1))
  (advice-add 'company-posframe-show :after (lambda () (company-posframe-quickhelp-show)))
  (company-posframe-mode))

;;; Generic stripes
;; I prefer using text-property to color stuff,
;; but when I don't feel like trying I use `stripes' overlays.

(use-package stripes
  :config
  (setq-default stripes-unit 1 stripes-overlay-priority 0))

(use-package hl-line
  :config
  (setq-default hl-line-overlay-priority 5)
  ;; Patch `hl-line-make-overlay' so that front advance is T
  (defun hl-line-make-overlay ()
    (let ((ol (make-overlay (point) (point) nil t nil)))
      (overlay-put ol 'priority hl-line-overlay-priority) ;(bug#16192)
      (overlay-put ol 'face hl-line-face)
      ol)))

;;; ⭐ Theme
;; I generate theme algorithmically from a list of hue and saturation
;; values. There're two hand-tune ones accessible using
;; `k-theme-switch', and my EMMS integration generate themes from
;; YouTube video thumbnail when it is played.

(setenv "GREP_COLOR" "31")
(setq-default k-color-style 'bright)

(deftheme k)

;;;; Font Inventory

(defun k-set-fonts (scripts spec)
  (dolist (script scripts)
    (set-fontset-font t script nil)
    (if (listp spec)
        (dolist (s spec)
          (set-fontset-font t script s nil t))
      (set-fontset-font t script spec))))
(k-set-fonts '(han kana cjk-misc)
             (font-spec :family "PingFang SC"))
;; (k-set-fonts '(cyrillic phonetic)
;;   (font-spec :family "Noto Sans" :size 18))
;; (k-set-fonts  '(hebrew)
;;               (font-spec :family "Arial Hebrew" :size 16))
(k-set-fonts '(emoji symbol)
             (list (font-spec :family "Hiragino Sans" :size 16)
                   (font-spec :family "Noto Emoji" :size 16)
                   (cond ((member "Apple Color Emoji" (font-family-list))
                          (font-spec :family "Apple Color Emoji" :size 16))
                         (t (font-spec :family "Noto Color Emoji")))))

;; Tweek fonts to  match `window-text-pixel-size'
(defvar k-light-monospace "Source Code Pro-20:weight=light")
(defvar k-monospace "Source Code Pro")
(defvar k-serif-monospace "Libertinus Mono-19")

;;;; Color palette
(defvar k-bg-blue)
(defvar k-fg-blue)
(defvar k-dk-blue)
(defvar k-bg-purple)
(defvar k-fg-purple)
(defvar k-dk-purple)
(defvar k-bg-pink)
(defvar k-fg-pink)
(defvar k-dk-pink)
(defvar k-bg-grey-1)
(defvar k-bg-grey-2)
(defvar k-bg-grey-3)
(defvar k-fg-red)

(defvar k-bg)
(defvar k-fg)
(defvar k-fg-1)
(defvar k-theme-dark-p nil)

(defvar k--hsl-sat 1.0)
(defsubst k-hsl-to-hex (h s l)
  (apply #'color-rgb-to-hex (color-hsl-to-rgb h (* s k--hsl-sat) l)))

(defun k-generate-theme (hue-1 sat-1 hue-2 sat-2 hue-3 sat-3 contrast dark-p
                               &optional bg grey-sat lum)
  "Algorithmically generate and load theme.
HUE-1 and SAT-1 is used for `k-*-blue',
HUE-2 and SAT-2 is used for `k-*-purple',
HUE-3 and SAT-3 is used for `k-*-pink'.
CONTRAST is the hue used for `k-fg-red'.
DARK-P specifies whether to generate a dark or light theme."
  (setq k-theme-dark-p dark-p)
  (let ((k--hsl-sat sat-1))
    (if dark-p
        (setq k-bg-blue (k-hsl-to-hex hue-1 0.4 0.2)
              k-fg-blue (k-hsl-to-hex hue-1 1.0 0.7)
              k-dk-blue (k-hsl-to-hex hue-1 1.0 0.77))
      (setq k-bg-blue (k-hsl-to-hex hue-1 1.0 0.87)
            k-fg-blue (k-hsl-to-hex hue-1 1.0 0.75)
            k-dk-blue (k-hsl-to-hex hue-1 1.0 0.5))))

  (let ((k--hsl-sat sat-2))
    (if dark-p
        (setq k-bg-pink (k-hsl-to-hex hue-2 0.4 0.15)
              k-fg-pink (k-hsl-to-hex hue-2 1.0 0.75)
              k-dk-pink (k-hsl-to-hex hue-2 1.0 0.8))
      (setq k-bg-pink (k-hsl-to-hex hue-2 0.9 0.92)
            k-fg-pink (k-hsl-to-hex hue-2 1.0 0.75)
            k-dk-pink (k-hsl-to-hex hue-2 1.0 0.5))))

  (let ((k--hsl-sat sat-3))
    (if dark-p
        (setq k-bg-purple (k-hsl-to-hex hue-3 0.4 0.15)
              k-fg-purple (k-hsl-to-hex hue-3 1.0 0.75)
              k-dk-purple (k-hsl-to-hex hue-3 1.0 0.8))
      (setq k-bg-purple (k-hsl-to-hex hue-3 0.9 0.92)
            k-fg-purple (k-hsl-to-hex hue-3 1.0 0.75)
            k-dk-purple (k-hsl-to-hex hue-3 1.0 0.5))))

  (if dark-p
      (setq k-bg-grey-1 (k-hsl-to-hex hue-1 grey-sat 0.10)
            k-bg-grey-2 (k-hsl-to-hex hue-1 grey-sat 0.15)
            k-bg-grey-3 (k-hsl-to-hex hue-1 grey-sat 0.20))
    (setq k-bg-grey-1 (k-hsl-to-hex 0.0 0.0 0.95)
          k-bg-grey-2 (k-hsl-to-hex 0.0 0.0 0.90)
          k-bg-grey-3 (k-hsl-to-hex 0.0 0.0 0.80)))
  (setq k-fg-red (k-hsl-to-hex contrast 1.0 0.5))

  (let ((k--hsl-sat sat-1))
    (if dark-p
        (setq k-bg (or bg "#000000")
              k-fg-1 (k-hsl-to-hex hue-1 0.3 0.5)
              k-fg (k-hsl-to-hex hue-1 0.25 0.6))
      (setq k-bg (or bg "#ffffff")
            k-fg (k-hsl-to-hex 0.0 0.0 0.0)
            k-fg-1 (k-hsl-to-hex
                    (if (< (mod (- hue-2 hue-1) 1.0) 0.5)
                        (+ hue-1 0.1)
                      (- hue-1 0.1))
                    0.20 0.53))))
  (k-load-faces)
  (defconst blink-cursor-colors (list k-fg-blue k-fg-pink k-fg-purple))
  (if dark-p
      (progn
        ;; (defconst blink-background-colors (list k-bg k-bg-blue k-bg-pink k-bg-purple))
        (setq-default face-near-same-color-threshold 50000)
        (setq-default pdf-view-midnight-colors (cons k-fg k-bg))
        (add-to-list 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (derived-mode-p 'pdf-view-mode)
              (pdf-view-midnight-minor-mode)))))
    (progn
      (setq-default face-near-same-color-threshold 30000)
      (delete-from-list 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'pdf-view-mode)
            (pdf-view-midnight-minor-mode -1)))))))

;;;; Face inventory
(defface k-quote nil "Base face for quote.")
(defface k-keyword nil "Base face for keyword.")
(defface k-proper-name nil "Base face for proper name.")
(defface k-doc nil "Base face for doc.")
(defface k-string nil "Base face for strings.")
(defface k-comment nil "Base face for comment.")
(defface k-common nil "Base face for common match substring.")
(defface k-prompt nil "Base face for prompts.")
(defface k-zebra nil "Base face for zebra stripes.")
(defface k-monochrome-emoji nil "Monochrome emoji face.")
(defface k-separator-overline nil "Face for separator overlines.")

;;;; Misc settings
(defface emms-mode-line-title nil "Face for EMMS track title in mode line.")
(setq-default goto-address-mail-face '(button k-quote))

;;;; Generate faces
(defun k-load-faces ()
  "Generate and set faces."
  (setq custom--inhibit-theme-enable nil)

  (custom-theme-set-faces
   'k
   `(default ((default :font ,k-light-monospace :background ,k-bg :foreground ,k-fg
                       :distant-foreground ,(if k-theme-dark-p "#000000" k-bg)
                       :weight light)))
   `(fixed-pitch ((default :family ,k-monospace :weight light)))
   `(fixed-pitch-serif ((default :family "Courier" :weight light)))
   `(variable-pitch ((default :family "Noto Sans" :weight light)))
   `(bold ((default :weight normal)))
   '(bold-italic ((default :inherit (bold italic))))
   '(underline ((default :underline t)))
   '(italic ((default :slant italic)))
   `(k-quote ((default :inherit (fixed-pitch-serif bold))))
   `(k-keyword ((default :foreground ,(if k-theme-dark-p k-dk-blue k-fg-1)
                         :inherit ,(if k-theme-dark-p nil 'bold))))
   `(k-proper-name ((default :inherit k-quote :foreground ,k-fg)))
   `(k-string ((default :foreground ,k-dk-pink)))
   `(k-doc ((default :font ,k-serif-monospace :inherit (k-string bold))))
   `(shadow ((default :foreground ,k-fg-1)))
   `(k-comment ((default :inherit (italic shadow))))
   `(k-common ((default :foreground ,k-fg :inherit bold)))
   `(k-prompt ((default :inherit bold :foreground ,k-fg-pink)))
   (if k-theme-dark-p
       `(k-zebra ((default :background ,k-bg-grey-1 :extend t)))
     `(k-zebra ((default :background ,k-bg-blue :extend t))))
   (if k-theme-dark-p
       `(k-separator-overline ((default :overline ,k-bg-grey-2)))
     `(k-separator-overline ((default :overline ,k-fg))))
   `(k-monochrome-emoji ((default :font ,(font-spec :family "Noto Emoji" :size 16))))
   '(font-lock-builtin-face ((default :inherit k-keyword)))
   `(font-lock-comment-face ((default :inherit k-comment)))
   '(font-lock-comment-delimiter-face ((default :inherit k-comment)))
   `(font-lock-doc-face ((default :inherit k-doc)))
   `(font-lock-doc-string-face ((default :inherit k-doc)))
   `(font-lock-string-face ((default :inherit k-string)))
   '(font-lock-function-name-face ((default :inherit k-proper-name)))
   '(font-lock-variable-name-face ((default :inherit k-proper-name)))
   '(font-lock-keyword-face ((default :inherit k-keyword)))
   ;; `(font-lock-negation-char-face ((,class (:foreground "#afafaf"))))
   `(font-lock-constant-face ((default :inherit k-string)))
   ;; `(font-lock-preprocessor-face ((,class (:foreground ,success))))
   `(font-lock-regexp-grouping-backslash ((default :inherit bold)))
   `(font-lock-regexp-grouping-construct ((default :inherit bold)))

   `(font-lock-type-face ((default :inherit k-proper-name)))
   `(font-lock-warning-face ((default :inherit warning)))
   `(error ((default :foreground ,k-fg-red :inherit bold)))
   `(success ((default :foreground ,k-dk-blue)))
   `(warning ((default :foreground ,k-dk-pink :inherit bold)))
   `(escape-glyph ((default :foreground ,k-fg-red :distant-foreground ,k-fg-red)))

   ;; Flycheck
   `(flycheck-error-list-highlight ((default :inherit bold)))
   `(flycheck-info ((default :underline (:style wave :color ,k-fg-blue))))
   `(flycheck-warning ((default :underline (:style wave :color ,k-fg-pink))))
   `(flycheck-error ((default :underline (:style wave :color ,k-fg-red))))
   ;; `(flycheck-info ((,class (:underline (:style wave :color ,warning)))))
   `(lsp-ui-sideline-code-action ((default :inherit button)))

   ;; Search
   `(match ((default :background ,k-bg-pink)))
   `(lazy-highlight ((default :distant-foreground ,k-bg :inherit region)))
   `(isearch ((default :inherit match)))
   `(isearch-fail ((default :foreground ,k-fg-red)))

   ;; Avy
   `(avy-lead-face ((default :background ,k-dk-blue :foreground ,k-bg
                             :inherit bold)))
   `(avy-lead-face-0 ((default :inherit avy-lead-face)))
   `(avy-lead-face-1 ((default :inherit avy-lead-face)))
   `(avy-lead-face-2 ((default :inherit avy-lead-face)))

   ;; Emacs interface
   `(cursor ((default (:background ,k-fg-pink))))
   `(fringe ((default :foreground ,k-fg-1 :background ,k-bg)))
   `(vertical-border ((default :foreground ,k-bg)))
   `(window-divider ((default :foreground ,k-bg :distant-foreground ,k-bg)))
   `(window-divider-first-pixel ((default :foreground ,k-bg :distant-foreground ,k-bg)))
   `(window-divider-last-pixel ((default :foreground ,k-bg :distant-foreground ,k-bg)))
   `(border ((default :inherit fringe)))
   `(highlight ((default :inherit region)))
   ;; `(gui-element ((,class (:background ,contrast-bg))))
   `(internal-border ((default (:background ,k-bg))))
   `(child-frame-border ((default (:background ,k-bg-blue))))
   `(tab-line ((default :background ,k-bg :height 2.0)))

   (if k-theme-dark-p
       `(mode-line ((default :background ,k-bg-grey-1)))
     `(mode-line ((default :background ,k-bg-blue))))
   `(mode-line-inactive ((default :inherit mode-line)))
   `(mode-line-buffer-id ((default :inherit bold)))
   `(mode-line-emphasis ((default :foreground ,(if (equal k-dk-blue k-dk-purple) k-dk-pink k-dk-purple)
                                  :inherit bold)))
   `(mode-line-highlight ((default :foreground ,k-dk-blue :inherit bold)))
   `(minibuffer-prompt ((default :inherit bold)))
   `(region ((default :background ,k-bg-blue)))
   `(secondary-selection ((default :background ,k-bg-blue)))
   (if k-theme-dark-p
       `(header-line ((default :background ,k-bg :underline ,k-bg-grey-2)))
     `(header-line ((default :background ,k-bg :underline ,k-fg))))

   `(button ((default :underline t :foreground ,k-fg :inherit bold)))
   `(link ((default :foreground ,k-fg-1 :underline t :inherit bold)))
   `(link-visited ((default :foreground ,k-dk-pink :inherit link)))
   `(info-menu-star ((default :foreground ,k-dk-pink)))
   `(info-header-node ((default :inherit outline-1)))
   `(info-menu-header ((default :inherit outline-2)))
   `(widget-button ((default :foreground ,k-fg :background ,k-bg-blue :box (:line-width 1))))
   `(widget-field ((default :foreground ,k-fg :background ,k-bg-blue :box (:line-width 1))))

   ;; `(trailing-whitespace ((,class (:background ,warning :foreground ,highlight))))
   ;; `(whitespace-empty ((,class (:foreground ,warning :background ,highlight))))
   ;; `(whitespace-hspace ((,class (:background ,contrast-bg))))
   ;; `(whitespace-indentation ((,class (:background ,contrast-bg))))
   ;; `(whitespace-line ((,class (:background ,contrast-bg))))
   ;; `(whitespace-newline ((,class (:background ,contrast-bg))))
   ;; `(whitespace-space ((,class (:background ,contrast-bg))))
   ;; `(whitespace-space-after-tab ((,class (:background ,contrast-bg))))
   ;; `(whitespace-space-before-tab ((,class (:background ,contrast-bg))))
   ;; `(whitespace-tab ((,class (:background ,contrast-bg))))
   ;; `(whitespace-trailing ((,class (:background ,contrast-bg))))

   ;; Parenthesis matching (built-in)))
   `(show-paren-match ((default :background ,k-bg-pink)))
   `(show-paren-mismatch ((default :background ,k-bg-pink)))

   ;; `(sh-heerroroc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
   ;; `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
   ;; `(mmm-declaration-submode-face ((,class (:inherit font-lock-doc-face :background ,(blend-color success background 0.2) :extend t))))
   ;; `(slime-highlight-edits-face ((,class (:weight bold))))
   `(slime-repl-input-face ((default :weight normal)))
   `(slime-repl-prompt-face ((default :inherit k-prompt)))
   `(slime-repl-output-face ((default :background ,k-bg-grey-1 :extend t)))
   `(slime-repl-result-face ((default :background ,k-bg-grey-1 :extend t :inherit k-comment)))
   `(slime-repl-inputed-output-face ((default :foreground ,k-dk-pink :slant normal)))
   `(slime-repl-output-mouseover-face ((default :inherit (match slime-repl-inputed-output-face))))
   `(sldb-restartable-frame-line-face ((default :inherit button)))

   `(clojure-keyword-face ((default :inherit k-keyword)))
   `(cider-result-overlay-face ((default :box (:line-width (-1 . -1) :color ,k-dk-blue))))
   `(cider-traced-face ((default :inherit region)))
   ;; `(csv-separator-face ((,class (:foreground ,warning))))

   ;; `(diff-added ((,class (:foreground ,string))))
   ;; `(diff-changed ((,class (:foreground ,success))))
   ;; `(diff-removed ((,class (:foreground ,warning))))
   ;; `(diff-header ((,class (:foreground ,warning :background nil))))
   ;; `(diff-file-header ((,class (:foreground ,keyword :background nil))))
   ;; `(diff-hunk-header ((,class (:foreground ,success))))
   ;; `(diff-refine-added ((,class (:inherit diff-added :inverse-video t))))
   ;; `(diff-refine-removed ((,class (:inherit diff-removed :inverse-video t))))

   ;; `(diff-hl-insert ((,class (:background ,string))))
   ;; `(diff-hl-change ((,class (:background ,keyword))))
   ;; `(diff-hl-delete ((,class (:background ,warning))))
   ;; `(diff-hl-unknown ((,class (:background ,success))))

   ;; `(ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
   ;; `(ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
   ;; `(ediff-odd-diff-A  ((,class (:foreground ,comment :background nil :inverse-video t))))
   ;; `(ediff-odd-diff-B  ((,class (:foreground ,comment :background nil :inverse-video t))))

   ;; `(eldoc-highlight-function-argument ((,class (:foreground ,string :weight bold))))

   ;; macrostep
   `(macrostep-expansion-highlight-face ((default :background ,k-bg-grey-1 :extend t)))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((default :inherit shadow)))
   `(undo-tree-visualizer-current-face ((default :foreground ,k-dk-pink :inherit bold)))
   `(undo-tree-visualizer-active-branch-face ((default :foreground ,k-dk-blue :inherit bold)))
   ;; `(undo-tree-visualizer-register-face ((,class (:foreground ,highlight))))
   `(vundo-highlight ((default :foreground ,k-fg-pink)))
   `(vundo-saved ((default :foreground ,k-fg-blue)))

   ;; Magit

   `(magit-diff-added ((default :background ,k-bg-blue :foreground ,k-fg-1)))
   `(magit-diffstat-added ((default :foreground ,k-dk-blue)))
   `(magit-diff-added-highlight ((default :background ,k-bg-blue)))
   `(magit-diff-removed ((default :background ,k-bg-pink :foreground ,k-fg-1)))
   `(magit-diffstat-removed ((default :foreground ,k-dk-pink)))
   `(magit-diff-removed-highlight ((default :background ,k-bg-pink)))
   `(magit-diff-context ((default :background ,k-bg :foreground ,k-fg-1)))
   `(magit-diff-context-highlight ((default :background ,k-bg-grey-1 :foreground ,k-fg)))
   `(magit-diff-hunk-heading ((default :background ,k-bg-grey-2)))
   `(magit-diff-hunk-heading-highlight ((default :background ,k-bg-grey-3)))
   `(magit-diff-file-heading ((default :inherit bold)))
   `(magit-diff-file-heading-highlight ((default :inherit (bold magit-diff-context-highlight))))
   ;; `(magit-blame-heading ((,class (:background ,darker-bg :foreground ,warning))))
   ;; `(magit-blame-date ((,class (:foreground ,error))))
   ;; `(magit-header-line ((,class (:inherit nil :weight bold))))
   ;; `(magit-dimmed ((,class (:foreground ,comment))))
   ;; `(magit-hash ((,class (:foreground ,comment))))
   ;; `(magit-tag ((,class (:foreground ,highlight))))
   `(magit-branch-local ((default :foreground ,k-dk-blue :inherit bold)))
   `(magit-branch-remote ((default :foreground ,k-dk-purple :inherit bold)))
   `(magit-branch-current ((default :box t :inherit magit-branch-local)))
   ;; `(magit-refname ((,class (:inherit comment))))
   ;; `(magit-signature-good ((,class (:inherit success))))
   ;; `(magit-signature-bad ((,class (:inherit error))))
   ;; `(magit-signature-untrusted ((,class (:foreground ,warning))))
   ;; `(magit-signature-unmatched ((,class (:foreground ,warning))))
   ;; `(magit-cherry-equivalent ((,class (:foreground ,success))))

   ;; `(magit-log-graph ((,class (:foreground ,comment))))
   ;; `(magit-log-author ((,class (:foreground ,warning))))
   ;; `(magit-log-date ((,class (:foreground ,keyword))))

   ;; `(magit-process-ok ((,class (:inherit success))))
   ;; `(magit-process-ng ((,class (:inherit error))))
   `(magit-section-heading ((default :inherit (outline-2 success))))
   ;; `(magit-section-heading-selection ((,class (:foreground ,warning :weight bold))))
   `(magit-section-highlight ((default :background ,k-bg-grey-1)))

   ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)))
   ;; `(compilation-column-number ((,class (:foreground ,highlight))))
   ;; `(compilation-line-number ((,class (:foreground ,highlight))))
   ;; `(compilation-message-face ((,class (:foreground ,keyword))))
   ;; `(compilation-mode-line-exit ((,class (:foreground ,string))))
   ;; `(compilation-mode-line-fail ((,class (:foreground ,error))))
   ;; `(compilation-mode-line-run ((,class (:foreground ,keyword))))

   ;; Dired
   `(dired-directory ((default :inherit bold)))
   ;; Grep
   ;; `(grep-context-face ((,class (:foreground ,comment))))
   ;; `(grep-error-face ((,class (:foreground ,error :weight bold :underline t))))
   ;; `(grep-hit-face ((,class (:foreground ,keyword))))
   ;; `(grep-match-face ((,class (:foreground nil :background nil :inherit match))))

   ;; `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

   ;; Completion
   `(vertico-current ((default :inherit match :extend t)))
   `(vertico-group-title ((default :background ,k-bg-purple :inherit bold)))
   `(vertico-group-separator ((default :background ,k-bg-purple :strike-through t :inherit shadow)))
   `(embark-target ((default :inherit vertico-current)))
   `(completions-common-part ((default :inherit k-common)))
   `(completions-annotations ((default :inherit k-comment)))
   `(orderless-match-face-0 ((default :inherit k-common)))
   `(orderless-match-face-1 ((default :inherit k-common :foreground ,k-dk-blue)))
   `(orderless-match-face-2 ((default :inherit k-common :foreground ,k-dk-purple)))
   `(orderless-match-face-3 ((default :inherit k-common :foreground ,k-dk-pink)))
   ;; company
   ;; `(company-preview ((,class (:foreground ,comment :background ,contrast-bg))))
   ;; `(company-preview-common ((,class (:inherit company-preview :foreground ,success))))
   ;; `(company-preview-search ((,class (:inherit company-preview :foreground ,keyword))))

   `(company-tooltip ((default :background ,k-bg)))
   `(company-tooltip-selection ((default :inherit match)))
   `(company-tooltip-scrollbar-thumb ((default :background ,k-bg-purple)))
   `(company-tooltip-scrollbar-track ((default :background ,k-bg-pink)))
   `(company-tooltip-common ((default :inherit k-common)))
   `(company-tooltip-common-selection ((default :inherit k-common)))
   `(company-tooltip-search ((default :inherit k-common)))
   `(company-tooltip-search-selection ((default :inherit k-common)))
   ;; `(company-echo-common ((,class (:inherit company-echo :foreground ,function))))

   ;; LaTeX
   `(font-latex-math-face ((default :inherit k-keyword)))
   `(font-latex-sedate-face ((default)))
   `(font-latex-warning-face ((default :inherit warning)))
   `(font-latex-script-char-face ((default :foreground ,k-dk-pink)))
   `(font-latex-string-face ((default :inherit k-string)))
   `(font-latex-sectioning-0-face ((default :inherit outline-1)))
   `(font-latex-sectioning-1-face ((default :inherit outline-1)))
   `(font-latex-sectioning-2-face ((default :inherit outline-2)))
   `(font-latex-sectioning-3-face ((default :inherit outline-3)))
   `(font-latex-sectioning-4-face ((default :inherit outline-4)))
   `(font-latex-sectioning-5-face ((default :inherit outline-5)))
   `(font-latex-italic-face ((default :inherit (bold italic))))
   `(font-latex-bold-face ((default :inherit bold)))
   `(preview-face ((default :background ,k-bg-grey-1 :extend t)))

   ;; `(org-agenda-structure ((,class (:foreground ,success))))
   ;; `(org-agenda-date ((,class (:foreground ,keyword :underline nil))))
   ;; `(org-agenda-done ((,class (:foreground ,string))))
   ;; `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
   `(org-block ((default :inherit fixed-pitch)))
   `(org-meta-line ((default :background ,k-bg-grey-1 :extend t)))
   `(org-list-dt ((default :weight normal)))
   ;; `(org-code ((,class (:foreground ,highlight))))
   ;; `(org-column ((,class (:background ,contrast-bg))))
   ;; `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
   `(org-date ((default :foreground ,k-dk-pink :inherit k-quote)))
   ;; `(org-document-info ((,class (:foreground ,warning))))
   ;; `(org-document-info-keyword ((,class (:foreground ,string))))
   `(org-document-title ((default :inherit outline-1)))
   ;; `(org-done ((,class (:foreground ,string))))
   ;; `(org-ellipsis ((,class (:foreground ,comment))))
   ;; `(org-footnote ((,class (:foreground ,warning))))
   ;; `(org-formula ((,class (:foreground ,error))))
   ;; `(org-hide ((,class (:foreground ,background :background ,background))))
   ;; `(org-link ((,class (:foreground ,keyword :underline t))))
   ;; `(org-scheduled ((,class (:foreground ,string))))
   ;; `(org-scheduled-previously ((,class (:foreground ,warning))))
   ;; `(org-scheduled-today ((,class (:foreground ,string))))
   ;; `(org-special-keyword ((,class (:foreground ,warning))))
   ;; `(org-table ((,class (:foreground ,success))))
   `(org-todo ((default (:foreground ,k-fg-red))))
   ;; `(org-upcoming-deadline ((,class (:foreground ,warning))))
   ;; `(org-warning ((,class (:weight bold :foreground ,error))))

   ;; `(markdown-url-face ((,class (:inherit link))))
   ;; `(markdown-link-face ((,class (:foreground ,keyword :underline t))))

   `(hl-line ((default :inherit region)))
   `(stripes ((default :background ,k-bg-grey-1)))
   `(highlight-indent-guides-character-face
     ((default :foreground ,k-bg-grey-2
               :distant-foreground ,k-bg-grey-2)))

   ;; Message-mode
   `(message-cited-text ((default :foreground ,k-dk-pink)))
   ;; `(message-header-other ((,class (:foreground nil :background nil :weight normal))))
   ;; `(message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,highlight))))
   ;; `(message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,warning))))
   ;; `(message-header-cc ((,class (:inherit message-header-to :foreground nil))))
   ;; `(message-header-name ((,class (:foreground ,keyword :background nil))))
   ;; `(message-header-newsgroups ((,class (:foreground ,warning :background nil :slant normal))))
   ;; `(message-separator ((,class (:foreground ,success))))

   ;; Outline
   `(outline-1  ((default :height 1.5 :inherit outline-8)))
   `(outline-2 ((default :height 1.3 :inherit outline-8)))
   `(outline-3 ((default :height 1.12 :inherit outline-8)))
   `(outline-4 ((default :height 1.0 :inherit outline-8)))
   `(outline-5 ((default :height 1.0 :inherit outline-8)))
   `(outline-6 ((default :height 1.0 :inherit outline-8)))
   `(outline-7 ((default :height 1.0 :inherit outline-8)))
   `(outline-8 ((default :height 240 :foreground ,k-dk-blue  :family "Libertinus Sans" :inherit bold)))

   ;; EMMS
   ;; `(emms-browser-artist-face ((,class (:inherit outline-2))))
   ;; `(emms-browser-album-face ((,class (:inherit outline-3))))
   ;; `(emms-browser-track-face ((,class (:inherit outline-4))))
   ;; `(emms-browser-year/genre-face ((,class (:inherit outline-1))))
   `(emms-playlist-selected-face ((default :inherit match :extend t)))
   `(emms-playlist-track-face ((default :inherit default)))

   ;; ytel
   `(ytel-video-published-face ((default :inherit org-date)))
   `(ytel-channel-name-face ((default :inherit k-proper-name)))
   `(ytel-video-view-face ((default :inherit shadow)))
   `(ytel-video-length-face ((default :inherit shadow)))
   ;; erc
   ;; `(erc-direct-msg-face ((,class (:foreground ,warning))))
   ;; `(erc-error-face ((,class (:foreground ,error))))
   ;; `(erc-header-face ((,class (:foreground ,foreground :background ,darker-bg))))
   `(erc-default-face ((default :inherit variable-pitch)))
   `(erc-input-face ((default :slant italic)))
   `(erc-nick-default-face ((default :inherit k-keyword)))
   `(erc-current-nick-face ((default :inherit erc-nick-default-face)))
   `(erc-my-nick-face ((default :inherit erc-nick-default-face :slant italic)))
   `(erc-nick-msg-face ((default :inherit erc-nick-default-face)))
   `(erc-notice-face ((default :inherit k-comment)))
   `(erc-action-face ((default :slant italic)))
   ;; `(erc-pal-face ((,class (:foreground ,warning))))
   `(erc-prompt-face ((default :inherit k-prompt)))
   `(erc-timestamp-face ((default :foreground ,k-dk-pink)))
   `(erc-keyword-face ((default :inherit k-keyword)))
   `(erc-button ((default :inherit button)))

   ;; Email

   `(message-header-subject ((default :inherit (bold variable-pitch))))
   `(message-header-to ((default :inherit (bold variable-pitch))))
   `(message-header-cc ((default :inherit message-header-to)))
   `(message-header-other ((default :inherit message-header-to)))
   `(message-header-name ((default :inherit k-keyword)))
   `(message-mml ((default :foreground ,k-fg-1)))
   `(notmuch-message-summary-face ((default :inherit message-header-to :background ,k-bg-grey-1)))
   `(notmuch-search-subject ((default :inherit variable-pitch)))
   `(notmuch-search-matching-authors ((default :inherit variable-pitch)))
   `(notmuch-search-date ((default :inherit variable-pitch)))
   `(notmuch-search-unread-face ((default :inherit bold)))
   `(notmuch-tag-unread ((default :foreground ,k-dk-pink)))
   `(notmuch-tag-deleted ((default :strike-through ,k-fg)))
   `(notmuch-tag-face ((default :inherit (shadow k-proper-name))))

   ;; custom
   `(custom-variable-tag ((default :inherit k-proper-name)))
   `(custom-button ((default :inherit widget-button)))
   `(custom-group-tag ((default :inherit outline-1)))
   `(custom-state ((default :inherit shadow)))

   ;; eww
   `(eww-form-text ((default :inherit widget-field)))
   `(eww-form-textarea ((default :inherit eww-form-text)))
   `(eww-form-submit ((default :inherit widget-button)))
   `(eww-form-select ((default :background ,k-bg-pink :inherit eww-form-submit)))
   `(eww-form-checkbox ((default :inherit eww-form-select)))
   `(eww-form-file ((default :background ,k-bg-purple :inherit eww-form-submit)))
   `(eww-valid-certificate ((default :inherit success)))
   `(eww-invalid-certificate ((default :inherit warning)))
   ;; FIXME: Weights got overridden. For some reason eww put `variable-pitch' before `shr-h*' before `shr-link'...
   `(shr-link ((default :inherit button)))
   `(shr-h1 ((default :inherit outline-1)))
   `(shr-h2 ((default :inherit outline-2)))
   `(shr-h3 ((default :inherit outline-3)))

   `(telega-msg-heading ((default :overline ,k-fg :background ,k-bg)))
   `(telega-msg-inline-forward ((default :inherit shadow)))
   `(telega-msg-inline-reply ((default :inherit shadow)))
   `(telega-msg-user-title ((default :inherit (bold variable-pitch) :foreground ,k-fg)))
   `(telega-msg-self-title ((default :inherit (shadow telega-msg-user-title))))
   `(telega-username ((default :inherit telega-msg-user-title)))
   `(telega-user-online-status ((default :foreground ,k-fg-pink)))
   `(telega-chat-prompt ((default :inherit default)))
   `(telega-entity-type-pre ((default :inherit (shadow fixed-pitch))))
   `(telega-entity-type-code ((default :inherit (shadow fixed-pitch))))
   `(telega-entity-type-spoiler ((default :background ,k-fg-1 :foreground ,k-fg-1)))
   `(telega-button ((default :inherit button)))
   `(telega-button-active ((default :inherit (highlight button))))
   `(telega-unmuted-count ((default :foreground ,k-fg-red)))
   `(telega-mention-count ((default :inherit (telega-unmuted-count bold))))
   `(pulse-highlight-start-face ((default :background ,k-bg-blue :extend t)))
   ;; ansi-term
   `(term ((default (:inherit default))))
   `(ansi-color-black   ((default (:foreground ,k-bg))))
   `(ansi-color-red     ((default (:foreground ,k-fg-red))))
   `(ansi-color-green   ((default (:foreground ,k-dk-purple))))
   `(ansi-color-yellow  ((default (:foreground ,k-dk-pink))))
   `(term-color-blue    ((default (:foreground ,k-dk-blue))))
   `(term-color-magenta ((default :foreground ,k-dk-purple)))
   `(term-color-cyan    ((default :foreground ,k-dk-blue)))
   `(term-color-white   ((default (:foreground ,k-bg :background ,k-fg))))

   `(pyim-page ((default :background ,k-bg)))
   `(pyim-page-selection ((default :inherit match)))))

(defun k-theme-switch (style)
  "Elegantly switch to k-theme with STYLE."
  (interactive
   (list (intern (completing-read "Style: " '(bright dark) nil t))))
  (pcase style
    ('bright (k-generate-theme 0.578 1.0 0.920 1.0 0.724 1.0 0.000 nil))
    ('dark (k-generate-theme 0.578 0.9 0.52 1.0 0.578 0.5 0.06 t
                             (k-hsl-to-hex 0.578 0.4 0.06)
                             0.4))))

;;;; GUI tweeks
(scroll-bar-mode -1)
(let ((gap (string-pixel-width "o")))
  ;; make space between windows
  (set-alist 'default-frame-alist 'right-fringe gap)
  (set-alist 'default-frame-alist 'left-fringe gap)
  (set-alist 'default-frame-alist 'right-divider-width (* gap 2))
  ;; make "outer gaps"
  (set-alist 'default-frame-alist 'internal-border-width gap))

(cond
 ((eq window-system 'ns)
  (set-alist 'default-frame-alist 'ns-transparent-titlebar t)
  (set-alist 'default-frame-alist 'ns-appearance 'dark)
  (setq frame-title-format nil)
  (setq ns-use-proxy-icon nil)
  (setq ns-use-native-fullscreen nil)))

(setq-default visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(set-alist 'default-frame-alist 'undecorated t)
(set-alist 'default-frame-alist 'alpha 100)

;; Try not to let underline touch the text.  We use underline to draw
;; a horizontal separator below header line, and this make it look better.
(setq-default underline-minimum-offset 10)

;;; ⭐ Per window echo area
;; This displays "pseudo" echo areas under each window.  I find it
;; more comfy to look at than the global echo area.  I also hacked
;; `vertico-buffer' to display vertico menu in this area, which appears
;; *above* the main window's mode line.

;; The implementation is a mega-hack: we split a echo area window
;; under the main window, set the main window's `mode-line-format'
;; window parameter to `none', and copy its actual mode line to the
;; echo area window, so that the echo area window appears to be above
;; main window's mode line.

(defvar-local k-echo-area--top-separator-overlay nil)
(defvar-local k-echo-area--mode-line nil)
(put 'k-echo-area--mode-line 'risky-local-variable t)

(defun k-echo-area-window (window)
  "Return the k-echo-area window for WINDOW."
  (when-let ((root (window-atom-root window))
             (main-window (window-child root))
             (echo-area-window (window-next-sibling main-window)))
    (when (buffer-local-value 'k-echo-area-mode (window-buffer echo-area-window))
      echo-area-window)))

(defun k-echo-area-main-window (window)
  "Return the window whose k-echo-area is WINDOW."
  (when-let ((root (window-atom-root window))
             (main-window (window-child root))
             (echo-area-window (window-next-sibling main-window)))
    (when (buffer-local-value 'k-echo-area-mode (window-buffer echo-area-window))
      main-window)))

(define-minor-mode k-echo-area-mode
  "Minor mode for k-echo-area buffers."
  :lighter nil
  (if k-echo-area-mode
      (save-excursion
        (setq-local overline-margin 0)
        (goto-char (point-min))
        (goto-char (line-end-position))
        (if k-echo-area--top-separator-overlay
            (move-overlay k-echo-area--top-separator-overlay (point-min) (point))
          (setq k-echo-area--top-separator-overlay (make-overlay (point-min) (point) nil t t))
          (overlay-put k-echo-area--top-separator-overlay 'face 'k-separator-overline)
          (overlay-put k-echo-area--top-separator-overlay 'after-string
                       (propertize
                        " "  'display
                        ;; :align-to calculate width based on buffer
                        ;; column number rather than visual column
                        ;; number, therefore will NOT fill to right
                        ;; edge when there are horizontal scroll.
                        ;; '(space :align-to right)

                        ;; The below solution specifies a sufficiently
                        ;; wide width instead, which seem to yield the
                        ;; desired behavior (always fill to the right)
                        '(space :width text)
                        'face 'k-separator-overline)))
        (setq-local mode-line-format (when k-echo-area--mode-line 'k-echo-area--mode-line)))
    (when k-echo-area--top-separator-overlay
      (delete-overlay k-echo-area--top-separator-overlay)
      (setq k-echo-area--top-separator-overlay nil))))

(defun k-echo-area-display (main-window buf)
  "Display BUF in a k-echo-area window created for MAIN-WINDOW."
  (let (height)
    (with-current-buffer buf
      (setq-local k-echo-area--mode-line
                  (when-let ((parent-mode-line (buffer-local-value 'mode-line-format
                                                                   (window-buffer main-window))))
                    (format-mode-line parent-mode-line)))
      (k-echo-area-mode)
      (setq height (count-screen-lines))
      (let ((echo-area-window (k-echo-area-window main-window)))
        (if echo-area-window
            (set-window-buffer echo-area-window buf)
          (setq echo-area-window
                (display-buffer
                 buf
                 `(display-buffer-in-atom-window
                   (dedicated . weak)
                   (window-parameters
                    (no-other-window . t)
                    (no-delete-other-window . t))))))
        (let (window-size-fixed)
          (set-window-text-height echo-area-window (max height 1)))
        (set-window-parameter main-window 'mode-line-format 'none)
        echo-area-window))))
(set-alist 'window-persistent-parameters 'window-atom t)
(set-alist 'window-persistent-parameters 'mode-line-format t)

(defun k-echo-area-clear (main-window)
  "Remove the k-echo-area window for MAIN-WINDOW."
  (save-selected-window
    (when-let ((echo-area-window (k-echo-area-window main-window)))
      (set-window-parameter main-window 'mode-line-format nil)
      (set-window-parameter echo-area-window 'window-atom nil)
      (delete-window echo-area-window))))

(defun k-echo-area-clear-1 (echo-area-window)
  "Remove the k-echo-area window."
  (when (and (window-live-p echo-area-window)
             (buffer-local-value 'k-echo-area-mode
                                 (window-buffer echo-area-window)))
    (save-selected-window
      (when-let ((main-window (k-echo-area-main-window echo-area-window)))
        (set-window-parameter main-window 'mode-line-format nil))
      (set-window-parameter echo-area-window 'window-atom nil)
      (delete-window echo-area-window))))

(defun k-echo-area-clear-all ()
  "Remove all k-echo-area window, for debug purpose only."
  (dolist (frame (frame-list))
    (dolist (window (window-list frame t))
      (k-echo-area-clear-1 window))))

(add-hook 'k-echo-area-mode-hook '(lambda () (setq-local k-inhibit-tab-line t)))

;;; ⭐ Message to per window echo area

(defvar k-echo-area-message-singleton t)
(defvar-local k-message nil)
(defvar k-message--buffers nil)

(defun k-message (format-string &rest args)
  "Like `message' but in k-echo-area.
Format FORMAT-STRING with ARGS."
  (if (minibufferp (window-buffer))
      (apply #'message format-string args)
    (if format-string
        (setq k-message (apply #'format format-string args))
      (setq k-message nil))))

(defun k-message-display ()
  "Refresh display of `k-message' for current buffer."
  (let ((message k-message))
    (with-current-buffer
        (or (cl-find-if (lambda (buf) (not (get-buffer-window buf t))) k-message--buffers)
            (let ((buf (generate-new-buffer " *echo per window*")))
              (push buf k-message--buffers)
              buf))
      (when k-echo-area-message-singleton
        (dolist (buf k-message--buffers)
          (unless (eq buf (current-buffer))
            (mapc #'k-echo-area-clear-1 (get-buffer-window-list buf nil t)))))
      (setq-local header-line-format nil
                  tab-line-format nil
                  window-size-fixed t)
      (if message
          (progn
            (setq-local cursor-type nil)
            (delete-region (point-min) (point-max))
            (insert message)
            (k-echo-area-display (selected-window) (current-buffer)))
        (when-let ((echo-area-window (k-echo-area-window (selected-window))))
          (when (memq (window-buffer echo-area-window) k-message--buffers)
            (k-echo-area-clear-1 echo-area-window)))))))

(add-hook 'post-command-hook #'k-message-display)
(add-hook 'echo-area-clear-hook '(lambda () (k-message nil)))

;; Use `k-message' for `eldoc'. Pretty comfy!
(setq eldoc-message-function 'k-message)

;;; Time

(use-package time
  :init
  (setq-default world-clock-list '(("BJT-8" "Beijing")
                                   ("America/Los_Angeles" "California"))))

;;; Appearances

(use-package all-the-icons
  :demand t
  :config
  (setq-default all-the-icons-faicon-scale-factor 0.7
                all-the-icons-default-faicon-adjust 0.35
                all-the-icons-material-scale-factor 0.8
                all-the-icons-default-material-adjust 0.1))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode))

(use-package highlight-indent-guides
  :hook (emacs-lisp-mode lisp-mode scheme-mode clojure-mode)
  :config
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line)
  (setq highlight-indent-guides-responsive nil)
  (setq highlight-indent-guides-auto-enabled nil))

(use-package highlight-parentheses
  :init (globalize highlight-parentheses-mode)
  :config
  (setq-default highlight-parentheses-colors '(nil))
  (set-face-attribute 'hl-paren-face nil :inherit 'show-paren-match)
  (show-paren-mode))

(use-package topsy
  :hook (prog-mode)
  :config
  (setq topsy-header-line-format
        `(:eval (or (funcall topsy-fn)
                    ,(propertize "── top ──"
                                 'face 'shadow)))))

(use-package outline
  :bind ( :map outline-minor-mode-map
          ("C-<tab>" . outline-toggle-children))
  :config
  (setq-default outline-minor-mode-buttons '(("▶" "▼" outline--valid-char-p))))

;;; Indent and whitespace

(use-package dtrt-indent
  :hook prog-mode
  :config
  (setq dtrt-indent-verbosity 0))

(use-package ws-butler
  :hook (prog-mode text-mode))

(use-package snap-indent
  :hook
  ( (prog-mode . snap-indent-mode)
    (tex-mode . snap-indent-mode)
    (slime-repl-mode . snap-indent-mode)))

;;; General Programming Utilities

(use-package flycheck
  :bind ( :map flycheck-mode-map
          ("M-n" . flycheck-next-error)
          ("M-p" . flycheck-previous-error))
  :hook (emacs-startup . global-flycheck-mode)
  :config
  (defun k-flycheck-display-error-messages (errors)
    (k-message (flycheck-help-echo-all-error-messages errors)))
  (setq flycheck-display-errors-function #'k-flycheck-display-error-messages)
  (setq-default flycheck-indication-mode nil
                flycheck-global-modes '(not slime-repl-mode lisp-mode))
  (advice-add 'flycheck-jump-to-error :before
              (lambda (_error)
                (unless (get-char-property (point) 'flycheck-error)
                  (push-mark)))))

(use-package lsp-mode
  :bind ( :map lsp-mode-map
          ("s-d" . lsp-execute-code-action))
  :init
  (setq-default lsp-headerline-breadcrumb-enable nil
                lsp-keymap-prefix "<f2>"))

;;; TeX

(use-package lsp-ltex
  :config
  (setq lsp-ltex-version "15.2.0"
        lsp-ltex-latex-environments '(("mathpar" . "ignore"))
        lsp-ltex-latex-commands nil))

(use-package tex
  :straight auctex
  :config
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

  (require 'texmathp)
  (add-to-list 'texmathp-tex-commands '("mathpar" env-on))
  (texmathp-compile))

(use-package cdlatex
  :config
  (setq cdlatex-math-symbol-alist '((42 ("\\times" "\\product")) (43 ("\\cup" "\\sum")))))

;;; ⭐ Completion system

(defvar-local vertico--buffer-window nil)

(use-package vertico
  :hook emacs-startup
  :bind
  ( :map vertico-map
    ("C-s" . k-grep-in-1))
  :config
  (setq-default vertico-count 20)

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

  (defun k-string-pixel-height (string)
    "Return the width of STRING in pixels."
    (if (zerop (length string))
        0
      ;; Keeping a work buffer around is more efficient than creating a
      ;; new temporary buffer.
      (with-current-buffer (get-buffer-create " *string-pixel-width*")
        (delete-region (point-min) (point-max))
        (insert string)
        (cdr (buffer-text-pixel-size
              nil
              ;; workaround: to prevent using vertico's window
              (next-window)
              t nil)))))
  (byte-compile 'k-string-pixel-height)

  ;; Patch `vertico--compute-scroll'
  (defun vertico--compute-scroll ()
    "Update scroll position."
    (let* ((max-scroll (max vertico--index 0))
           (min-scroll max-scroll)
           (height (k-string-pixel-height (nth vertico--index vertico--candidates)))
           (max-height (* (- vertico-count vertico-scroll-margin) (default-line-height))))
      (while (and (cl-plusp min-scroll)
                  (<= (cl-incf height (k-string-pixel-height (nth min-scroll vertico--candidates)))
                      max-height))
        (cl-decf min-scroll))
      (setq vertico--scroll (max min-scroll (min vertico--scroll max-scroll)))))

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
    (vertico--resize-window vertico-count))
  (byte-compile 'vertico--compute-scroll)
  (byte-compile 'vertico--display-candidate))

(use-package vertico-buffer
  :hook vertico-mode
  :straight nil
  :after vertico
  :load-path "straight/repos/vertico/extensions/"
  :config

  ;; we use `fit-window-to-buffer' instead and ignore HEIGHT
  (cl-defmethod vertico--resize-window (height &context (vertico-buffer-mode (eql t)))
    (when k-echo-area--top-separator-overlay
      (overlay-put k-echo-area--top-separator-overlay 'after-string nil))
    (let ((string (overlay-get vertico--candidates-ov 'after-string)))
      (put-text-property 0 1 'display '(space :align-to right) string)
      (put-text-property 0 1 'face 'k-separator-overline string))
    (let ((string (overlay-get vertico--count-ov 'before-string)))
      (add-face-text-property 0 (length string) 'k-separator-overline nil string))
    ;; (set-window-text-height vertico--buffer-window (+ 1 height))
    (fit-window-to-buffer vertico--buffer-window))

  ;; Customize vertico prompt
  (defun vertico--format-count ()
    "Format the count string."
    (concat
     (when (> (recursion-depth) 1)
       (propertize (format "%s " (recursion-depth)) 'face 'warning))
     (format "%-6s "
             (format #("%s/%s" 0 2 (face success))
                     (cond ((>= vertico--index 0) (1+ vertico--index))
                           (vertico--allow-prompt "*")
                           (t "!"))
                     vertico--total))))

  ;; Vertico insert echo messages into its input line.  Without any
  ;; patch, such echo message masks
  ;; `k-echo-area--top-separator-overlay', breaking our horizontal
  ;; rule drawn by overline.  The following resolves this.
  (defun k-minibuffer-message-advice (orig-func message &rest args)
    (when vertico--input
      (setq message (substring message))
      (add-face-text-property 0 (length message) 'k-separator-overline nil message))
    (apply orig-func message args))
  (advice-add 'minibuffer-message :around #'k-minibuffer-message-advice)

  ;; Make `vertico-buffer' use k-echo-area
  (cl-defmethod vertico--setup :after (&context (vertico-buffer-mode (eql t)))
    "Setup buffer display."
    (setq k-message nil)
    (add-hook 'pre-redisplay-functions 'vertico-buffer--redisplay nil 'local)
    (setq-local fringe-indicator-alist '((truncation nil nil)))
    (let* ((buf (current-buffer)) win
           (_ (unwind-protect
                  (setf
                   win (with-minibuffer-selected-window
                         ;; (k-window-echo-area-display buf)
                         (k-echo-area-display (selected-window) buf))
                   vertico--buffer-window win
                   main-win (selected-window))))
           (sym (make-symbol "vertico-buffer--destroy"))
           (depth (recursion-depth)))
      (fset sym (lambda ()
                  (k-echo-area-clear main-win)
                  (when (= depth (recursion-depth))
                    (with-selected-window (active-minibuffer-window)
                      (when vertico-buffer-hide-prompt
                        (set-window-vscroll nil 0))
                      (remove-hook 'minibuffer-exit-hook sym)))))
      ;; NOTE: We cannot use a buffer-local minibuffer-exit-hook here.
      ;; The hook will not be called when abnormally exiting the minibuffer
      ;; from another buffer via `keyboard-escape-quit'.
      (add-hook 'minibuffer-exit-hook sym)
      (overlay-put vertico--candidates-ov 'window win)
      (when (and vertico-buffer-hide-prompt vertico--count-ov)
        (overlay-put vertico--count-ov 'window win))
      (setq-local show-trailing-whitespace nil
                  header-line-format nil
                  tab-line-format nil
                  truncate-lines t
                  face-remapping-alist
                  (copy-tree `((mode-line-inactive mode-line)
                               ,@face-remapping-alist))))))

(use-package marginalia
  :hook emacs-startup
  :config
  ;; Automatically give more generous field width
  (setq-default marginalia-field-width 48)
  (defun marginalia--affixate (metadata annotator cands)
    "Affixate CANDS given METADATA and Marginalia ANNOTATOR."
    ;; reset `marginalia--cand-width-max'
    (let* ((width (cl-loop for win in (get-buffer-window-list) minimize (window-width win)))
           ;; estimate width
           (marginalia-field-width
            (max (- (floor (* width 0.8))
                    (let ((max (cl-loop for cand in cands
                                        maximize (string-width cand))))
                      (* (ceiling (or max 0) marginalia--cand-width-step) marginalia--cand-width-step)))
                 ;; minimum value for safety
                 2))
           (marginalia--metadata metadata))
      (setq-local marginalia--cand-width-max (default-value 'marginalia--cand-width-max))
      (marginalia--align
       (with-selected-window (or (minibuffer-selected-window) (selected-window))
         (cl-loop for cand in cands collect
                  ;; don't use `marginalia--cache' because we change width dynamically
                  (let ((ann (or (funcall annotator cand) "")))
                    (cons cand (if (string-blank-p ann) "" ann))))))))
  (byte-compile 'marginalia--affixate))

(use-package orderless
  :init
  (setq-default completion-styles ;; '(orderless)
                '(flex orderless))
  (setq-default completion-ignore-case t)
  (setq-default read-buffer-completion-ignore-case t)
  (setq-default read-file-name-completion-ignore-case t)
  (setq-default enable-recursive-minibuffers t)
  :config
  (setq-default orderless-matching-styles '(orderless-literal orderless-flex orderless-regexp))
  (define-key vertico-map (kbd "s-f") 'vertico-next-group)
  (define-key vertico-map (kbd "s-b") 'vertico-previous-group))

(use-package consult
  :init
  (setq-default consult-preview-key "<f2>")
  :bind
  (("s-h" . consult-imenu)
   ("s-;" . consult-goto-line)
   ("C-c C-SPC" . consult-mark)
   ("s-s" . consult-line)
   :map minibuffer-local-map
   ("C-s" . consult-history)
   ("C-r" . consult-history))
  :config
  (setq-default consult-grep-args
                '("zgrep" (consult--grep-exclude-args) "--null --line-buffered --color=never --ignore-case --line-number -I -r .")))

(use-package embark
  :demand t
  :bind
  (("C-z" . embark-act)
   :map embark-file-map
   ("g" . k-grep-in))
  :commands (k-grep-in-1)
  :init
  (setq-default prefix-help-command #'embark-prefix-help-command)
  :config
  (require 'embark)
  (setq embark-prompter #'embark-keymap-prompter)
  (setq embark-indicators
        '( embark--vertico-indicator embark-mixed-indicator
           embark-highlight-indicator embark-isearch-highlight-indicator))
  (defun k-grep-in-1 ()
    "Grep in current embark target."
    (interactive)
    (embark--act 'k-grep-in (car (embark--targets)) embark-quit-after-action))
  (defun k-grep-in (filename)
    "Grep in FILENAME."
    (if (file-directory-p filename)
        (consult-grep filename)
      (let ((buffer (find-file-noselect filename)))
        (with-current-buffer buffer
          (consult-line))))))

(use-package embark-consult)

;;; EXWM

(use-package exwm-randr
  :straight exwm
  :if k-exwm-enabled-p
  :demand t
  :config
  (exwm-randr-enable))

(use-package exwm
  :if k-exwm-enabled-p
  :demand t
  :config
  (setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "HDMI-1"))

  (defun k-exwm-update-title ()
    (exwm-workspace-rename-buffer (concat exwm-class-name ": " exwm-title)))
  (add-hook 'exwm-update-title-hook 'k-exwm-update-title)

  (start-process "picom" "*picom*" "picom")
  (exwm-enable))

;;; Misc key bindings

(define-key key-translation-map (kbd "<f1>") (kbd "C-h"))
(global-set-key "c" 'describe-char)
(global-set-key "a" 'describe-face)
(global-set-key "M" 'describe-keymap)
(global-set-key [f2] nil)

(global-set-key (kbd "s-w") 'save-buffer)
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-=") 'text-scale-adjust)
(global-set-key (kbd "s-+") 'text-scale-adjust)
(global-set-key (kbd "s--") 'text-scale-decrease)
;; (k-global-set-key (kbd "C-c C-c C-SPC") 'consult-global-mark)
(k-global-set-key (kbd "s-0") 'delete-window)
(k-global-set-key (kbd "s-1") 'delete-other-windows)
(k-global-set-key (kbd "s-2") 'split-window-below)
(k-global-set-key (kbd "s-3") 'split-window-right)
(k-global-set-key (kbd "s-=") 'balance-windows)
(k-global-set-key (kbd "s-k") 'bury-buffer)
(k-global-set-key (kbd "s-i") 'find-file)
(k-global-set-key (kbd "s-q") 'consult-buffer)

(global-set-key (kbd "s-SPC") 'fixup-whitespace)

(k-global-set-key (kbd "s-g") 'eww-new-buffer)
(k-global-set-key (kbd "s-a") 'k-emms)

(when k-exwm-enabled-p
  (setq exwm-input-global-keys
        `((,(kbd "s-<escape>") . exwm-reset)
          (,(kbd "s-r") . windmove-right)
          (,(kbd "s-l") . windmove-left)))
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
          ([?\C-k] . [S-end delete])
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])))
  (define-key exwm-mode-map (kbd "C-q") 'exwm-input-send-next-key)
  (define-key exwm-mode-map (kbd "C-c C-q") nil)
  (global-set-key (kbd "<XF86AudioRaiseVolume>") #'k-set-volume-increase)
  (global-set-key (kbd "<XF86AudioLowerVolume>") #'k-set-volume-decrease))


(define-key indent-rigidly-map (kbd "C-b") 'indent-rigidly-left)
(define-key indent-rigidly-map (kbd "C-f") 'indent-rigidly-right)
(define-key indent-rigidly-map (kbd "M-b") 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map (kbd "M-f") 'indent-rigidly-right-to-tab-stop)

;;;; More efficient bindings for keyboard macro
(use-package kmacro
  :bind (("C-x (" . kmacro-start-macro-or-insert-counter)
         ("C-x e" . kmacro-end-or-call-macro)
         ("C-x )" . nil)))

(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))

(use-package crux
  :bind (( [remap move-beginning-of-line] . crux-move-beginning-of-line)
         ( [remap kill-line] . crux-smart-kill-line))
  :config
  (crux-with-region-or-line kill-region)
  (crux-with-region-or-line kill-ring-save))

;;;; Disable input method (option+space) hot key on macOS

(defun k-ns-toggle-input-method-shortcut (enable)
  (call-process-shell-command
   (format "/usr/libexec/PlistBuddy -c \"Set :AppleSymbolicHotKeys:60:enabled %s\" ~/Library/Preferences/com.apple.symbolichotkeys.plist"
           (if enable "true" "false")))
  (call-process-shell-command
   "/System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u"))

(defun k-ns-focus-change-function ()
  (k-ns-toggle-input-method-shortcut (not (frame-focus-state))))

(add-function :after after-focus-change-function #'k-ns-focus-change-function)

;;; Lisp development

(use-package emacs
  ;; Disable C-c C-r binding to eval region, because we advice C-x C-e
  ;; to do that when region is active.
  :bind ( :map lisp-mode-shared-map
          ("C-c C-p" . eval-print-last-sexp)
          ("C-c C-l" . load-file)
          ("C-c C-r")
          :map emacs-lisp-mode-map
          ("C-c C-k" . emacs-lisp-byte-compile-and-load)
          ("C-c C-r")
          :map lisp-interaction-mode-map
          ("C-j")
          ("C-c C-r"))
  :hook (Info-selection . k-info-rename-buffer)
  :config
  (defun k-info-rename-buffer ()
    "Rename info buffer according to current node."
    (rename-buffer (format "info: %s" list-buffers-directory)))

  (define-advice eval-last-sexp (:around (orig-func &rest args) k)
    (if mark-active (call-interactively #'eval-region)
      (apply orig-func args)))

  (setq-default eval-expression-print-level nil
                eval-expression-print-length nil))

(use-package macrostep
  :bind ( :map emacs-lisp-mode-map
          ("C-c M-e" . macrostep-expand))
  :init
  ;; To fix the outdated naming in (define-minor-mode macrostep-mode ...)
  ;; TODO: Remove once upstream fix this.
  (defvaralias 'macrostep-mode-map 'macrostep-mode-keymap))

(use-package comment-or-uncomment-sexp
  :after paredit
  :bind ( :map paredit-mode-map
          ("M-;" . comment-or-uncomment-sexp)
          ("C-M-;" . structured-comment-defun))
  :config
  ;; #+nil structural comment for Common Lisp
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
    (if (derived-mode-p 'lisp-mode)
        (structured-comment-maybe 'sexp "#+nil ")
      (funcall orig-fun n)))
  (advice-add 'comment-or-uncomment-sexp :around 'structured-comment-advice)
  (defun structured-comment-defun ()
    "Use #+nil to comment a top-level form for Common Lisp."
    (interactive)
    (if (derived-mode-p 'lisp-mode)
        (structured-comment-maybe 'defun "#+nil
")
      (save-excursion
        (beginning-of-line)
        (if (eq (char-after) ?\;)
            (comment-or-uncomment-sexp)
          (beginning-of-defun)
          (comment-or-uncomment-sexp))))))

(use-package paredit
  :hook ( emacs-lisp-mode lisp-mode scheme-mode clojure-mode
          slime-repl-mode geiser-repl-mode cider-repl-mode
          slime-mrepl-mode)
  :bind ( :map paredit-mode-map
          ("C-j")
          ("RET")
          ("M-c" . paredit-convolute-sexp)))

(use-package paxedit
  :after paredit
  :hook paredit-mode
  :bind ( :map paxedit-mode-map
          ("C-w" . paxedit-kill-1)
          ("M-w" . paxedit-copy-1)
          ("M-j" . paxedit-compress))
  :config
  (defun paxedit-copy-1 (beg end)
    (interactive "r")
    (cond (mark-active
           (paredit-check-region beg end)
           (kill-ring-save beg end))
          (current-prefix-arg
           (paxedit-copy)
           (message "Sexp copied"))
          (t (paxedit-symbol-copy)
             (message "Symbol copied"))))
  (defun paxedit-kill-1 (beg end)
    (interactive "r")
    (cond (mark-active
           (paredit-check-region beg end)
           (kill-region beg end))
          (current-prefix-arg
           (paxedit-kill))
          (t (paxedit-symbol-kill)))))
;; (add-hook 'slime-repl-mode-hook #'k-pad-header-line-after-advice)

(font-lock-add-keywords 'lisp-mode '(("(\\(setf\\)" 1 font-lock-keyword-face)
                                     ("(\\(setq\\)" 1 font-lock-keyword-face)
                                     ("(\\(psetf\\)" 1 font-lock-keyword-face)
                                     ("(\\(psetq\\)" 1 font-lock-keyword-face)))
(font-lock-remove-keywords 'lisp-mode '(("#:\\(?:\\sw\\|\\s_\\|\\\\.\\)+" 0 font-lock-builtin-face)
                                        ("[`‘]\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)['’]" (1 font-lock-constant-face prepend))))
(font-lock-add-keywords 'lisp-mode '(("#:\\(?:\\sw\\|\\s_\\|\\\\.\\)+" 0 font-lock-constant-face)
                                     ("[`‘]\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)['’]" 1 font-lock-function-name-face prepend)))
(set-alist 'lisp-el-font-lock-keywords-2 "\\\\\\\\\\(?:\\[\\(?1:\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)]\\|`\\(?1:\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\(?: \\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)*\\)'\\)"
           '((1 font-lock-function-name-face prepend)))
(set-alist 'lisp-el-font-lock-keywords-2 "[`‘']\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)['’]"
           '((1 font-lock-function-name-face prepend)))

(add-to-list 'lisp-imenu-generic-expression
             (list "Section" "^;;;\\([^#].*\\)$" 1) t)
(define-advice eval-last-sexp (:around (orig-func &rest args) k)
  (if mark-active (call-interactively #'eval-region)
    (apply orig-func args)))

(use-package slime
  :bind ( ("s-o" . switch-to-scratch)
          :map slime-mode-map
          ("C-M-g" . slime-undefine)
          ("C-c C-s")
          :map slime-editing-map
          ("<f2> v" . slime-inspect)
          ("<f2> c" . slime-inspect-presentation-at-point)
          ("<f2> o" . slime-describe-symbol)
          ("<f2> i" . slime-documentation-lookup)
          ("C-c C-p" . slime-eval-print-last-expression))
  :autoload ensure-slime
  :hook (emacs-startup . ensure-slime)
  :config
  (let ((async-shell-command-buffer 'new-buffer))
    (system-packages-ensure "sbcl"))

  (define-advice slime-load-contribs
      (:after (&rest args) k)
    ;; (slime-load-file "~/.emacs.d/scripts.lisp")
    )
  (define-advice slime-eval-last-expression (:around (orig-func &rest args) k)
    (if mark-active (call-interactively #'slime-eval-region)
      (apply orig-func args)))
  (setq-default
   inferior-lisp-program "sbcl"
   slime-lisp-implementations
   `((sbcl ("sbcl" "--dynamic-space-size" "4096"))
     (mega-sbcl ("sbcl" "--dynamic-space-size" "24000" "--control-stack-size" "2"))
     (ccl ("ccl64"))))

  ;; Handy slime commands and key bindings
  (defun ensure-slime ()
    (slime-setup '( slime-company slime-fancy slime-quicklisp
                    slime-asdf slime-media slime-parse slime-mrepl))
    (unless slime-default-connection
      (slime)))

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

  ;; *slime-scratch*
  (defun switch-to-scratch ()
    "Switch to scratch buffer."
    (interactive)
    (if slime-editing-mode
        (slime-scratch)
      (switch-to-buffer-other-window "*scratch*")))

  ;; Slime mode line
  (defun slime-mode-line ()
    (concat (slime-connection-name) " "
            (propertize (downcase (string-trim (slime-current-package) "#?:\\|\"" "\""))
                        'face 'k-proper-name)))

  ;; Hacks to make slime-autodoc works better
  (setq eldoc-idle-delay 0)

  ;; Enable Paredit and Company in Lisp related minibuffers
  (defun k-slime-command-p (symbol)
    (let ((name (symbol-name symbol)))
      (or (string-prefix-p "sldb" name)
          (and (string-prefix-p "slime" name)
               (not (eq symbol 'slime-repl-quicklisp-quickload))
               (not (string-suffix-p "system" name))))))
  (byte-compile 'k-slime-command-p)

  (defun sexp-minibuffer-hook ()
    (when (and (symbolp this-command)
               (or (eq this-command 'eval-expression)
                   (k-slime-command-p this-command)))
      (paredit-mode)
      (company-mode)))
  (add-hook 'minibuffer-setup-hook 'sexp-minibuffer-hook)

  ;; Slime debug window non-prolifiration
  (set-alist 'display-buffer-alist "\\`*sldb" '((display-buffer-reuse-mode-window))))

(use-package slime-repl :straight slime
  :bind ( :map slime-mode-map
          ("s-x" . slime-repl-sync)
          :map slime-repl-mode-map
          ("M-r")
          ("M-s")
          ("DEL")
          ("C-c C-s" . consult-history)
          ("C-c C-r" . consult-history))
  :config
  (set-alist 'consult-mode-histories 'slime-repl-mode '(slime-repl-input-history))

  (defun slime-repl-sync ()
    "Switch to Slime REPL and synchronize package/directory."
    (interactive)
    (slime-sync-package-and-default-directory)
    (slime-repl)))

(use-package slime-company
  :config
  (setq-default slime-company-completion 'fuzzy)
  (add-to-list 'company-backends 'company-slime)

  (defun company-slime (command &optional arg &rest ignored)
    "Company mode backend for slime."
    (let ((candidate (and arg (substring-no-properties arg))))
      (cl-case command
        (init
         (slime-company-active-p))
        (prefix
         (when (and ;; OUR CHANGE
                (or (slime-company-active-p)
                    (k-slime-command-p current-minibuffer-command))
                (slime-connected-p)
                (or slime-company-complete-in-comments-and-strings
                    (null (slime-company--in-string-or-comment))))
           (company-grab-symbol)))
        (candidates
         (slime-company--fetch-candidates-async candidate))
        (meta
         (let ((*slime-company--meta-request* t))
           (slime-company--arglist candidate)))
        (annotation
         (concat (when slime-company-display-arglist
                   (slime-company--arglist-only candidate))
                 (when slime-company-display-flags
                   (concat " " (get-text-property 0 'flags arg)))))
        (doc-buffer
         (unless *slime-company--meta-request*
           (slime-company--doc-buffer candidate)))
        (quickhelp-string
         (unless *slime-company--meta-request*
           (slime-company--quickhelp-string candidate)))
        (location
         (slime-company--location candidate))
        (post-completion
         (slime-company--post-completion candidate))
        (sorted
         (eq slime-company-completion 'fuzzy))))))

(use-package slime-mrepl :straight slime)

;;; Version control

(use-package vc-hooks :straight (:type built-in)
  :config
  (setq-default vc-follow-symlinks t))

(use-package diff-mode
  :config
  ;; show whitespace in diff-mode
  (add-hook 'diff-mode-hook
            (lambda ()
              (setq-local whitespace-style
                          '( face tabs tab-mark spaces space-mark trailing
                             indentation::space indentation::tab
                             newline newline-mark))
              (whitespace-mode))))

(use-package magit
  :bind ( "s-m" . magit-status)
  :config
  (defun cloc-magit-root ()
    "Run Count Line Of Code for current Git repo."
    (interactive)
    (k-run-helper-command (concat "cloc " (magit-toplevel)) "*cloc*"))
  (add-hook 'magit-post-stage-hook 'k-generate-org-index--magit-post-stage-hook))

(use-package transient
  :config
  ;; Make `transient' and thus `magit' use k-echo-area
  (setq-default transient-display-buffer-action
                '((lambda (buffer alist)
                    (k-echo-area-display (selected-window) buffer))))
  (defun transient--delete-window ()
    (k-echo-area-clear-1 transient--window)))

(use-package smerge-mode
  :straight (:type built-in)
  :demand t
  :bind ( :map smerge-mode-map
          ("M-n" . smerge-next)
          ("M-p" . smerge-prev)
          ("C-c")
          ("C-c C-c" . smerge-keep-current))
  ;; ensure keymap precedence over flycheck
  :after flycheck)

;;;; autosave and backup files
(setq-default delete-old-versions t
              kept-new-versions 6
              kept-old-versions 2
              version-control t)
(add-hook 'emacs-startup-hook 'auto-save-visited-mode)

;;; Fast cursor movement

(use-package avy
  :init
  (defconst hyper-mask (- ?\H-a ?a))
  (defun hyper-ace ()
    (interactive)
    (avy-goto-word-1 (- last-command-event hyper-mask)))
  (dolist (x (number-sequence ?a ?z))
    (global-set-key (vector (+ hyper-mask x)) #'hyper-ace))
  (setq avy-keys (number-sequence ?a ?z))
  :config
  (defun my-avy--regex-candidates (fun regex &optional beg end pred group)
    (let ((regex (pyim-cregexp-build regex)))
      (funcall fun regex beg end pred group)))
  (advice-add 'avy--regex-candidates :around #'my-avy--regex-candidates)

  (put 'avy 'priority 10))

(use-package ace-link
  :demand t
  :config
  (defun ace-link--widget-action (pt)
    (when (number-or-marker-p pt)
      (goto-char pt)
      (let ((button (get-char-property pt 'button)))
        (when button
	  (widget-apply-action button)))))
  (defun ace-link--widget-collect ()
    "Collect the positions of visible widgets in current buffer."
    (let (candidates pt)
      (save-excursion
        (save-restriction
          (narrow-to-region
           (window-start)
           (window-end))
          (goto-char (point-min))
          (setq pt (point))
          (while (progn (widget-forward 1)
                        (> (point) pt))
            (setq pt (point))
            (push (point) candidates))))
      (nreverse candidates)))
  (defun ace-link-widget ()
    "Open or go to a visible widget."
    (interactive)
    (let ((pt (avy-with ace-link-widget
                (avy-process
                 (ace-link--widget-collect)
                 (avy--style-fn avy-style)))))
      (ace-link--widget-action pt)))
  (add-to-list 'avy-styles-alist
               '(ace-link-widget . pre))
  (ace-link-setup-default "o"))

(use-package goto-last-change
  :bind ("s-e" . goto-last-change))

;;; window/buffer/frame/workspaces movement

(use-package buffer-move
  :init
  ;; Intuitively, this works like windmove but move buffer together with cursor.
  (k-global-set-key (kbd "C-s-p") 'buf-move-up)
  (k-global-set-key (kbd "C-s-n") 'buf-move-down)
  (k-global-set-key (kbd "C-s-r") 'buf-move-right)
  (k-global-set-key (kbd "C-s-l") 'buf-move-left))

(use-package framemove
  :autoload fm-next-frame)

(use-package windmove
  :init
  (k-global-set-key (kbd "s-p") 'windmove-up)
  (k-global-set-key (kbd "s-n") 'windmove-down)
  (global-set-key (kbd "s-r") 'windmove-right)
  (global-set-key (kbd "s-l") 'windmove-left)
  :config
  ;; Moving between window/buffer/frame/workspaces in 4 directions
  (defun next-workspace (direction)
    (cl-case direction
      (left (exwm-workspace-switch (1- exwm-workspace-current-index)))
      (right (exwm-workspace-switch (1+ exwm-workspace-current-index)))))

  (define-advice windmove-find-other-window
      (:around (orig-func direction &rest args) k)
    "If there is an error, try framemove in that direction."
    (or (apply orig-func direction args)
        (progn
          (if k-exwm-enabled-p
              (next-workspace direction)
            (fm-next-frame direction))
          (selected-window)))))

(use-package winner
  :bind ( ("s-c" . winner-undo)
          ("s-C" . winner-redo))
  :hook emacs-startup)

;;; ⭐ Multi media

(defvar-local k-emms-playlist-file nil)

(use-package emms
  :commands k-emms
  :bind
  ( :map emms-playlist-mode-map
    ("p" . emms-pause)
    ("n")
    ("M-p" . emms-previous)
    ("M-n" . emms-next)
    ("s-f" . emms-playlist-mode-previous)
    ("s-b" . emms-playlist-mode-next))
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-mode-line-mode 0)
  (add-hook 'emms-playlist-mode-hook 'stripes-mode)
  (add-hook 'emms-playlist-mode-hook 'hl-line-mode)

  (setq emms-source-file-default-directory "~/.emacs.d/")
  (defvar k-emms-default-playlist "~/.emacs.d/rhythm-game.emms.el")
  (defun k-emms (file)
    "Switch to the current emms-playlist buffer, use
emms-playlist-mode and query for a playlist to open."
    (interactive (list k-emms-default-playlist))
    (if (or (null emms-playlist-buffer)
	    (not (buffer-live-p emms-playlist-buffer)))
        (emms-play-playlist file))
    (with-current-emms-playlist
      (setq-local k-emms-playlist-file file
                  write-contents-functions '(k-emms-save)))
    (emms-playlist-mode-go))
  (defun k-emms-save ()
    "Save emms playlist buffer."
    (interactive)
    (let ((emms-source-playlist-ask-before-overwrite nil))
      (emms-playlist-save 'native k-emms-playlist-file))
    t)

  ;; Eye candies
  (require 'emms-player-mpv)

  (setq emms-player-list '(emms-player-mpv))

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
  (add-to-list 'emms-player-mpv-parameters "--ytdl-format=bestaudio")
  (when (eq window-system 'ns)
    (add-to-list 'emms-player-mpv-parameters "--ontop-level=desktop")
    (add-to-list 'emms-player-mpv-parameters "--ontop")
    (add-to-list 'emms-player-mpv-parameters "--fullscreen")
    (add-to-list 'emms-player-mpv-parameters "--no-native-fs")
    (add-to-list 'emms-player-mpv-parameters "--no-focus-on-open"))
  (when k-exwm-enabled-p
    (add-to-list 'emms-player-mpv-parameters "--x11-name=mpv-background"))
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
    (emms-player-mpv-ipc-req-send '(get_property time-pos)
                                  #'(lambda (pos err)
                                      (unless err
                                        (emms-playing-time-set pos)
                                        (when k-blink-cursor-time-start
                                          (setq k-blink-cursor-time-start
                                                (time-add (current-time)
                                                          (seconds-to-time (- pos))))))))
    (force-mode-line-update))

  (defun k-emms-player-mpv-event-function (json-data)
    (pcase (alist-get 'event json-data)
      ("video-reconfig"
       (emms-player-mpv-event-playing-time-sync))))
  (add-to-list 'emms-player-mpv-event-functions 'k-emms-player-mpv-event-function)

  (require 'emms-info-tinytag)

  (defvar k-python3
    (cond ((executable-find "python") "python")
          ((executable-find "python3") "python3")
          (t (warn "Unable to guess python3 path."))))
  (setq emms-info-tinytag-python-name k-python3)
  (add-to-list 'emms-info-functions 'emms-info-tinytag)

  (defun k-emms-generate-theme ()
    (let ((url (emms-track-get (emms-playlist-current-selected-track) 'name)))
      (when (string-match "https://www.youtube.com/watch\\?v=\\(.*\\)" url)
        (slime-eval-async
            `(k/cl-user::get-color-url ,(concat "https://img.youtube.com/vi/" (match-string 1 url) "/default.jpg"))
          (lambda (colors)
            (if colors
                (let ((inhibit-message t))
                  (message "Generate theme: %s" colors)
                  (set-frame-parameter nil 'alpha 75)
                  (apply #'k-generate-theme
                         (append colors '(0.0 t))))
              (k-theme-switch 'dark)
              (set-frame-parameter nil 'alpha 100)))))))

  (defun k-emms-bpm-cursor ()
    (let ((bpm (emms-track-get (emms-playlist-current-selected-track) 'info-bpm)))
      (if bpm
          (progn
            (cancel-timer k-blink-cursor-timer)
            (setq k-blink-cursor-interval (/ 60.0 bpm)
                  k-blink-cursor-time-start (current-time))
            (setq k-blink-cursor-timer
                  (run-at-time k-blink-cursor-interval nil 'blink-cursor-timer-function)))
        (message "No BPM data for: %s" (emms-track-get (emms-playlist-current-selected-track) 'info-title))
        (setq k-blink-cursor-time-start nil
              k-blink-cursor-interval 0.5))))

  (defun k-emms-bpm-cursor-stop-hook ()
    (if (or emms-player-paused-p emms-player-stopped-p)
        (setq k-blink-cursor-time-start nil
              k-blink-cursor-interval 0.5)
      (k-emms-bpm-cursor)))

  ;; (add-hook 'emms-player-started-hook 'k-emms-generate-theme)
  ;; (add-hook 'emms-player-started-hook 'k-emms-bpm-cursor)
  ;; (add-hook 'emms-player-paused-hook 'k-emms-bpm-cursor-stop-hook)
  ;; (add-hook 'emms-player-stopped-hook 'k-emms-bpm-cursor-stop-hook)

  (defvar k-emms-player-mpv-volume 100)

  (defun k-emms-player-mpv-volume-change (amount)
    (let* ((amount (or amount 10))
           (new-volume (+ k-emms-player-mpv-volume amount)))
      (if (> new-volume 100)
          (emms-player-mpv-cmd '(set_property volume 100))
        (emms-player-mpv-cmd `(add volume ,amount))))
    (emms-player-mpv-cmd '(get_property volume)
                         #'(lambda (vol err)
                             (unless err
                               (let ((vol (truncate vol)))
                                 (setq k-emms-player-mpv-volume vol)
                                 (message "Music volume: %s%%"
                                          vol))))))

  (setq-default emms-volume-change-function 'k-emms-player-mpv-volume-change))

(use-package emms-playlist-mode
  :straight emms
  :config
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
                         'priority 1)))))))

(use-package exwm
  :if k-exwm-enabled-p
  :config
  (defun k-exwm-update-class ()
    "Custom window management.
Put mpv windows in the background as dynamic wallpapers.
Hide davmail windows on startup."
    (pcase exwm-instance-name
      ("mpv-background"
       (setq exwm-window-type (list xcb:Atom:_NET_WM_WINDOW_TYPE_DESKTOP))
       (with-slots (x y width height) (exwm-workspace--get-geometry exwm--frame)
         (exwm--set-geometry exwm--id x y width height)))
      ("davmail-DavGateway"
       (exwm-layout--hide exwm--id))))
  (add-hook 'exwm-update-class-hook 'k-exwm-update-class))

(use-package ytel
  :bind ( :map ytel-mode-map
          ("RET" . ytel-play)
          ("p" . (lambda () (interactive) (ytel-play t)))
          ("a" . ytel-add))
  :config
  (setq-default ytel-invidious-api-url "https://vid.puffyan.us"
                ytel-title-video-reserved-space 40
                ytel-author-name-reserved-space 20)
  ;; Custom video entry formatting
  (defun ytel--insert-video (video)
    "Insert `VIDEO' in the current buffer."
    (with-slots (published author length title views) video
      (if length
          (insert (ytel--format-video-published published)
	          " "
	          (truncate-string-to-width author 20 nil ?\  (truncate-string-ellipsis))
	          " "
	          (ytel--format-video-length length)
	          "  "
	          (truncate-string-to-width title 40 nil ?\  (truncate-string-ellipsis))
	          " "
	          (k-fill-right (format "%s views" views)))
        (insert "--"))))
  (byte-compile 'ytel--insert-video)
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

  (add-hook 'ytel-mode-hook 'stripes-mode)
  (add-hook 'ytel-mode-hook
            '(lambda ()
               (setq-local hl-line-face '(:inherit match :extend t))
               (hl-line-mode)))
  (advice-add 'ytel--draw-buffer :after #'k-pad-header-line-after-advice))

;;; ⭐ Blink cursor
;; It can synchronize to BPM which EMMS is playing!
;; This works together with `k-emms-bpm-cursor'. It also
;; uses absolute timing, otherwise Emacs timer will drift.

(blink-cursor-mode -1)
(defvar blink-cursor-colors (list "#000"))
(defvar blink-background-colors nil)
(defvar k-blink-cursor-time-start nil)
(defvar k-blink-cursor-interval 0.5)
(defvar k-blink-cursor-flash-interval 0.1)
(defvar k-blink-cursor-timer (run-at-time k-blink-cursor-interval nil 'blink-cursor-timer-function))
(setq blink-cursor-count 0)

(defun blink-cursor-timer-function ()
  (if (internal-show-cursor-p)
      (progn
        (when (>= blink-cursor-count (length blink-cursor-colors))
          (setq blink-cursor-count 0))
        (let ((color (nth blink-cursor-count blink-cursor-colors))
              (bg (nth blink-cursor-count blink-background-colors)))
          (set-cursor-color color)
          (setq blink-cursor-count (+ 1 blink-cursor-count))
          (internal-show-cursor nil nil))
        (setq k-blink-cursor-timer
              (run-at-time
               (if k-blink-cursor-time-start
                   (- k-blink-cursor-interval
                      (mod (float-time (time-since k-blink-cursor-time-start)) k-blink-cursor-interval))
                 k-blink-cursor-flash-interval)
               nil 'blink-cursor-timer-function)))
    (internal-show-cursor nil t)
    (setq k-blink-cursor-timer (run-at-time (- k-blink-cursor-interval k-blink-cursor-flash-interval) nil 'blink-cursor-timer-function))))

(defun k-rhythm-hit-result ()
  (when k-blink-cursor-time-start
    (let* ((time (float-time (time-since k-blink-cursor-time-start)))
           (err (abs (- time (* k-blink-cursor-interval (round time k-blink-cursor-interval)))))
           (inhibit-message t))
      (cond ((< err 0.06) (message "Perfect"))
            ((< err 0.12) (message "Ok"))
            ((< err 0.18) (message "Meh"))
            (t (message "Miss"))))))

;; (add-hook 'pre-command-hook 'k-rhythm-hit-result)

;;; Scheme

(use-package scheme)

(use-package geiser
  :bind ( :map scheme-mode-map
          ("M-l" . geiser-load-current-buffer)
          ("C-h h" . geiser-doc-look-up-manual))
  :config
  (defun geiser-mode-maybe ()
    (unless (eq major-mode 'scheme-interaction-mode)
      (geiser-mode)))
  (add-hook 'scheme-mode-hook 'geiser-mode-maybe)
  (setq geiser-mode-start-repl-p t))

(use-package geiser-racket)

(use-package racket-mode
  :bind ( :map racket-mode-map
          ("M-l" . (lambda ()
                     (interactive)
                     (geiser-load-file (buffer-file-name (current-buffer)))
                     (switch-to-geiser-module (geiser-eval--get-module) (current-buffer)))))
  :config
  (add-hook 'racket-mode-hook 'geiser-mode))

;; Terminal (vterm)

(k-use-guix-maybe vterm)

(use-package vterm
  :straight nil
  :bind ( :map vterm-mode-map
          ("C-c C-t" . nil)
          ("C-c C-j" . vterm-copy-mode)
          ("C-c M-o" . vterm-clear)
          ("C-q" . vterm-send-next-key)
          ("C-d" . (lambda () (interactive) (vterm-send-key "d" nil nil t)))
          :map vterm-copy-mode-map
          ("C-c C-k" . (lambda () (interactive) (vterm-copy-mode -1))))
  :config
  ;; Ad-hoc workaround: interaction with wide fringe/padding
  (defun vterm--get-margin-width () 1)

  (setq vterm-max-scrollback 1000000))

;; (use-package multi-vterm
;;   :after vterm
;;   :bind
;;   ( ("s-x" . multi-vterm-next)
;;     ("s-X" . multi-vterm)
;;     :map vterm-mode-map
;;     ("s-x" . multi-vterm)
;;     ("s-f" . multi-vterm-next)
;;     ("s-b" . multi-vterm-prev)
;;     :map vterm-copy-mode-map
;;     ("s-x" . multi-vterm)
;;     ("s-f" . multi-vterm-next)
;;     ("s-b" . multi-vterm-prev)))

;; (use-package unix-in-slime
;;   :straight (:local-repo "~/quicklisp/local-projects/unix-in-lisp")
;;   :bind
;;   ( ("s-x" . unix-in-slime-next)
;;     ("s-X" . unix-in-slime)))

;; Web browsing

(setq-default browse-url-browser-function 'eww-browse-url)

(use-package exwm
  :if k-exwm-enabled-p
  :config
  (setq-default browse-url-secondary-browser-function 'k-browse-url-chromium)
  (defun k-browse-url-chromium (url &rest args)
    (start-process "chromium" " *chromium*" "chromium"
                   (concat "--app=" url))))

(defvar-local k-eww-title nil)

(use-package eww
  :commands eww-new-buffer
  :config
  (setq-default browse-url-browser-function 'eww-browse-url
                eww-search-prefix "https://www.google.com/search?q=")
  (add-hook 'eww-after-render-hook 'k-pad-header-line-after-advice)
  (defvar k-eww-history (make-hash-table :test 'equal)
    "Global history for eww. A EQUAL hash that maps title strings to URL.")
  (defun k-eww-after-render-hook ()
    "Save `k-eww-history'."
    (let ((title (plist-get eww-data :title))
          (url (plist-get eww-data :url)))
      (puthash (concat (truncate-string-to-width title 40 nil nil (truncate-string-ellipsis))
                       #(" " 0 1 (display (space :align-to center)))
                       (propertize url 'face 'completions-annotations))
               url k-eww-history)))
  (add-hook 'eww-after-render-hook 'k-eww-after-render-hook)

  ;; Move page title from header line to buffer name instead
  (setq-default eww-header-line-format "%u")
  (define-advice eww-update-header-line-format
      (:after () k)
    "Update EWW buffer title."
    (setq k-eww-title
          (if (zerop (length (plist-get eww-data :title)))
	      "[untitled]"
            (plist-get eww-data :title)))
    (setq-local k-mode-line-format-left
                '(#("eww: " 0 4 (face mode-line-buffer-id))
                  (:propertize k-eww-title face variable-pitch)))
    (rename-buffer (format "eww: %s" k-eww-title) t))

  (defun k-eww-read-url ()
    "Read URL with global history completion from `k-eww-history'.
If inside a Google Search buffer, use the search keyword as
default input."
    (let* ((title (plist-get eww-data :title))
           (suffix " - Google Search")
           (cand
            (completing-read "Enter URL or keywords: " k-eww-history
                             nil nil
                             (when (string-suffix-p suffix title)
                               (string-remove-suffix suffix title)))))
      (or (gethash cand k-eww-history) cand)))

  (defun eww-new-buffer (url)
    (interactive (list (k-eww-read-url)))
    (with-temp-buffer
      (if current-prefix-arg
          (let ((eww-search-prefix "https://scholar.google.com/scholar?q="))
            (eww url))
        (eww url))))
  (define-key eww-mode-map (kbd "G") 'eww-new-buffer)

  (require 'gv)

  (define-advice url-http
      (:before (url &rest _args) k-reddit)
    "Redirect to old.reddit.com"
    (when (string-equal (url-host url) "www.reddit.com")
      (setf (url-host url) "old.reddit.com"))))

(use-package exwm
  :if k-exwm-enabled-p
  :after eww
  :config
  (defun k-eww-reload-in-chromium ()
    (interactive)
    (k-browse-url-chromium (plist-get eww-data :url)))
  (define-key eww-mode-map (kbd "f") 'k-eww-reload-in-chromium))

(k-use-guix-maybe pdf-tools)

(use-package pdf-tools
  :demand t
  :straight nil
  :hook (after-init . pdf-loader-install)
  :config
  (setq pdf-view-midnight-invert nil))

(use-package image-mode
  :straight (:type built-in)
  :bind ( :map image-mode-map
          ("+" . image-increase-size)
          ("-" . image-decrease-size)
          ("r" . image-rotate))
  :config
  ;; Disable transient map because we've already put those bindings
  ;; into the main `image-mode-map'
  (setq-default image--repeat-map (make-sparse-keymap)))

;; (when (featurep 'xwidget-internal)
;;   (add-to-list 'load-path "~/.emacs.d/lisp/xwwp")
;;   (require 'xwwp-full)
;;   (define-key xwidget-webkit-mode-map (kbd "o") 'xwwp-ace-toggle)
;;   (define-key xwidget-webkit-mode-map (kbd "s-h") 'xwwp-section)
;;   (setq-default xwwp-ace-label-style
;;                 `(("z-index" . "2147483647")
;;                   ("color" . ,k-dk-blue)
;;                   ("font-family" . "monospace")
;;                   ("background-color" . ,"rgba(255,255,255,0.5)")
;;                   ("font-size" . "1.5em")
;;                   ("padding" . "0.1em")
;;                   ("border-width" . "0.1em")
;;                   ("border-style" . "solid")))
;;   (define-key xwidget-webkit-mode-map (kbd "l") 'xwidget-webkit-back)
;;   (define-key xwidget-webkit-mode-map (kbd "r") 'xwidget-webkit-forward)
;;   (define-key xwidget-webkit-mode-map (kbd "g") 'xwidget-webkit-reload))

;;; ⭐ System utils

(defun k-screenshot ()
  "Save a screenshot and copy its path."
  (interactive)
  (let ((path (concat (getenv "HOME") "/Documents/Screenshot-" (format-time-string "%Y-%m-%d,%H:%M:%S") ".png")))
    (cond ((executable-find "screencapture")
           (call-process "screencapture" nil nil nil path))
          ((executable-find "import")
           (call-process "import" nil nil nil "-window" "root" path))
          (t (error "idk how")))
    (kill-new path)
    (message (concat "Screenshot saved to " path))))

(defun k-get-volume ()
  "Get volume."
  (cond ((executable-find "amixer")
         (let ((output (shell-command-to-string "amixer get Master")))
           (string-match "\\[\\([0-9]+\\)%\\]" output)
           (string-to-number (match-string 1 output))))
        ((executable-find "osascript")
         (string-to-number
          (shell-command-to-string
           "osascript -e 'output volume of (get volume settings)'")))))

(defun k-set-volume (volume)
  "Change volume."
  (interactive (list (read-from-minibuffer
                      (format "Change volume (current %s%%): "
                              (k-get-volume))
                      nil nil t)))
  (cl-check-type volume number)
  (if (= 0 (call-process-shell-command
                (cond ((executable-find "amixer")
                       (format "amixer set Master %s%%" volume))
                      ((executable-find "osascript")
                       (format "osascript -e 'set volume output volume %s'" volume)))))
      (message "Master Volume: %s%%" volume)
    (error "Failed to set volume")))

(defun k-set-volume-decrease ()
  (interactive)
  (k-set-volume (max 0 (- (k-get-volume) 2))))

(defun k-set-volume-increase ()
  (interactive)
  (k-set-volume (min 100 (+ (k-get-volume) 2))))

(use-package sudo-edit
  :init
  (add-hook 'find-file-hook
            (lambda ()
              (setq default-directory
                    (replace-regexp-in-string "^/sudo:root@localhost:" "" default-directory)))))

(use-package system-packages)

(use-package insecure-lock
  :config
  (defun insecure-lock-hide ()
    (if insecure-lock-mode
        (progn
          (set-frame-parameter nil 'insecure-lock--saved-alpha (frame-parameter nil 'alpha))
          (set-frame-parameter nil 'alpha 0.0))
      (set-frame-parameter nil 'alpha (frame-parameter nil 'insecure-lock--saved-alpha))))
  (setq insecure-lock-mode-hook '(insecure-lock-hide insecure-lock-blank-screen insecure-lock-posframe)))

;;; Org

(use-package org
  :config
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

  (setq org-preview-latex-default-process 'dvisvgm)
  (plist-put org-format-latex-options :scale 2.0)
  (setq-default org-html-with-latex 'dvisvgm)
  (setq-default org-link-descriptive t
                org-hide-emphasis-markers t
                org-src-fontify-natively t)

  (org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))
  (org-babel-do-load-languages 'org-babel-load-languages '((lisp . t)))
  (setq org-babel-lisp-eval-fn 'slime-eval)
  (setq org-imenu-depth 4)

  (require 'org-inlinetask)

  (defun k-org-mode-hook ()
    (visual-line-mode)
    (org-variable-pitch-minor-mode)
    ;; (org-appear-mode)
    (org-superstar-mode)
    (org-indent-mode))
  (add-hook 'org-mode-hook #'k-org-mode-hook))

(use-package org-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(use-package org-variable-pitch)

(use-package org-superstar
  :config
  (setq-default org-superstar-item-bullet-alist
                '((?* . ?•)
                  (?+ . ?➤)
                  (?- . ?•))
                org-superstar-headline-bullets-list
                '(;; Original ones nicked from org-bullets
                  ?◉
                  ?○
                  ?▷)))

(use-package poly-org
  :config
  (define-polymode poly-org-mode
    :hostmode 'poly-org-hostmode
    :innermodes '(poly-org-innermode poly-org-latex-innermode)
    :switch-buffer-functions
    (list (lambda (old new)
            (let ((font-lock-mode nil))
              (pm--move-minor-modes '(org-indent-mode) old new))))
    (setq-local org-src-fontify-natively nil)
    (setq-local polymode-run-these-before-change-functions-in-other-buffers
                (append '(org-before-change-function
                          org-element--cache-before-change
                          org-table-remove-rectangle-highlight)
                        polymode-run-these-before-change-functions-in-other-buffers))
    (setq-local polymode-run-these-after-change-functions-in-other-buffers
                (append '(org-element--cache-after-change)
                        polymode-run-these-after-change-functions-in-other-buffers))))

(defun k-polymode-init-inner-hook ()
  (oset pm/chunkmode adjust-face 'org-block)
  (topsy-mode -1))
(add-hook 'polymode-init-inner-hook 'k-polymode-init-inner-hook)

(use-package engrave-faces
  :config
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
  (engrave-faces-use-theme 'default))

;;; ERC

(use-package erc
  :config
  (advice-add 'erc-update-mode-line-buffer :after 'k-pad-header-line-after-advice)
  (setq-default erc-track-enable-keybindings nil
                erc-prompt-for-password t
                erc-header-line-format
                (concat (propertize "%n" 'face 'erc-nick-default-face)
                        " on " (propertize "%t" 'face 'mode-line-buffer-id)
                        " (%m,%l) "
                        (propertize "%o" 'face 'variable-pitch)))
  (setq erc-server "localhost"
        erc-port 6670)
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (advice-add 'erc-login :before
              (lambda ()
                (erc-server-send "CAP REQ :znc.in/self-message")
                (erc-server-send "CAP END")))
  (setq-default erc-fill-function 'ignore)
  (add-hook 'erc-mode-hook 'visual-line-mode)

  (require 'erc-stamp)
  (defun erc-insert-timestamp-right (string)
    (unless (and erc-timestamp-only-if-changed-flag
	         (string-equal string erc-timestamp-last-inserted))
      (setq erc-timestamp-last-inserted string)
      (goto-char (point-max))
      (forward-char -1)                 ; before the last newline
      (let* ((from (point)))
        (k-insert-fill-right string)
        (erc-put-text-property from (point) 'field 'erc-timestamp)
        (erc-put-text-property from (point) 'rear-nonsticky t)
        (when erc-timestamp-intangible
	  (erc-put-text-property from (1+ (point)) 'cursor-intangible t))))))


;;; Email

(let ((async-shell-command-buffer 'new-buffer))
  (system-packages-ensure "notmuch")
  (unless (executable-find "mbsync")
    (system-packages-install "isync")))

(defvar k-email-accounts
  '(( "Stanford"
      host "localhost"
      user "qthong@stanford.edu"
      port 1143
      timeout 0
      AuthMechs login
      pass 123 ;; davmail doesn't care
      ssltype none)))
(defvar k-maildirs-prefix "~/maildirs/")
(defun insert-plist (plist)
  (cl-loop for (k v) on plist by #'cddr
           do (insert (symbol-name k) " " (prin1-to-string v) "\n"))
  (insert "\n"))
(with-temp-buffer
  (dolist (account k-email-accounts)
    (let* ((name (car account))
           (remote (concat name "-remote"))
           (local (concat name "-local")))
      (ignore-errors (mkdir k-maildirs-prefix))
      (ignore-errors (mkdir (concat k-maildirs-prefix name)))
      (insert-plist `( IMAPAccount ,name
                       ,@ (cdr account)))
      (insert-plist `( IMAPStore ,remote
                       account ,name))
      (insert-plist `( MaildirStore ,local
                       path ,(concat k-maildirs-prefix name "/")
                       inbox ,(concat k-maildirs-prefix name "/INBOX")
                       subfolders verbatim))
      (insert-plist `( channel ,name
                       far ,(concat ":" remote ":")
                       near ,(concat ":" local ":")
                       patterns *
                       expunge none
                       CopyArrivalDate yes
                       sync all
                       create near
                       SyncState *))))
  (write-file "~/.mbsyncrc"))

(require 'gnus-util)
(defun k-format-relative-date (messy-date)
  (let* (;;If we don't find something suitable we'll use this one
         (my-format "%b %d '%y"))
    (let* ((difference (time-subtract nil messy-date))
	   (templist '(((gnus-seconds-today) . "%H:%M")
                       (604800 . "%a %H:%M")
                       ((gnus-seconds-year) . "%b %d")
                       (t . "%Y %b %d")))
	   (top (eval (caar templist) t)))
      (while (if (numberp top) (time-less-p top difference) (not top))
        (progn
	  (setq templist (cdr templist))
	  (setq top (eval (caar templist) t))))
      (if (stringp (cdr (car templist)))
	  (setq my-format (cdr (car templist)))))
    (format-time-string (eval my-format t) messy-date)))

(use-package message :straight gnus
  :config
  (setq-default message-signature "Best,\nQiantan"
                message-fill-column nil
                user-full-name "Qiantan Hong"
                user-mail-address "qthong@stanford.edu"))

(use-package notmuch
  :bind ( :map notmuch-common-keymap
          ("G" . k-update-notmuch)
          :map notmuch-hello-mode-map
          ("o" . ace-link-widget))
  :hook (emacs-startup . k-ensure-davmail)
  :config
  (setq-default notmuch-search-oldest-first nil
                notmuch-show-logo nil
                notmuch-tag-formats
                '(("unread" nil)
                  ("inbox" nil)
                  ("replied" (all-the-icons-faicon "reply"))
                  ("attachment" (all-the-icons-faicon "paperclip"))
                  ("flagged" (all-the-icons-faicon "flag")))
                notmuch-message-headers
                '("Subject"
                  ;; "Date" ;; relative date already displayed in summary line
                  "To"
                  "Cc")
                notmuch-wash-citation-regexp  "\\(^[[:space:]]*>.*\n\\)+\\|\\(^____*\n\\(.*\n\\)*\\)"
                notmuch-wash-citation-lines-prefix 2
                notmuch-wash-citation-lines-suffix 0
                notmuch-fcc-dirs nil)

  ;; Custom email entry formatting
  (defun notmuch-search-show-result (result pos)
    "Insert RESULT at POS."
    ;; Ignore excluded matches
    (unless (= (plist-get result :matched) 0)
      (save-excursion
        (goto-char pos)
        (let ((right
               (concat
                (let ((tags (plist-get result :tags))
	              (orig-tags (plist-get result :orig-tags)))
                  (notmuch-tag-format-tags tags orig-tags))
                (propertize (format " %s/%s" (plist-get result :matched)
			            (plist-get result :total))
		            'face 'notmuch-search-count))))
          (insert
           (propertize (k-format-relative-date (plist-get result :timestamp))
		       'face 'notmuch-search-date)
           #(" " 0 1 (display (space :align-to 10)))
           (propertize (truncate-string-to-width
                        (notmuch-sanitize (plist-get result :authors))
                        20 0 nil (truncate-string-ellipsis))
                       'face 'notmuch-search-matching-authors)
           #(" " 0 1 (display (space :align-to 28)))
           (propertize (truncate-string-to-width
                        (notmuch-sanitize (plist-get result :subject))
                        (- (window-text-width) (length right) 25)
                        0 nil (truncate-string-ellipsis))
		       'face 'notmuch-search-subject)
           (propertize " " 'display `(space :align-to (- text ,(length right) 1)))
           right "\n"))
        (notmuch-search-color-line pos (point) (plist-get result :tags))
        (put-text-property pos (point) 'notmuch-search-result result))))
  (advice-add 'notmuch-show--build-buffer :after #'k-pad-header-line-after-advice)

  (defun k-ensure-davmail ()
    "Make sure davmail is running."
    (unless
        (cl-find-if (lambda (p)
                      (let ((case-fold-search t))
                        (string-match-p "davmail" (or (cdr (assoc 'args (process-attributes p))) ""))))
                    (list-system-processes))
      (start-process "davmail" "*davmail*" "davmail")))

  (defvar k-update-notmuch-timer nil)
  (defvar k-update-notmuch-timer-interval 120)
  (defvar k-notmuch-unread-count 0)
  (defun k-update-notmuch-unread-count ()
    (setq k-notmuch-unread-count (string-to-number (notmuch-saved-search-count "tag:unread"))))
  (defun k-update-notmuch (&optional silent)
    "Update email database asynchronously."
    (interactive)
    (k-ensure-davmail)
    (if (process-live-p (get-buffer-process (get-buffer "*notmuch-update*")))
        (unless silent
          (display-buffer "*notmuch-update*" '(nil (inhibit-same-window . t))))
      (k-run-helper-command "mbsync -a; notmuch new; exit" "*notmuch-update*"
                            #'notmuch-refresh-all-buffers silent)
      (timer-set-time k-update-notmuch-timer
                      (timer-relative-time nil k-update-notmuch-timer-interval)
                      k-update-notmuch-timer-interval)
      (k-update-notmuch-unread-count)))
  (add-hook 'notmuch-after-tag-hook 'k-update-notmuch-unread-count)
  (when k-update-notmuch-timer
    (cancel-timer k-update-notmuch-timer))
  (setq k-update-notmuch-timer
        (run-at-time k-update-notmuch-timer-interval
                     k-update-notmuch-timer-interval
                     'k-update-notmuch t))

  (defun k-notmuch-unread-status ()
    (if (> k-notmuch-unread-count 0)
        (concat (number-to-string k-notmuch-unread-count) " "
                (all-the-icons-octicon "mail" :v-adjust 0.0 :height 0.8))
      ""))
  (add-to-list 'k-status-functions 'k-notmuch-unread-status))

(use-package smtpmail
  :init
  (setq-default user-mail-address "qthong@stanford.edu"
                send-mail-function 'smtpmail-send-it)
  :config
  (setq-default smtpmail-smtp-server "localhost"
                smtpmail-smtp-service 1025
                smtpmail-stream-type 'plain
                smtpmail-smtp-user "qthong@stanford.edu"))

;;; Input Method

(use-package pyim
  :autoload pyim-cregexp-build
  :init (setq-default default-input-method 'pyim)
  :config
  (setq-default pyim-punctuation-translate-p '(auto no yes))
  (defun k-pyim-probe ()
    (or buffer-read-only
        (get-text-property (point) 'read-only)))
  (setq-default pyim-indicator-list '(pyim-indicator-with-modeline)
                pyim-english-input-switch-functions
                '(k-pyim-probe pyim-probe-program-mode pyim-probe-isearch-mode))
  (pyim-basedict-enable)
  (pyim-greatdict-enable))

(use-package pyim-basedict)

(use-package pyim-greatdict
  :straight
  (pyim-greatdict :type git :host github :repo "tumashu/pyim-greatdict"
                  :files ("*.pyim.gz" :defaults)))

;;; ⭐ Misc handy commands

(defvar lookup-word-buffer nil)

(defun lookup-word (word)
  (interactive (list (thing-at-point 'word t)))
  (select-window (display-buffer-below-selected
                  (or (and (buffer-live-p lookup-word-buffer) lookup-word-buffer)
                      (current-buffer))
                  nil))
  (setq lookup-word-buffer (browse-url (format "https://en.wiktionary.org/wiki/%s#Latin" word))))

(defun demolish-package (symbol)
  "Nuke everything under namespace SYMBOL.
This is useful when maintaining a long running Emacs image and
you want to try reloading/updating a package."
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
         (setf (symbol-plist symbol) nil))))))

;; https://gist.github.com/jdtsmith/1fbcacfe677d74bbe510aec80ac0050c
(defun k-reraise-error (func &rest args)
  "Call function FUNC with ARGS and re-raise any error which occurs.
Useful for debugging post-command hooks and filter functions, which
normally have their errors suppressed."
  (condition-case err
      (apply func args)
    ((debug error) (signal (car err) (cdr err)))))

(defun toggle-debug-on-hidden-errors (func)
  "Toggle hidden error debugging for function FUNC."
  (interactive "a")
  (cond
   ((advice-member-p #'k-reraise-error func)
    (advice-remove func #'k-reraise-error)
    (message "Debug on hidden errors disabled for %s" func))
   (t
    (advice-add func :around #'k-reraise-error)
    (message "Debug on hidden errors enabled for %s" func))))

(defun k-straight-freeze-versions ()
  "Run `straight-freeze-versions' asynchronously in Emacs subprocess."
  (interactive)
  (when-let ((buf (find-buffer-visiting "~/.emacs.d/init.el")))
    (with-current-buffer buf
      (when (buffer-modified-p buf)
        (save-buffer))))
  (k-run-helper-command "emacs --script '~/.emacs.d/init.el' --eval='(straight-freeze-versions)'"
                        "*straight-freeze-versions*"))

;;; ⭐ Status area

;; A status area at the right bottom corner (using the right side of
;; global echo area).  It is used for displaying battery, time, and
;; vampire time zone.

(defvar k-status-functions '(k-time-status))

(defun k-time-status ()
  "Status function for current time."
  (format-time-string "%-m/%d %-I:%M %p"))

(defun k-battery-status ()
  "Status function for battery status."
  (let ((output (shell-command-to-string "acpi --battery"))
        (case-fold-search nil))
    (string-match " \\([0-9]+\\)%" output)
    (let* ((percent (string-to-number (match-string 1 output)))
           (charging (string-match-p "Charging" output))
           (quarter (/ percent 25))
           (all-the-icons-default-faicon-adjust 0.2)
           (all-the-icons-default-alltheicon-adjust 0.2))
      (concat
       (cond (charging (all-the-icons-alltheicon "battery-charging"))
             ((> quarter 3) (all-the-icons-faicon "battery-full"))
             ((> quarter 2) (all-the-icons-faicon "battery-three-quarters"))
             ((> quarter 1) (all-the-icons-faicon "battery-half"))
             ((> quarter 0) (all-the-icons-faicon "battery-quarter"))
             (t (all-the-icons-faicon "battery-empty" :face 'error)))
       " "
       (match-string 1 output) "%"))))

(when (executable-find "acpi")
  (add-to-list 'k-status-functions 'k-battery-status))

(defun k-telega-status ()
  (when (and (featurep 'telega) (telega-chat-buffers))
    (let* ((c (telega-chat-get 373230619))
           (n (+ (plist-get c :unread_count)
                 (plist-get c :unread_mention_count)
                 (plist-get c :unread_reaction_count))))
      (if (> n 0) (format "%s ♡" n) nil))))

(add-to-list 'k-status-functions 'k-telega-status)

(defun k-status-update ()
  "Update status area."
  (let* ((msg (mapcar #'funcall k-status-functions))
         (msg (mapconcat #'identity (cl-remove nil msg) "  "))
         (width (string-width msg))
         (msg (k-fill-right msg)))
    (with-current-buffer " *Echo Area 0*"
      (remove-overlays (point-min) (point-max))
      (overlay-put (make-overlay (point-min) (point-max) nil nil t)
                   'after-string msg))
    (with-current-buffer " *Minibuf-0*"
      (delete-region (point-min) (point-max))
      (insert msg))
    ;; (when-let (buffer (get-buffer " *Vampire Time Screensaver*"))
    ;;   (with-current-buffer buffer
    ;;     (delete-region (point-min) (point-max))
    ;;     (insert msg))
    ;;   (posframe-show buffer :poshandler 'posframe-poshandler-frame-center
    ;;                  :border-width 1))
    ))

(add-hook 'post-command-hook 'k-status-update)
(defvar k-status-timer (run-at-time t 1 'k-status-update))

;;; Vampire timezone
;; How much sun-protection-free time left?

(require 'solar)
;
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

(defun vampire-time-status ()
  "Status function for vampire time zone."
  (let ((time (time-to-vampire-time)))
    (concat (format-seconds "%h:%.2m:%.2s" (cadr time))
            " "
            (let ((all-the-icons-default-faicon-adjust 0.25))
              (if (eq (car time) 'sunrise)
                  (all-the-icons-faicon "moon-o")
                (all-the-icons-faicon "sun-o"))))))

(add-to-list 'k-status-functions 'vampire-time-status)

;; (defun vampire-time-screensaver ()
;;   (if insecure-lock-mode
;;       (progn
;;         (get-buffer-create " *Vampire Time Screensaver*")
;;         (k-status-update))
;;     (posframe-delete " *Vampire Time Screensaver*")))

;;; telega.el
;; A heavily modified telega.el to tweak its appearance to my liking.

(k-use-guix-maybe telega)

(defvar k-telega-extra-xheight 10)

(use-package telega
  :straight nil
  :bind ( :map telega-chat-mode-map
          ("<f2>")
          ("C-c C-e" . k-telega-chatbuf-attach-sticker)
          :map telega-root-mode-map
          ("<f2>"))
  :config
  (defun k-telega-chatbuf-attach-sticker (all)
    (interactive "P")
    (telega-chatbuf-attach-sticker (not all)))
  (set-keymap-parent (k-ensure-prefix-map telega-chat-mode-map "C-c")
                     telega-prefix-map)
  (set-keymap-parent (k-ensure-prefix-map telega-root-mode-map "C-c")
                     telega-prefix-map)
  (setq-default
   telega-filters-custom
   '(("Main" not (folder "Politics"))
     ("Important" . important)
     ("Online" and
      (not saved-messages)
      (user is-online))
     ("lng_filters_type_groups" type basicgroup supergroup)
     ("lng_filters_type_channels" type channel)
     ("lng_filters_type_no_archived" . archive))
   telega-online-status-function 'telega-buffer-p
   telega-sticker-size '(8 . 24)
   telega-chat-input-markups '("org" nil)
   telega-symbol-checkmark "●"
   telega-symbol-heavy-checkmark "✓"
   telega-chat-fill-column 80
   telega-emoji-use-images nil
   telega-symbol-eye (propertize "🔎" 'face '(shadow k-monochrome-emoji))
   telega-symbol-pin (propertize "📌" 'face '(success k-monochrome-emoji))
   telega-symbol-pending (all-the-icons-material "sync")
   telega-symbol-forward (propertize (compose-chars ?🗩 ?🠒) 'face '(shadow k-monochrome-emoji))
   telega-symbol-video-chat-passive (all-the-icons-material "videocam" :face 'shadow)
   telega-symbol-video-chat-active (all-the-icons-material "videocam" :face 'success))

  (define-advice  telega-chars-xheight
      (:around (orig n &optional face) k)
    (+ (funcall orig n face) (* n k-telega-extra-xheight)))
  (require 'cl)

  (define-advice telega-sticker--create-image
      (:around (orig &rest args) k)
    (with-advice
     (image-type-available-p
      :around (orig type)
      (and (not (eq type 'webp))
           (funcall orig type)))
     (apply orig args)))

  (define-advice telega--fmt-text-faces (:around (orig fmt-text &optional for-msg) k)
    (let ((text (funcall orig fmt-text for-msg)))
      (when for-msg
        (add-face-text-property 0 (length text) 'variable-pitch t text))
      text))

  (define-advice telega-ins--special (:around (orig &rest args) k)
    (with-advice
     (telega-symbol
      :around (orig ending)
      (if (eq ending 'horizontal-bar) " " (funcall orig ending)))
     (apply orig args)))

  (define-advice telega-ins--message0 (:around (orig &rest args) k)
    (with-advice
     (telega-fmt-eval-align
      :around (orig estr attrs)
      (funcall orig estr (plist-put attrs :align-symbol nil)))
     (apply orig args)))

  (define-advice telega-ins--date (:around (orig timestamp))
    (telega-ins (k-format-relative-date timestamp)))

  (defun k-telega-load-all-history ()
    "Load all history in current chat."
    (interactive)
    (with-advice
     (telega--getChatHistory :around
         (orig chat from-msg-id offset
               &optional limit only-local callback)
         (funcall orig chat from-msg-id offset
                  limit t callback))
     (when (telega-chatbuf--need-older-history-p)
       (telega-chatbuf--load-older-history
        (lambda (_count)
          (sit-for 0 t)
          (if (telega-chatbuf--need-older-history-p)
              (k-telega-load-all-history)
            ( telega-chatbuf--history-state-set :older-loaded nil)))))))
  ;; (define-advice telega-chatbuf--load-initial-history
  ;;     (:around (orig) k)
  ;;   (with-advice
  ;;    (telega-chat--load-history :around
  ;;        (orig chat &optional from-msg-id offset limit
  ;;              callback)
  ;;        (funcall orig chat from-msg-id offset limit
  ;;                 (lambda (history)
  ;;                   (funcall callback history)
  ;;                   (k-telega-load-all-history))))
  ;;    (funcall orig)))
  (setq-default telega-chat-history-limit 100)
  (add-hook 'telega-root-mode-hook
            '(lambda ()
               (setq-local hl-line-face 'match)
               (hl-line-mode)))

  (telega-autoplay-mode))

(use-package enwc
  :if k-exwm-enabled-p
  :config
  (setq-default enwc-default-backend 'nm))

(use-package proced
  :config
  (setq-default proced-auto-update-interval 1
                proced-format-alist
                '((short pid comm tree pcpu vsize start time user)
                  (medium pid comm tree pcpu vsize rss pmem state  ttname start time pid user args)
                  (long pid comm tree pri nice pcpu vsize rss pmem ttname state
                        start time pid user euid group args)
                  (verbose pid pgrp comm tree sess pri nice pcpu vsize rss pmem
                           state thcount  ttname tpgid minflt majflt cminflt cmajflt
                           start time utime stime ctime cutime cstime etime pid ppid user euid group egid args)))
  (add-hook 'proced-mode-hook (lambda () (proced-toggle-auto-update 1))))

;;; Undo Tree

(use-package undo-tree
  :disabled t
  :bind ( ("s-z" . undo-tree-visualize)
          :map undo-tree-visualizer-mode-map
          ("M-n" . undo-tree-visualize-redo-to-x)
          ("M-p" . undo-tree-visualize-undo-to-x))
  :hook (emacs-startup . global-undo-tree-mode)
  :config
  (setq-default undo-limit 1000000
                undo-strong-limit 10000000
                undo-outer-limit 100000000
                undo-tree-enable-undo-in-region t
                undo-tree-auto-save-history nil ;; Too fucking slow!
                undo-tree-visualizer-timestamps t))

(use-package vundo
  :bind ( ("s-z" . undo)
          ([remap undo] . vundo))
  :config
  (setq-default vundo-glyph-alist vundo-unicode-symbols
                vundo-window-max-height 10)
  (setq-default undo-limit 1000000
                undo-strong-limit 10000000
                undo-outer-limit 100000000)
  ;; Let vundo use k-echo-area
  (defun vundo ()
    "Display visual undo for the current buffer."
    (interactive)
    (when (not (consp buffer-undo-list))
      (user-error "There is no undo history"))
    (when buffer-read-only
      (user-error "Buffer is read-only"))
    (run-hooks 'vundo-pre-enter-hook)
    (let ((vundo-buf (vundo-1 (current-buffer))))
      (select-window
       (k-echo-area-display (selected-window) vundo-buf))
      (let ((window-min-height 4))
        (fit-window-to-buffer nil vundo-window-max-height))
      (goto-char
       (vundo-m-point
        (vundo--current-node vundo--prev-mod-list)))
      (setq vundo--roll-back-to-this
            (vundo--current-node vundo--prev-mod-list))))

  (defun k-vundo-kill-buffer-advice (orig-func &rest args)
    "Let vundo use `k-echo-area-clear-1' instead of `kill-buffer-and-window'.
The latter would also delete the main window because it is atomic
with k-echo-area."
    (with-advice
     (kill-buffer-and-window
      :override ()
      (k-echo-area-clear-1 (get-buffer-window)))
     (apply orig-func args)))
  (advice-add 'vundo-quit :around 'k-vundo-kill-buffer-advice)
  (advice-add 'vundo-confirm :around 'k-vundo-kill-buffer-advice)

  ;; `jit-lock-mode' need to be passed nil to turn off
  (define-derived-mode vundo-mode special-mode
    "Vundo" "Mode for displaying the undo tree."
    (setq mode-line-format nil
          truncate-lines t
          cursor-type nil)
    ;; OUR CHANGE: bug fix
    (jit-lock-mode nil)
    ;; OUR CHANGE: also turn of line truncation symbol
    (setq-local fringe-indicator-alist '((truncation nil nil)))
    (face-remap-add-relative 'default 'vundo-default)

    ;; Disable evil-mode, as normal-mode
    ;; key bindings override the ones set by vundo.
    (when (and (boundp 'evil-emacs-state-modes)
               (not (memq 'vundo-mode evil-emacs-state-modes)))
      (push 'vundo-mode evil-emacs-state-modes))))

(use-package undo-fu-session
  :hook (emacs-startup . 'undo-fu-session-global-mode))

;;; ⭐ Org index generation

(use-package toc-org)

(defun k-generate-org-index (output-buffer source-filename)
  "Read Emacs Lisp from current buffer and write org index to OUTPUT-BUFFER.
SOURCE-FILENAME is used for generate relative link with line numbers.
Processing starts from the point in current buffer and write to the point
in OUTPUT-BUFFER. Both points are advanced during processing."
  (let ((input-buffer (current-buffer)))
    (cl-flet ((output (&rest args)
                (with-current-buffer output-buffer
                  (apply #'insert args)))
              (link ()
                "Return a relative link with current line number in the input buffer."
                (with-current-buffer input-buffer
                  (save-restriction
                    (widen)
                    (format "file:%s#L%s" source-filename (line-number-at-pos))))))
      (cl-macrolet ((with-output (&rest body)
                      `(let ((from (point)))
                         ,@body
                         (output (buffer-substring-no-properties from (point))))))
        (while (< (point) (point-max))
          (let (sexp-type)
            (cond ;; skip ";;; Commentary:" etc
             ((looking-at-p ";;; [[:alpha:]]*:$\\|;;; .* ends .*$")
              (forward-line))
             ;; Org subsection for ";;;; ..."
             ((looking-at ";;;; ")
              (goto-char (match-end 0))
              (cl-fresh-line output-buffer)
              (output "** [[" (link) "][")
              (with-output
               (end-of-line))
              (output "]]\n"))
             ;; Org section for ";;; ..."
             ((looking-at ";;; ")
              (goto-char (match-end 0))
              (cl-fresh-line output-buffer)
              (output "* [[" (link) "][")
              (with-output
               (end-of-line))
              (output "]]\n"))
             ;; Skip comment block starting with ";; (".  Useful for
             ;; skipping commented out Lisp form.  Will also skip
             ;; any following text commemt without empty line.
             ((looking-at ";; (")
              (while (looking-at ";;")
                (forward-comment 1)
                (skip-chars-forward "[:space:]")))
             ;; ";;" comment gets output verbatim
             ((looking-at ";;")
              (goto-char (match-end 0))
              (with-output
               (end-of-line)))
             ;; For `use-package' form, we invoke
             ;; `k-generate-org-index' recursively on the body.  If
             ;; it generates any output, we make a Org subsection.
             ;; Otherwise, we set `sexp-type' and let top-level
             ;; definition (later in code) processing facility
             ;; handle it instead.
             ((looking-at "(use-package ")
              (goto-char (match-end 0))
              (let ((name (symbol-name (symbol-at-point)))
                    (from (line-end-position))
                    (link (link)))
                (up-list)
                (backward-char 1)
                (setq sexp-type "Package")
                (when (> (point) from)
                  (let ((output-from (with-current-buffer output-buffer (point))))
                    (with-current-buffer output-buffer
                      (save-restriction
                        (narrow-to-region (point-max) (point-max))
                        (with-current-buffer input-buffer
                          (save-excursion
                            (save-restriction
                              (narrow-to-region from (point))
                              (goto-char (point-min))
                              (k-generate-org-index output-buffer source-filename))))))
                    (when (> (with-current-buffer output-buffer (point-max)) output-from)
                      (with-current-buffer output-buffer
                        (save-excursion
                          (goto-char output-from)
                          (cl-fresh-line output-buffer)
                          (insert "** Package [[" link "][" name "]]")
                          (insert "\n")
                          (setq sexp-type nil))))))
                (if sexp-type
                    (beginning-of-defun)
                  (forward-line))))
             ;; Recognize top-level definitions.
             ((looking-at-p "(defun\\|(defsubst\\|(cl-defmethod")
              (setq sexp-type "Function"))
             ((looking-at-p "(define-advice")
              (setq sexp-type "Advice"))
             ((looking-at-p "(\\(cl-\\)?defmacro")
              (setq sexp-type "Macro"))
             ;; Skip other sexps.
             (t (forward-sexp)
                (end-of-line)))
            ;; Top-level-definition processing.
            (when sexp-type
              (let ((link (link)))
                (save-excursion
                  (down-list)
                  (forward-sexp)
                  (forward-sexp)
                  (let ((name (symbol-at-point))
                        (doc (ignore-errors
                               (forward-sexp)
                               (skip-chars-forward "[:space:]\n")
                               (when (looking-at-p "\"")
                                 (let ((from (1+ (point))))
                                   (forward-sexp)
                                   (buffer-substring-no-properties from (1- (point))))))))
                    ;; Add indentation
                    (when doc
                      (setq doc (s-join "\n    " (s-lines doc))))
                    ;; Remove trailing whitespace first. We'll build
                    ;; it later.
                    (with-current-buffer output-buffer
                      (skip-chars-backward "[:space:]\n")
                      (delete-char (- (point-max) (point))))
                    (if doc
                        ;; If `doc' is non-nil, we always make a new
                        ;; list item on its own line
                        (progn
                          (cl-fresh-line output-buffer)
                          (output "  - " sexp-type " [[" link "][" (symbol-name name) "]]: " doc "\n"))
                      ;; If `doc' is nil, see if we have an item with
                      ;; the same `sexp-type' immediately preceding
                      ;; us. If so, amalgamate with the preceding list
                      ;; item.
                      (with-current-buffer output-buffer
                        (if (save-excursion
                              (forward-line 0)
                              (and (looking-at-p (concat "  - " sexp-type))
                                   (progn
                                     (end-of-line)
                                     (not (looking-back "\\." 1)))))
                            (progn
                              (insert ", "))
                          (cl-fresh-line output-buffer)
                          (insert "  - " sexp-type " "))
                        (insert "[[" link "][" (symbol-name name) "]]" "\n"))))))
              (forward-sexp)
              (end-of-line)))
          ;; If we have empty lines in the source file, also ensure
          ;; we have at least one empty line in the output
          (let ((linum (line-number-at-pos)))
            (skip-chars-forward "[:space:]\n")
            (when (and (> (- (line-number-at-pos) linum) 1)
                       (with-current-buffer output-buffer (not (looking-back "\n\n" 2))))
              (cl-fresh-line output-buffer)
              (output "\n"))))))))

(defun k-generate-org-index-init ()
  "Generate README.org from init.el."
  (interactive)
  (with-current-buffer (find-file-noselect "~/.emacs.d/init.el")
    (save-excursion
      (goto-char (point-min))
      (forward-line) ;; skip first line
      (let ((output-buffer (find-file-noselect "~/.emacs.d/README.org")))
        (with-current-buffer output-buffer
          (goto-char (point-min))
          (delete-char (- (point-max) (point-min)))
          (insert "#+TITLE: kchan's Emacs config

This org file is an index automatically generated from init.el.
Links in the file are clickable on GitHub and bring you to the
source code.  The more non-trivial parts of my config are marked
with ⭐, which I think some people may find interesting. Have
fun!

* Table of Contents :TOC:
"))
        (k-generate-org-index output-buffer "init.el")
        (with-current-buffer output-buffer
          (toc-org-mode)
          (save-buffer))))))

(defun k-generate-org-index--magit-post-stage-hook ()
  (when (equal (magit-toplevel) (file-truename "~/.emacs.d/"))
    (let ((staged (magit-staged-files)))
      (when (and (member "init.el" staged)
                 (not (member "README.org"  staged)))
        (k-generate-org-index-init)
        (magit-stage-file "README.org")))))

;;; Finale

;; load up the theme
(k-theme-switch 'dark)

;; perform GC
(setq gc-cons-threshold 8000000 gc-cons-percentage 0.25)

(provide 'init)
;;; init.el ends here
