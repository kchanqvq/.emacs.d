;;; -*- lexical-binding: t -*-

(deftheme k)

;; Tweek fonts to  match `window-text-pixel-size'...

(defvar k-light-monospace "Source Code Pro-20:weight=light")
(defvar k-serif-monospace "Libertinus Mono-19")
(defvar k-courier-height 200)
(set-frame-font k-light-monospace nil t)

(defvar k-color-style 'bright)
(pcase
 k-color-style
 ('bright
  (defconst k-bg-blue "#bde0fe")
  (defconst k-fg-blue "#75BFFF")
  (defconst k-dk-blue "#0088FF")
  (defconst k-bg-purple "#C7B5E8")
  (defconst k-fg-purple "#B48CFF")
  (defconst k-dk-purple "#8B4DFF")
  (defconst k-bg-pink "#FFD4E9")
  (defconst k-fg-pink "#FF8AC2")
  (defconst k-dk-pink "#FF4FA4")
  (defconst k-bg-grey-1 "grey95")
  (defconst k-bg-grey-2 "grey90")
  (defconst k-bg-grey-3 "grey80")

  (defconst k-bg "#ffffff")
  (defconst k-bg-1 k-bg-blue)

  ;; workaround a highlight-indent-guide bug: values of FF break bitmap function
  (defconst k-bg-2 "#ECF6FE")

  (defconst k-bg-con k-bg-pink)
  (defconst k-bg-con-1 k-bg-purple)
  (defconst k-fg "#000000")
  (defconst k-fg-1 "#7070a0")
  (defconst k-fg-con k-fg-pink)
  (defconst k-fg-err "#FF703B")

  (defconst blink-cursor-colors (list k-fg-blue k-fg-pink k-fg-purple))
  (defconst blink-highlight-colors (list k-bg-blue k-bg-pink k-bg-purple)))

 ('dark
  (defconst k-bg-blue "#1e2d57")
  (defconst k-fg-blue "#0088FF")
  (defconst k-dk-blue "#75BFFF")
  (defconst k-bg-purple "#C7B5E8")
  (defconst k-fg-purple "#55efc4")
  (defconst k-dk-purple "#55efc4")
  (defconst k-bg-pink "#fd79a8")
  (defconst k-fg-pink "#FF8AC2")
  (defconst k-dk-pink "#FF8AC2")
  (defconst k-bg-grey-1 "grey20")
  (defconst k-bg-grey-2 "grey30")
  (defconst k-bg-grey-3 "grey40")

  (defconst k-bg "#273763")
  (defconst k-bg-1 "#4a587d")
  (defconst k-bg-2 k-bg-blue)
  (defconst k-bg-con  "#435ca3")
  (defconst k-bg-con-1 k-bg-purple)
  (defconst k-fg "#ffffff")
  (defconst k-fg-1 "#2ec4ff")
  (defconst k-fg-con k-fg-pink)
  (defconst k-fg-err "#fdcb6e")
  (defconst blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65"))
  (defconst blink-highlight-colors (list "#5D7E79" "#475E94" "#733780" "#808164"))))

(defface k-quote nil "Base face for quote.")
(defface k-keyword nil "Base face for keyword.")
(defface k-proper-name nil "Base face for proper name.")
(defface k-doc nil "Base face for doc.")
(defface k-string nil "Base face for strings.")
(defface k-comment nil "Base face for comment.")
(defface k-common nil "Base face for common match substring.")
(defface k-prompt nil "Base face for prompts.")
(defface k-zebra nil "Base face for zebra stripes.")
(defface k-timestamp nil "Base face for timestamps.")
(defface emms-mode-line-title nil "Face for EMMS track title in mode line.")

(custom-theme-set-faces
 'k
 `(k-quote ((default :family "Courier" :height ,k-courier-height)))
 `(k-keyword ((default :foreground ,k-fg-1 :inherit bold)))
 `(k-proper-name ((default :inherit (k-quote bold) :foreground ,k-fg)))
 `(k-string ((default :foreground ,k-dk-pink)))
 `(k-doc ((default :font ,k-serif-monospace :inherit k-string :weight normal)))
 `(k-comment ((default  :inherit italic :foreground ,k-fg-1)))
 `(k-common ((default :inherit bold)))
 `(k-prompt ((default :inherit bold :foreground ,k-fg-pink)))
 `(k-zebra ((default :background ,k-bg-blue :extend t)))
 `(default ((default :background ,k-bg :foreground ,k-fg :weight light)))
 '(bold ((default :weight normal)))
 '(bold-italic ((default :slant italic :weight normal)))
 '(underline ((default :underline t)))
 '(italic ((default :slant italic)))
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
 `(font-lock-reference-face ((default :inherit k-proper-name)))
 `(font-lock-constant-face ((default :inherit k-proper-name)))
 ;; `(font-lock-preprocessor-face ((,class (:foreground ,success))))
 `(font-lock-regexp-grouping-backslash ((default :inherit bold)))
 `(font-lock-regexp-grouping-construct ((default :inherit bold)))

 `(font-lock-type-face ((default :inherit k-keyword)))
 `(font-lock-warning-face ((default :inherit warning)))
 `(error ((default :foreground ,k-fg-err :inherit bold)))
 `(shadow ((default :foreground ,k-fg-1)))
 `(success ((default :foreground ,k-dk-blue :inherit bold)))
 `(warning ((default :foreground ,k-fg-err :inherit bold)))

 ;; Flycheck
 ;; `(flycheck-error ((,class (:underline (:style wave :color ,error)))))
 ;; `(flycheck-info ((,class (:underline (:style wave :color ,warning)))))
 ;; `(flycheck-warning ((,class (:underline (:style wave :color ,warning)))))
 ;; `(flycheck-fringe-error ((,class (:foreground ,error))))
 ;; `(flycheck-fringe-info ((,class (:foreground ,warning))))
 ;; `(flycheck-fringe-warning ((,class (:foreground ,warning))))

 ;; Flymake
 ;; `(flymake-warnline ((,class (:underline (:style wave :color ,warning) :background ,background))))
 ;; `(flymake-errline ((,class (:underline (:style wave :color ,error) :background ,background))))

 ;; Flyspell
 ;; `(flyspell-incorrect ((,class (:underline (:style wave :color ,error)))))

 ;; Search
 `(match ((default :background ,k-bg-con)))
 `(lazy-highlight ((default :inherit region)))
 `(isearch ((default :inherit match)))
 `(isearch-fail ((default :inherit error)))

 ;; Avy
 `(avy-lead-face ((default :background ,k-dk-blue :foreground ,k-bg
                           :font "Source Code Pro:weight=normal")))
 `(avy-lead-face-0 ((default :inherit avy-lead-face)))
 `(avy-lead-face-1 ((default :inherit avy-lead-face)))
 `(avy-lead-face-2 ((default :inherit avy-lead-face)))

 ;; Emacs interface
 `(cursor ((default (:background ,k-fg-pink))))
 `(fringe ((default :foreground ,k-fg-1)))
 `(vertical-border ((default :foreground ,k-bg)))
 `(window-divider-first-pixel ((default :foreground ,k-bg)))
 `(border ((default :inherit fringe)))
 `(border-glyph (nil))
 `(highlight ((default :inherit region)))
 ;; `(gui-element ((,class (:background ,contrast-bg))))
 `(internal-border ((default (:background ,k-bg-blue))))
 `(child-frame-border ((default (:background ,k-bg-blue))))
 `(mode-line ((default :background ,k-bg-blue)))
 `(mode-line-buffer-id ((default :inherit (mode-line bold))))
 `(mode-line-inactive ((default :inherit mode-line)))
 `(mode-line-emphasis ((default :foreground ,k-dk-purple :inherit (mode-line bold))))
 `(mode-line-highlight ((default :foreground ,k-dk-blue :inherit (mode-line bold))))
 `(minibuffer-prompt ((default :background ,k-bg :weight normal)))
 `(region ((default :background ,k-bg-1)))
 `(secondary-selection ((default :background ,k-bg-1)))
 `(header-line ((default :background ,k-bg :underline ,k-fg)))

 `(button ((default :underline t :foreground ,k-fg :inherit bold)))
 `(link ((default :foreground ,k-fg-1 :inherit button)))
 `(widget-button ((default :foreground ,k-fg :background ,k-bg-blue :box (:line-width 1))))
 `(widget-field ((default :foreground ,k-fg :background ,k-bg-1 :box (:line-width 1))))

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
 `(show-paren-match ((default :background ,k-bg-con)))
 `(show-paren-mismatch ((default :background ,k-bg-con)))

 ;; `(sh-heerroroc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
 ;; `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
 ;; `(mmm-declaration-submode-face ((,class (:inherit font-lock-doc-face :background ,(blend-color success background 0.2) :extend t))))
 ;; `(slime-highlight-edits-face ((,class (:weight bold))))
 `(slime-repl-input-face ((default :weight normal)))
 `(slime-repl-prompt-face ((default :inherit k-prompt)))
 `(slime-repl-output-face ((default :background ,k-bg-2 :extend t)))
 `(slime-repl-result-face ((default :inherit k-comment)))
 `(slime-repl-inputed-output-face ((default :foreground ,k-dk-pink :slant normal)))
 `(slime-repl-output-mouseover-face ((default :inherit (match slime-repl-inputed-output-face))))

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
 ;; `(macrostep-expansion-highlight-face ((,class (:background ,brighter-bg :foreground nil))))

 ;; undo-tree
 `(undo-tree-visualizer-default-face ((default :inherit shadow)))
 `(undo-tree-visualizer-current-face ((default :foreground ,k-dk-pink :inherit bold)))
 `(undo-tree-visualizer-active-branch-face ((default :foreground ,k-dk-blue :inherit bold)))
 ;; `(undo-tree-visualizer-register-face ((,class (:foreground ,highlight))))

 ;; Magit

 ;; `(magit-diff-added ((default :background ,"#ddffdd")))
 ;; `(magit-diff-added-highlight ((default :background ,"#cceecc")))
 ;; `(magit-diff-removed ((default :background ,"#ffe9e9")))
 ;; `(magit-diff-removed-highlight ((default :background ,"#ffdddd")))
 `(magit-diff-added ((default :background ,k-bg-blue :foreground ,k-fg-1)))
 `(magit-diff-added-highlight ((default :background ,k-bg-blue)))
 `(magit-diff-removed ((default :background ,k-bg-pink :foreground ,k-fg-1)))
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
 `(vertico-current ((default :inherit match)))
 `(vertico-group-title ((default :background ,k-bg-purple :inherit bold)))
 `(vertico-group-separator ((default :background ,k-bg-purple :strike-through t :inherit shadow)))
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
 `(company-tooltip-scrollbar-thumb ((default :background ,k-bg-con-1)))
 `(company-tooltip-scrollbar-track ((default :background ,k-bg-con)))
 ;; `(company-preview-common ((default :foreground ,k-fg-err)))
 ;; `(company-preview-search ((default :foreground ,k-fg-err)))
 `(company-tooltip-common ((default :inherit k-common)))
 `(company-tooltip-common-selection ((default :inherit k-common)))
 `(company-tooltip-search ((default :inherit k-common)))
 `(company-tooltip-search-selection ((default :inherit k-common)))
 ;; `(company-echo-common ((,class (:inherit company-echo :foreground ,function))))

 ;; LaTeX
 `(font-latex-math-face ((default :inherit shadow)))
 `(font-latex-sedate-face ((default :inherit bold)))
 `(font-latex-warning-face ((default :inherit warning)))
 `(preview-face ((default :background ,k-bg-grey-1 :extend t)))

 ;; `(org-agenda-structure ((,class (:foreground ,success))))
 ;; `(org-agenda-date ((,class (:foreground ,keyword :underline nil))))
 ;; `(org-agenda-done ((,class (:foreground ,string))))
 ;; `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
 ;; `(org-block ((,class (:foreground ,warning))))
 `(org-list-dt ((default :weight normal)))
 ;; `(org-code ((,class (:foreground ,highlight))))
 ;; `(org-column ((,class (:background ,contrast-bg))))
 ;; `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
 `(org-date ((default :foreground ,k-dk-pink :inherit k-quote)))
 ;; `(org-document-info ((,class (:foreground ,warning))))
 ;; `(org-document-info-keyword ((,class (:foreground ,string))))
 ;; `(org-document-title ((,class (:weight bold :foreground ,warning :height 1.44))))
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
 ;; `(org-todo ((,class (:foreground ,error))))
 ;; `(org-upcoming-deadline ((,class (:foreground ,warning))))
 ;; `(org-warning ((,class (:weight bold :foreground ,error))))

 ;; `(markdown-url-face ((,class (:inherit link))))
 ;; `(markdown-link-face ((,class (:foreground ,keyword :underline t))))

 `(hl-line ((default :inherit region)))
 `(stripes ((default :background ,k-bg-grey-1)))
 `(highlight-indent-guides-character-face ((default :foreground ,k-bg-2)))
 `(highlight-indent-guides-top-character-face ((default :foreground ,k-bg-1)))
 `(highlight-indent-guides-odd-face ((default :inherit highlight-indent-guides-character-face)))
 `(highlight-indent-guides-even-face ((default :inherit highlight-indent-guides-character-face)))

 ;; Message-mode
 ;; `(message-header-other ((,class (:foreground nil :background nil :weight normal))))
 ;; `(message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,highlight))))
 ;; `(message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,warning))))
 ;; `(message-header-cc ((,class (:inherit message-header-to :foreground nil))))
 ;; `(message-header-name ((,class (:foreground ,keyword :background nil))))
 ;; `(message-header-newsgroups ((,class (:foreground ,warning :background nil :slant normal))))
 ;; `(message-separator ((,class (:foreground ,success))))

 ;; Outline
 `(outline-1  ((default :height 1.5 :inherit bold)))
 `(outline-2 ((default :height 1.3 :inherit bold)))
 `(outline-3 ((default :height 1.12 :inherit bold)))
 `(outline-4 ((default :height 1.0 :inherit bold)))
 `(outline-5 ((default :height 1.0 :inherit bold)))
 `(outline-6 ((default :height 1.0 :inherit bold)))
 `(outline-7 ((default :height 1.0 :inherit bold)))
 `(outline-8 ((default :height 1.0 :inherit bold)))
 `(outline-9 ((default :height 1.0 :inherit bold)))

 ;; EMMS
 ;; `(emms-browser-artist-face ((,class (:inherit outline-2))))
 ;; `(emms-browser-album-face ((,class (:inherit outline-3))))
 ;; `(emms-browser-track-face ((,class (:inherit outline-4))))
 ;; `(emms-browser-year/genre-face ((,class (:inherit outline-1))))
 `(emms-playlist-selected-face ((default :inherit match :extend t)))
 `(emms-playlist-track-face ((default :foreground ,k-fg)))
 `(emms-mode-line-title ((default :inherit italic)))

 ;; ytel
 `(ytel-video-published-face ((default :inherit org-date)))
 `(ytel-channel-name-face ((default :inherit k-proper-name)))
 `(ytel-video-view-face ((default :inherit shadow)))
 `(ytel-video-length-face ((default :inherit shadow)))
 ;; erc
 ;; `(erc-direct-msg-face ((,class (:foreground ,warning))))
 ;; `(erc-error-face ((,class (:foreground ,error))))
 ;; `(erc-header-face ((,class (:foreground ,foreground :background ,darker-bg))))
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

 ;; GNUS
 `(gnus-group-mail-1 ((default :inherit bold)))
 `(gnus-group-mail-1-empty ((default :foreground ,k-dk-purple)))
 `(gnus-group-mail-2 ((default :inherit bold)))
 `(gnus-group-mail-2-empty ((default :foreground ,k-dk-blue)))
 `(gnus-group-mail-3 ((default :inherit bold)))
 `(gnus-group-mail-3-empty ((default :foreground ,k-fg)))
 `(gnus-summary-normal-ancient ((default :inherit gnus-summary-normal-read)))
 `(gnus-summary-normal-read ((default :foreground ,k-fg)))
 `(gnus-summary-normal-unread ((default :foreground ,k-fg)))
 `(gnus-summary-normal-ticked ((default :inherit region)))
 `(gnus-summary-selected ((default :inherit match :extend t)))
 `(gnus-header-name ((default :inherit k-keyword)))
 `(gnus-header-from ((default :inherit (shadow k-proper-name))))
 `(gnus-header-content ((default :inherit default)))
 `(gnus-header-subject ((default :inherit (variable-pitch bold))))
 `(gnus-button ((default :inherit button)))

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

 ;; ansi-term
 ;; `(term ((,class (:foreground nil :background nil :inherit default))))
 ;; `(term-color-black   ((,class (:foreground ,foreground :background ,foreground))))
 ;; `(term-color-error     ((,class (:foreground ,error :background ,error))))
 ;; `(term-color-green   ((,class (:foreground ,string :background ,string))))
 ;; `(term-color-warning  ((,class (:foreground ,highlight :background ,highlight))))
 ;; `(term-color-blue    ((,class (:foreground ,keyword :background ,keyword))))
 ;; `(term-color-magenta ((,class (:foreground ,success :background ,success))))
 ;; `(term-color-cyan    ((,class (:foreground ,warning :background ,warning))))
 ;; `(term-color-white   ((,class (:foreground ,background :background ,background))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'k)
