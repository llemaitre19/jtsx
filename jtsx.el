;;; jtsx.el --- Extends default support for JSX/TSX -*- lexical-binding: t -*-

;; Copyright (C) 2023 Loïc Lemaître

;; Author: Loïc Lemaître <loic.lemaitre@gmail.com>
;; Maintainer: Loïc Lemaître <loic.lemaitre@gmail.com>
;; URL: https://github.com/llemaitre19/jtsx
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.2.0
;; Keywords: languages
;; License: GNU General Public License 3

;;; Commentary:
;; jtsx is a package for editing JSX or TSX files.  It provides jsx-mode and
;; tsx-mode major modes implemented respectively on top of js-ts-mode and
;; tsx-ts-mode, benefiting thus from the new tree-sitter built-in feature.

;; Summary of features and fixes:
;; * Fix commenting and indenting issues with JSX code in Emacs built-in
;; js-ts-mode and tsx-ts-mode modes
;; * Refactoring: moving, wrapping, renaming `JSX` elements
;; * Jumping between opening and closing `JSX` elements
;; * Electric JSX closing element
;; * Code folding

;;; Code:

(require 'treesit)
(require 'newcomment)
(require 'hideshow)

(eval-and-compile
  ;; HACK
  ;; Prevent language warnings when loading `typescript-ts-mode` (when Tree-sitter
  ;; typescript and tsx languages are not installed yet).
  ;; Temporary advice `treesit-ready-p' function to make it quiet.
  (defun jtsx-treesit-ready-p (orig-func &rest args)
    "Advice ORIG-FUNC to make it quiet.
First element of ARGS and t are the new arguments."
  (apply orig-func (list (car args) t)))

  (advice-add 'treesit-ready-p :around #'jtsx-treesit-ready-p)
  (require 'typescript-ts-mode)
  (advice-remove 'treesit-ready-p #'jtsx-treesit-ready-p))

(defgroup jtsx nil
  "Extends default treesit support for JSX/TSX."
  :group 'languages)

(defcustom jtsx-switch-indent-offset 0
  "Offset for indenting the contents of a switch block.
The value must not be negative."
  :type 'integer
  :safe 'integerp
  :group 'jtsx)

(defcustom jtsx-indent-statement-block-regarding-standalone-parent nil
  "Use standalone parent as anchor to evaluate statement block indentation.
If t and if the parent of a statement block is not on its own line, the
statement block will be indented relative to the beginning of the whole parent
continuated expression."
  :type 'boolean
  :safe 'booleanp
  :group 'jtsx)

(defcustom jtsx-jsx-element-move-allow-step-out t
  "Allow to step out when moving a jsx element."
  :type 'boolean
  :safe 'booleanp
  :group 'jtsx)

(defcustom jtsx-enable-jsx-electric-closing-element t
  "Enable electric JSX closing element feature."
  :type 'boolean
  :safe 'booleanp
  :group 'jtsx)

(defcustom jtsx-enable-all-syntax-highlighting-features t
  "Enable all available syntax highlighting features.
If nil, default level defined by the underlying major mode is used.
See `treesit-font-lock-level' for more informations."
  :type 'boolean
  :safe 'booleanp
  :group 'jtsx)

(defconst jtsx-jsx-ts-root-keys '("jsx_element"
                                  "jsx_self_closing_element"
                                  "jsx_expression"
                                  "jsx_text"))

(defconst jtsx-jsx-ts-element-tag-keys '("jsx_opening_element" "jsx_closing_element"))

(defconst jtsx-jsx-hs-root-keys '("jsx_element" "jsx_expression"))

(defvar jtsx-ts-indent-rules)

(defun jtsx-node-jsx-context-p (node)
  "Check if NODE inside JSX context."
  (member (treesit-node-type node) '("jsx_expression"
                                     "jsx_element"
                                     "jsx_attribute"
                                     "jsx_self_closing_element"
                                     "jsx_text"
                                     "jsx_opening_element"
                                     "jsx_closing_element"
                                     "jsx_namespace_name")))

(defun jtsx-jsx-context-at-p (position)
  "Check if inside JSX context at POSITION."
  (save-excursion
    (let ((pred (lambda (n) (jtsx-node-jsx-context-p n))))
      (treesit-parent-until (treesit-node-at position) pred t))))

(defun jtsx-jsx-context-p ()
  "Check if in JSX context at point or at region ends."
  (and (jtsx-jsx-context-at-p (point)) (or (not (region-active-p)) (jtsx-jsx-context-at-p (mark)))))

(defun jtsx-jsx-attribute-context-at-p (position)
  "Check if insinde a JSX element attribute at POSITION."
  (when-let ((node (treesit-node-at position)))
    (jtsx-enclosing-jsx-node node '("jsx_attribute"))))

(defun jtsx-jsx-attribute-context-p ()
  "Check if in JSX element attribute context at point or at region ends."
  (let ((jsx-attr-at-point (jtsx-jsx-attribute-context-at-p (point))))
    (if (not (region-active-p))
        jsx-attr-at-point
      (let ((jsx-attr-at-mark (jtsx-jsx-attribute-context-at-p (mark)))
            (is-closing-tag (lambda (pos) (equal (treesit-node-type (treesit-node-at pos)) ">"))))
        (cond ((and jsx-attr-at-point jsx-attr-at-mark) t)
              ((and (not jsx-attr-at-point) (not jsx-attr-at-mark)) nil)
              ;; Supports this case : <A attr> : if point or mark is between "r" and ">"
              ;; it is not considered insite the attribute, but on the ">" node.
              (jsx-attr-at-point (funcall is-closing-tag (mark)))
              (jsx-attr-at-mark (funcall is-closing-tag (point))))))))

(defun jtsx-comment-jsx-dwim (arg)
  "Comment or uncomment JSX at point or in region.
See `comment-dwim' documentation for ARG usage."
  (let ((comment-start "{/* ")
        (comment-end " */}")
        (comment-use-syntax nil)
        (comment-start-skip "\\([[:space:]]*\\)\\(//+\\|{?/\\*+\\)")
        (comment-end-skip "\\*+/}?[[:space:]]*"))
    (comment-dwim arg)))

(defun jtsx-comment-jsx-attribute-dwim (arg)
  "Comment or uncomment JSX element attribute at point or in region.
See `comment-dwim' documentation for ARG usage."
  (let ((comment-start "/* ")
        (comment-end " */")
        (comment-use-syntax nil)
        (comment-start-skip "\\([[:space:]]*\\)\\(//+\\|/\\*+\\)")
        (comment-end-skip "\\*+/[[:space:]]*"))
    (comment-dwim arg)))

(defun jtsx-comment-dwim (arg)
  "Add support for commenting/uncommenting inside JSX.
See `comment-dwim' documentation for ARG usage."
  (interactive "*P")
  (if (jtsx-jsx-context-p)
      (if (jtsx-jsx-attribute-context-p)
          (jtsx-comment-jsx-attribute-dwim arg)
        (jtsx-comment-jsx-dwim arg))
    (comment-dwim arg)))

(defun jtsx-enclosing-jsx-node (node types &optional fallback-types include-node jsx-exp-guard)
  "Get first parent of NODE matching one of TYPES.
If the research failed and FALLBACK-TYPES are not nil retry with FALLBACK-TYPES.
If INCLUDE-NODE is not nil, NODE is included in the research.
If JSX-EXP-GUARD is not nil, do not traverse jsx expression."
  (let ((enclosing-node (if include-node node (treesit-node-parent node))))
    (while (and enclosing-node (not (member (treesit-node-type enclosing-node) types)))
      (setq enclosing-node (if (and jsx-exp-guard
                                    (equal (treesit-node-type enclosing-node) "jsx_expression"))
                               nil ; give up the research
                             (treesit-node-parent enclosing-node))))
    (if (or enclosing-node (not fallback-types))
        enclosing-node
      (jtsx-enclosing-jsx-node node fallback-types nil include-node jsx-exp-guard))))

(defun jtsx-enclosing-jsx-element (node)
  "Get first parent of NODE matching `jsx_element' type."
  (jtsx-enclosing-jsx-node node '("jsx_element") nil t))

(defun jtsx-enclosing-jsx-element-at-point ()
  "Get first parent matching `jsx_element' type at point."
  (jtsx-enclosing-jsx-element (treesit-node-at (point))))

(defun jtsx-jump-jsx-opening-tag ()
  "Jump to the opening tag of the JSX element."
  (interactive)
  (let ((enclosing-element (jtsx-enclosing-jsx-element-at-point)))
    (if enclosing-element
        (goto-char (1+ (treesit-node-start enclosing-element))) ; +1 to jump right after the "<"
      (message "No JSX opening element found."))))

(defun jtsx-jump-jsx-closing-tag ()
  "Jump to the closing tag of the JSX element."
  (interactive)
  (let ((enclosing-element (jtsx-enclosing-jsx-element-at-point)))
    (if enclosing-element
        (goto-char (1- (treesit-node-end enclosing-element))) ; -1 to jump right before the "/>"
      (message "No JSX closing element found."))))

(defun jtsx-jump-jsx-element-tag-dwim ()
  "Jump either to the opening or the closing tag of the JSX element."
  (interactive)
  (let ((enclosing-element (jtsx-enclosing-jsx-element-at-point)))
    (if enclosing-element
        (let ((start (treesit-node-start enclosing-element))
              (end (treesit-node-end enclosing-element)))
          (if (> (point) (+ start (/ (- end start) 2)))
              (jtsx-jump-jsx-opening-tag) ; We are closer to the closing tag.
            (jtsx-jump-jsx-closing-tag))) ; We are closer to the opening tag.
      (message "No JSX element found."))))

(defun jtsx-rename-jsx-identifier (node new-name)
  "Rename the NODE named tag to NEW-NAME."
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (cl-assert (and node-start node-end) nil "Unable to retrieve start or end of node.")
    (save-excursion
      (delete-region node-start node-end)
      (goto-char node-start)
      (insert new-name)
      (point))))

(defun jtsx-rename-jsx-fragment (node new-name)
  "Rename the NODE fragment to NEW-NAME."
  (let ((node-end (treesit-node-end node)))
    (cl-assert node-end nil "Unable to retrieve end of node.")
    (save-excursion
      (goto-char (1- node-end)) ; -1 to be inside <> or </>
      (insert new-name)
      (point))))

(defun jtsx-jsx-fragment-p (node)
  "Check if NODE is a JSX fragment."
  (and (member (treesit-node-type node) jtsx-jsx-ts-element-tag-keys)
       ;; JSX Fragment tags only have 2 children : "<" (or "</") and ">".
       ;; Other JSX elemeny tags have at least one additinal children which is the identifier.
       (eq (treesit-node-child-count node) 2)))

(defun jtsx-rename-jsx-element-tag (new-name child-field-name &optional)
  "Rename a JSX element tag to NEW-NAME.
CHILD-FIELD-NAME identify the tag to rename (`open_tag' or `close_tag')."
  (cl-assert (member child-field-name '("open_tag" "close_tag"))
             t "Unexpected child-field-name: %s.")
  ;; Node and parent node are not passed as argument because they must be as up to
  ;; date as possible since the function alters the buffer and hense the treesit
  ;; tree.
  ;; Note that treesit parser is robust enough to be not confused by mismaching
  ;; element tag identifiers.
  (let* ((node (treesit-node-at (point)))
         (element-node (jtsx-enclosing-jsx-element node)))
    (cl-assert element-node nil "Unable to retrieve the enclosing jsx_element node.")
    (let* ((tag-node (treesit-node-child-by-field-name element-node child-field-name))
           (fragment (jtsx-jsx-fragment-p tag-node))
           (node-to-rename (if fragment tag-node
                             ;; Get identifier node
                             (treesit-node-child-by-field-name tag-node "name"))))
      (cl-assert node-to-rename nil "Unable to retrieve the node to rename.")
      (if fragment (jtsx-rename-jsx-fragment node-to-rename new-name)
        (jtsx-rename-jsx-identifier node-to-rename new-name)))))

(defun jtsx-rename-jsx-element (new-name)
  "Rename a JSX element to NEW-NAME at point.
Point can be in the opening or closing."
  (interactive "sRename element to: ")
  (let* ((node (treesit-node-at (point)))
         (node-type (treesit-node-type node))
         (parent-node (treesit-node-parent node))
         (parent-node-type (treesit-node-type parent-node)))
    (unless (and (member node-type '("identifier" ">"))
                 (cond ((equal parent-node-type "jsx_self_closing_element")
                        (jtsx-rename-jsx-identifier node new-name))
                       ((member parent-node-type jtsx-jsx-ts-element-tag-keys)
                        (jtsx-rename-jsx-element-tag new-name "open_tag")
                        (jtsx-rename-jsx-element-tag new-name "close_tag"))))
      (message "No JSX element to rename."))))

(defun jtsx-first-child-jsx-node (node types &optional backward)
  "Find the first child of NODE matching one of the TYPES.
If BACKWARD is not nil, start the search by the last children of NODE."
  (when node
    (let ((children (cl-remove-if-not (lambda (n) (member (treesit-node-type n) types))
                                      (treesit-node-children node))))
      (when (length> children 0) (car (if backward (last children) children))))))

(defun jtsx-first-child-jsx-element-tag (node &optional backward)
  "Find the first child of NODE being a JSX element tag.
If BACKWARD is not nil, start the search by the last children of NODE."
  (jtsx-first-child-jsx-node node jtsx-jsx-ts-element-tag-keys backward))

(defun jtsx-first-child-jsx-element (node &optional backward)
  "Find the first child of NODE being a JSX element.
If BACKWARD is not nil, start the search by the last children of NODE."
  (jtsx-first-child-jsx-node node jtsx-jsx-ts-root-keys backward))

(defun jtsx-first-sibling-jsx-element (node &optional backward)
  "Find the first sibling of NODE being a JSX element.
If BACKWARD is not nil, start the search by the last children of NODE."
  (let ((sibling (if backward
                     (treesit-node-prev-sibling node)
                   (treesit-node-next-sibling node))))
    (when (member (treesit-node-type sibling) jtsx-jsx-ts-root-keys)
      sibling)))

(defun jtsx-find-jsx-element-valid-move (full-element-move backward allow-step-in)
  "Find a valid move for a JSX element (or any JSX root node).
Root nodes are defined in `jtsx-jsx-ts-root-keys'.
If FULL-ELEMENT-MOVE is nil, consider moving only the element tag at point.
If BACKWARD is not nil, search for valid move before point, else after point.
If ALLOW-STEP-IN is not nil, a move can go deeper in the JSX hierarchy.  Only
used if FULL-ELEMENT-MOVE is t.
Return a plist containing the move information : `:node-start', `:node-end',
 `:line'."
  (let* ((current-node (jtsx-enclosing-jsx-node (treesit-node-on
                                                 (save-excursion (back-to-indentation) (point))
                                                 (pos-eol))
                                                jtsx-jsx-ts-element-tag-keys
                                                jtsx-jsx-ts-root-keys
                                                t
                                                t)) ; Do not traverse jsx expression
         (current-node-type (treesit-node-type current-node))
         (enclosing-node (jtsx-enclosing-jsx-node current-node
                                                  jtsx-jsx-ts-root-keys
                                                  nil
                                                  t
                                                  t)) ; Do not traverse jsx expression
         (moving-opening-or-closing-tag (and (not full-element-move)
                                             (member current-node-type
                                                     jtsx-jsx-ts-element-tag-keys)))
         (node (if moving-opening-or-closing-tag
                   ;; We do not want the enclosing node as we only move the opening or closing
                   ;; element
                   current-node
                 enclosing-node))
         (node-type (treesit-node-type node))
         (node-candidate (if (and moving-opening-or-closing-tag
                                  (or (and backward (equal node-type "jsx_closing_element"))
                                      (and (not backward) (equal node-type "jsx_opening_element"))))
                             ;; We are looking at children inside node. It just happens when
                             ;; we only move opening or closing tag and the move direction
                             ;; will make a child element go out of the parent element. For
                             ;; example, if moving <div> forward or </div> backward:
                             ;; <div>
                             ;;   <span>
                             ;;   </span>
                             ;; </div>
                             (jtsx-first-child-jsx-element enclosing-node backward)
                           ;; We are looking at NODE siblings.
                           (jtsx-first-sibling-jsx-element enclosing-node backward))))
    (when-let ((node-final-candidate
                ;; Manage step in and step out
                (cond
                 ;; No step in or out
                 ((not full-element-move) node-candidate)
                 ;; Try to step in
                 ((and node-candidate allow-step-in)
                  (let ((element-tag-node (jtsx-first-child-jsx-element-tag node-candidate
                                                                            backward)))
                    (if (member (treesit-node-type element-tag-node) jtsx-jsx-ts-element-tag-keys)
                        element-tag-node
                      node-candidate)))
                 ;; Try to step out
                 ((and (not node-candidate) jtsx-jsx-element-move-allow-step-out)
                  (let* ((parent-node
                          ;; Step out target can only be a JSX element
                          (jtsx-enclosing-jsx-node enclosing-node '("jsx_element") nil nil t))
                         (grand-parent-node (treesit-node-parent parent-node)))
                    ;; Ensure not to go out of JSX context
                    (when (jtsx-node-jsx-context-p grand-parent-node)
                      parent-node)))
                 (t node-candidate))))
      (let ((node-start (treesit-node-start node))
            (node-end (treesit-node-end node))
            (line (if backward
                      (line-number-at-pos (treesit-node-start node-final-candidate))
                    ;; +1 for insertion
                    (1+ (line-number-at-pos (treesit-node-end node-final-candidate))))))
        (cl-assert (and node-start node-end line) nil
                   "Unable to retrieve line, node start or node end.")
        (list :node-start node-start :node-end node-end :line line)))))

(defun jtsx-goto-line (line)
  "Go to the beginning of LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun jtsx-move-jsx-element (full-element-move backward &optional allow-step-in)
  "Move a JSX element (or any JSX root node).
Root nodes are defined in `jtsx-jsx-ts-root-keys'.
If FULL-ELEMENT-MOVE is nil, consider moving only the element tag at point.
If BACKWARD is not nil, search for valid move before point, else after point.
If ALLOW-STEP-IN is not nil, a move can go deeper in the JSX hierarchy.  Only
used if FULL-ELEMENT-MOVE is t."
  (if (jtsx-jsx-context-p)
      (if-let ((res (jtsx-find-jsx-element-valid-move full-element-move backward
                                                      allow-step-in)))
          (let* ((node-start (plist-get res :node-start))
                 (node-end (plist-get res :node-end))
                 (line (plist-get res :line))
                 (region-start (save-excursion (goto-char node-start)(pos-bol)))
                 (region-end (save-excursion (goto-char node-end) (forward-line 1) (point)))
                 (lines-count (count-lines region-start region-end))
                 ;; Take into account region lines for prev-line and yank-line depending on
                 ;; the move direction.
                 (prev-line (if backward  (+ (line-number-at-pos region-end) lines-count)
                              (line-number-at-pos region-start)))
                 (yank-line (- line (if backward 0 lines-count))))
            (kill-region region-start region-end)
            (jtsx-goto-line yank-line)
            (yank '(1))
            (indent-region (save-excursion (when (not backward) (jtsx-goto-line prev-line))
                                           (pos-bol))
                           (save-excursion (when backward (jtsx-goto-line prev-line))
                                           (forward-line (- lines-count 1)) (pos-eol))))
        (message "No move in this direction."))
    (message "Not inside jsx context.")))

(defun jtsx-move-jsx-element-tag-forward ()
  "Move a JSX element tag (opening or closing) forward."
  (interactive)
  (jtsx-move-jsx-element nil nil))

(defun jtsx-move-jsx-element-tag-backward ()
  "Move a JSX element tag (opening or closing) backward."
  (interactive)
  (jtsx-move-jsx-element nil t))

(defun jtsx-move-jsx-element-forward ()
  "Move a JSX element (or any JSX root node) forward."
  (interactive)
  (jtsx-move-jsx-element t nil))

(defun jtsx-move-jsx-element-backward ()
  "Move a JSX element (or any JSX root node) backward."
  (interactive)
  (jtsx-move-jsx-element t t))

(defun jtsx-move-jsx-element-step-in-forward ()
  "Move a JSX element (or any JSX root node) forward.
Step into sibling elements if possible."
  (interactive)
  (jtsx-move-jsx-element t nil t))

(defun jtsx-move-jsx-element-step-in-backward ()
  "Move a JSX element (or any JSX root node) backward.
Step into sibling elements if possible."
  (interactive)
  (jtsx-move-jsx-element t t t))

(defun jtsx-treesit-syntax-error-p ()
  "Check if there are errors reported by treesit."
  (jtsx-enclosing-jsx-node (treesit-node-at (point)) '("ERROR")))

(defun jtsx-jsx-electric-closing-element (n)
  "Insert `>' and the associated closing tag (`</xxx>') if expected.
N is a numeric prefix argument.  If greater than 1, insert N times `>', but
 never insert closing tag."
  (interactive "p")
  (insert-char (char-from-name "GREATER-THAN SIGN") n t)
  (when (and (= n 1) jtsx-enable-jsx-electric-closing-element (jtsx-jsx-context-p))
    (when-let ((node (treesit-node-at (1- (point))))) ; Safer to get node inside the tag
      (when-let ((parent-node (treesit-node-parent node)))
        (when (and (equal (treesit-node-type node) ">")
                   (equal (treesit-node-type parent-node) "jsx_opening_element"))
          (let* ((identifier-node (treesit-node-child-by-field-name parent-node "name"))
                 (identifier (if identifier-node
                                 (buffer-substring (treesit-node-start identifier-node)
                                                   (treesit-node-end identifier-node))
                               ""))
                 (closing-tag (format "</%s>" identifier)))
            ;; We try to guess if auto adding the closing tag is expected or not.  We assume that
            ;; before inserting the new opening tag, the code syntax was clean. So if after adding
            ;; the new opening tag we detect a syntax issue, that means a closing tag is expected.
            ;; This logic is quite basic, but no sure we can really do better with treesit
            ;; informations about syntax issues.
            (when (jtsx-treesit-syntax-error-p)
              (save-excursion (insert closing-tag)))))))))

(defun jtsx-trimmed-region ()
  "Return the trimmed region as a plist.
Keys are `:start' and `:end'."
  (let* ((start-pos (min (point) (mark)))
         (end-pos (max (point) (mark)))
         (trimmed-start-pos (save-excursion
                              (goto-char start-pos)
                              (skip-chars-forward " \t\n")
                              (point)))
         (trimmed-end-pos (save-excursion
                            (goto-char end-pos)
                            (skip-chars-backward " \t\n")
                            (point))))
    (if (< trimmed-start-pos trimmed-end-pos)
        `(:start ,trimmed-start-pos :end ,trimmed-end-pos)
      ;; Something is going wrong, fallback to initail region
      `(:start ,start-pos :end ,end-pos))))

(defun jtsx-wrap-in-jsx-element (element-name)
  "Wrap JSX root nodes in a JSX element.
Nodes are selected by a region if there is an active one.  Else the node at
 point is used.
ELEMENT-NAME is the name of the new wrapping element."
  (interactive "sJSX element name: ")
  (if (jtsx-jsx-context-p)
      (let* ((start-pos (if (region-active-p) (plist-get (jtsx-trimmed-region) :start) (point)))
             (end-pos (if (region-active-p) (plist-get (jtsx-trimmed-region) :end) (point)))
             (element-start (jtsx-enclosing-jsx-node (treesit-node-at start-pos)
                                                     jtsx-jsx-ts-root-keys nil t))
             (element-end (if (region-active-p)
                              (jtsx-enclosing-jsx-node (treesit-node-at end-pos)
                                                       jtsx-jsx-ts-root-keys nil t)
                            element-start)))
        (cl-assert (and element-start element-end) "Not able to retrieve node start or node end.")
        (let ((opening-line (line-number-at-pos (treesit-node-start element-start)))
              (closing-line (1+ (line-number-at-pos (treesit-node-end element-end)))) ; +1 for
              ;; insertion
              (opening-tag (format "<%s>\n" element-name))
              (closing-tag (format "</%s>\n" element-name)))
          (save-excursion
            (jtsx-goto-line closing-line)
            (insert closing-tag)
            (jtsx-goto-line opening-line)
            (insert opening-tag))
          (indent-region (save-excursion (jtsx-goto-line opening-line) (pos-bol))
                         (save-excursion (jtsx-goto-line (1+ closing-line)) (pos-eol))) ; + 1
          ;; because of the opening tag insertion
          (jtsx-goto-line opening-line)
          (search-forward ">")
          (backward-char 1))) ; Let the cursor ready to add attributes
    (message "Not inside jsx context.")))

(defun jtsx-hs-forward-sexp (n)
  "Make `forward-sexp' compatible with Hideshow in JSX.
See `forward-sexp' documentation for informations about N argument."
  (interactive "p")
  (unless (and (jtsx-jsx-context-p)
               (when-let* ((node (treesit-node-at (point)))
                           (enclosing-node (jtsx-enclosing-jsx-node node jtsx-jsx-hs-root-keys))
                           (end-pos (treesit-node-end enclosing-node)))
                 (goto-char end-pos)))
    ;; Starting Emacs 30, treesit set its own function, which has some issues.
    ;; Bug report: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66988
    ;; Use the default one instead.
    (let ((forward-sexp-function nil))
      (forward-sexp n))))

(defun jtsx-hs-find-block-beginning ()
  "Enhance `hs-find-block-beginning' for JSX."
  (unless (and (jtsx-jsx-context-p)
               (when-let* ((node (treesit-node-at (point)))
                           (enclosing-node (jtsx-enclosing-jsx-node node jtsx-jsx-hs-root-keys))
                           (start-pos (treesit-node-start enclosing-node)))
                 (goto-char start-pos)))
    (hs-find-block-beginning)))

(defmacro jtsx-ts-indent-rules-for-key (ts-lang-key)
  "Extract indent rules for TS-LANG-KEY language from `jtsx-ts-indent-rules'."
  `(cdr (assoc ,ts-lang-key jtsx-ts-indent-rules)))

(defun jtsx-ts-add-indent-rule (ts-lang-key rule &optional add-first)
  "Add an indent RULE for TS-LANG-KEY language into `jtsx-ts-indent-rules'.
If ADD-FIRST is not nil, preprend the RULE in the list for priority purpose."
  (let* ((original-rules (jtsx-ts-indent-rules-for-key ts-lang-key))
         (new-rules (if add-first
                        (append `(,rule) original-rules)
                      (append original-rules `(,rule)))))
    (setf (jtsx-ts-indent-rules-for-key ts-lang-key) new-rules)))

(defun jtsx-ts-remove-indent-rule (ts-lang-key rule)
  "Remove an indent RULE for TS-LANG-KEY language from `jtsx-ts-indent-rules'."
  (let* ((original-rules (jtsx-ts-indent-rules-for-key ts-lang-key))
         (new-rules (remove rule original-rules)))
    (setf (jtsx-ts-indent-rules-for-key ts-lang-key) new-rules)))

(defun jtsx-configure-mode-base (mode
                                 mode-map
                                 ts-lang-key
                                 indent-var-name)
  "Base function for JSX/TSX Major mode configuration.
MODE, MODE-MAP, TS-LANG-KEY, INDENT-VAR-NAME variables allow customization
 depending on the mode."
  ;; Bind keys
  (define-key mode-map [remap comment-dwim] 'jtsx-comment-dwim)
  (define-key mode-map ">" #'jtsx-jsx-electric-closing-element)

  ;; JSX folding with Hideshow
  (add-to-list 'hs-special-modes-alist
               `(,mode "{\\|(\\|<[^/>]*>" "}\\|)\\|</[^/>]*>" "/[*/]"
                       jtsx-hs-forward-sexp
                       nil
                       jtsx-hs-find-block-beginning))

  ;; Patch indentation
  (jtsx-ts-add-indent-rule ts-lang-key
                           `((parent-is "switch_body") parent-bol ,jtsx-switch-indent-offset))
  (when jtsx-indent-statement-block-regarding-standalone-parent
    (jtsx-ts-add-indent-rule ts-lang-key '((node-is "}") standalone-parent 0) t)
    (jtsx-ts-remove-indent-rule ts-lang-key '((node-is "}") parent-bol 0))
    (jtsx-ts-add-indent-rule ts-lang-key
                             `((parent-is "statement_block") standalone-parent ,indent-var-name))
    (jtsx-ts-remove-indent-rule ts-lang-key
                                `((parent-is "statement_block") parent-bol ,indent-var-name))
    (jtsx-ts-add-indent-rule ts-lang-key
                             `((node-is "statement_block") standalone-parent ,indent-var-name))
    (jtsx-ts-remove-indent-rule ts-lang-key
                                `((node-is "statement_block") parent-bol ,indent-var-name))
    (jtsx-ts-add-indent-rule ts-lang-key
                             `((node-is "statement_block") standalone-parent ,indent-var-name))
    (jtsx-ts-remove-indent-rule ts-lang-key
                                `((node-is "statement_block") parent-bol ,indent-var-name)))
  (setq-local treesit-simple-indent-rules jtsx-ts-indent-rules)

  ;; Use maximum level of syntax highlighting if enabled
  (when jtsx-enable-all-syntax-highlighting-features
    (setq-local treesit-font-lock-level 4))

  ;; Apply treesit customization
  (treesit-major-mode-setup))

(defun jtsx-prioritize-mode-if-present (mode)
  "Prioritize MODE entries in `auto-mode-alist'."
  (let ((entries-to-prioritize (seq-remove (lambda (entry) (not (eq mode (cdr entry))))
                                           auto-mode-alist)))
    (mapc (lambda (entry) (progn
                            ;; Delete same entry first if existing, so that the new entry will be
                            ;; set with the highest priority.
                            (delete entry auto-mode-alist)
                            (add-to-list 'auto-mode-alist entry)))
          entries-to-prioritize)))

(defun jtsx-deep-copy-indent-rules (key rules)
  "Get a (partial) deep copy of RULES alist for KEY language."
  (let ((copy (copy-alist rules)))
    (setf (cdr (assoc key copy)) (mapcar (lambda (rule) rule) (cdr (assoc key copy))))
    (cl-copy-list copy)))

;;;###autoload
(define-derived-mode jsx-mode js-ts-mode "JSX"
  "Major mode extending `js-ts-mode'."
  :group 'jtsx
  (let ((ts-lang-key 'javascript))
    (when (treesit-ready-p ts-lang-key)
      (jtsx-prioritize-mode-if-present 'jsx-mode) ; js-ts-mode mode sets auto-mode-alist when loaded
      ;; Do a deep copy of javascript indent rules variable, to prevent side effects as we will
      ;; modify it.
      (setq-local jtsx-ts-indent-rules
                  (jtsx-deep-copy-indent-rules 'javascript js--treesit-indent-rules))
      (jtsx-ts-remove-indent-rule ts-lang-key
                                  '((node-is "switch_\\(?:case\\|default\\)") parent-bol 0))
      (when (version= emacs-version "29.1")
        (jtsx-ts-remove-indent-rule ts-lang-key '(js-jsx--treesit-indent-compatibility-bb1f97b))
        (mapc (lambda (rule) (jtsx-ts-add-indent-rule 'javascript rule))
              (js-jsx--treesit-indent-compatibility-bb1f97b)))
      (jtsx-configure-mode-base 'jsx-mode jsx-mode-map ts-lang-key 'js-indent-level))))

;;;###autoload
(define-derived-mode tsx-mode tsx-ts-mode "TSX"
  "Major mode extending `tsx-ts-mode'."
  :group 'jtsx
  (let ((ts-lang-key 'tsx))
    (when (treesit-ready-p ts-lang-key)
      (setq-local jtsx-ts-indent-rules (typescript-ts-mode--indent-rules 'tsx))
      (jtsx-configure-mode-base 'tsx-mode tsx-mode-map 'tsx 'typescript-ts-mode-indent-offset))))

;; typescript-ts-mode package sets auto-mode-alist when loaded
(jtsx-prioritize-mode-if-present 'tsx-mode)

;;;###autoload
(defun jtsx-install-treesit-language (ts-lang-key)
  "Wrapper around `treesit-install-language-grammar' with preset sources.
TS-LANG-KEY is the language to be installed."
  ;; Known bug : calling `treesit-install-language-grammar' multiple times for the same language
  ;; seems buggy.
  ;; Fixed by commit 1098c114b74 in emacs 30.0.50
  ;; (bug https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66673)
  (interactive (list (intern (completing-read "Language: " '(javascript tsx)))))
  (unless (alist-get ts-lang-key treesit-language-source-alist)
    (let ((source (pcase ts-lang-key
                    ('javascript '("https://github.com/tree-sitter/tree-sitter-javascript"
                                   "master"
                                   "src"))
                    ('tsx '("https://github.com/tree-sitter/tree-sitter-typescript"
                            "master"
                            "tsx/src"))
                    (_ nil))))
      (cl-assert source (format "Not expected language: %s" ts-lang-key))
      (let ((lang-source (cons ts-lang-key source)))
        (if treesit-language-source-alist
            (add-to-list 'treesit-language-source-alist lang-source)
          (setq treesit-language-source-alist (list lang-source))))))
  (funcall-interactively #'treesit-install-language-grammar ts-lang-key))

(provide 'jtsx)
;;; jtsx.el ends here
