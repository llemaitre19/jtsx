;;; jtsx.el --- Extends JSX/TSX built-in support -*- lexical-binding: t -*-

;; Copyright (C) 2023 Loïc Lemaître

;; Author: Loïc Lemaître <loic.lemaitre@gmail.com>
;; Maintainer: Loïc Lemaître <loic.lemaitre@gmail.com>
;; URL: https://github.com/llemaitre19/jtsx
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.5.1
;; Keywords: languages

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; jtsx is a package for editing JSX or TSX files.  It provides jtsx-jsx-mode
;; and jtsx-tsx-mode major modes implemented respectively on top of js-ts-mode
;; and tsx-ts-mode, benefiting thus from the new tree-sitter built-in feature.

;; Summary of features:
;; * Commenting/uncommenting JSX code
;; * Refactoring: moving, wrapping/unwrapping, deleting, renaming JSX elements
;; * Jumping between opening and closing JSX elements
;; * Electric JSX closing element and new line
;; * Code folding
;; * Some additional indentation options

;; Note on the default configuration:
;; This package doesn't automatically register its provided major modes into
;; auto-mode-alist.  It is up to the user to do the association, letting him/her
;; choose which of the provided modes he/she wants to use.  Same consideration
;; applies for command shortcuts.

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

;; TODO: the switch indentation level (ie case and default) should default to the major mode
;; indentation level (most common than no indentation). To be done into a major jtsx release.
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

(defcustom jtsx-enable-jsx-element-tags-auto-sync nil
  "Enable jsx automatic synchronization of element opening and closing tag name."
  :type 'boolean
  :safe 'booleanp
  :group 'jtsx)

(defcustom jtsx-enable-jsx-electric-closing-element t
  "Enable electric JSX closing element feature."
  :type 'boolean
  :safe 'booleanp
  :group 'jtsx)

(defcustom jtsx-enable-electric-open-newline-between-jsx-element-tags t
  "Enable electric new line between jsx element tags."
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

(defconst jtsx-jsx-ts-keys '("jsx_expression"
                             "jsx_element"
                             "jsx_attribute"
                             "jsx_self_closing_element"
                             "jsx_text"
                             "jsx_opening_element"
                             "jsx_closing_element"
                             "jsx_namespace_name"))

(defconst jtsx-jsx-ts-root-keys '("jsx_element"
                                  "jsx_self_closing_element"
                                  "jsx_expression"
                                  "jsx_text"))

(defconst jtsx-jsx-ts-element-tag-keys '("jsx_opening_element" "jsx_closing_element"))

(defconst jtsx-hs-block-keys '(;; Parentheses
                               "arguments"
                               "formal_parameters"
                               "parenthesized_expression"
                               ;; Brackets
                               "array"
                               "array_pattern"
                               ;; Braces
                               "class_body"
                               "export_clause"
                               "jsx_expression"
                               "object"
                               "object_pattern"
                               "named_imports"
                               "statement_block"
                               "switch_body"
                               ;; JSX elements
                               "jsx_element"))

(defvar jtsx-ts-indent-rules)

(defvar-local jtsx-last-buffer-chars-modifed-tick 0)

(defun jtsx-goto-none-empty-content (arg)
  "Go to none empty content.
If ARG >= O go forward, else backward."
  (let ((skipped-chars " \t\n\r"))
    (if (>= arg 0)
        (skip-chars-forward skipped-chars)
      (skip-chars-backward skipped-chars))))

(defun jtsx-goto-content-forward ()
  "Go forward to none empty content."
  (jtsx-goto-none-empty-content 1))

(defun jtsx-goto-content-backward ()
  "Go backward to none empty content."
  (jtsx-goto-none-empty-content -1))

(defun jtsx-save-buffer-chars-modified-tick ()
  "Save the returned value of `buffer-chars-modified-tick' function."
  (setq-local jtsx-last-buffer-chars-modifed-tick (buffer-chars-modified-tick)))

(defun jtsx-command-modified-buffer-p ()
  "Check if last command has modified the buffer."
  (< jtsx-last-buffer-chars-modifed-tick (buffer-chars-modified-tick)))

(defun jtsx-treesit-node-at (pos)
  "Return the treesit node at POS or POS-1 depending on the context.
Treesit looks at the node after the position (excepted when at the end of the
line), it fine in most situations, excepted when at the end of a region where
getting the treesit node before position is more suitable."
  (let ((effective-pos (if (and (region-active-p) (eq pos (region-end))) (1- pos) pos)))
    (treesit-node-at effective-pos)))

(defun jtsx-traversing-jsx-expression-p (node initial-node)
  "Check whether we are going outside a jsx expression.
NODE is the current node, and INITIAL-NODE is the node which the research has
started from.  We consider that we are not traversing the jsx-expression if
one of the INITIAL-NODE and NODE positions matches (start or end), ie we were
already outside it."
  (and (equal (treesit-node-type node) "jsx_expression")
       (not (eq (treesit-node-start initial-node) (treesit-node-start node)))
       (not (eq (treesit-node-end initial-node) (treesit-node-end node)))))

(defun jtsx-enclosing-jsx-node (node types &optional fallback-types include-node jsx-exp-guard)
  "Get first parent of NODE matching one of TYPES.
If the research failed and FALLBACK-TYPES are not nil retry with FALLBACK-TYPES.
If INCLUDE-NODE is not nil, NODE is included in the research.
If JSX-EXP-GUARD is not nil, do not traverse jsx expression."
  (let* ((get-parent-node (lambda (current-node)
                           (let ((parent-node (treesit-node-parent current-node)))
                             (if (and jsx-exp-guard
                                      (jtsx-traversing-jsx-expression-p parent-node node))
                                 nil
                               parent-node))))
         (enclosing-node (if include-node node (funcall get-parent-node node))))
    (while (and enclosing-node (not (member (treesit-node-type enclosing-node) types)))
      (setq enclosing-node (funcall get-parent-node enclosing-node)))
    (if (or enclosing-node (not fallback-types))
        enclosing-node
      (jtsx-enclosing-jsx-node node fallback-types nil include-node jsx-exp-guard))))

(defun jtsx-enclosing-jsx-element (node &optional jsx-exp-guard)
  "Get first parent of NODE matching `jsx_element' type.
If JSX-EXP-GUARD is not nil, do not traverse jsx expression."
  (jtsx-enclosing-jsx-node node '("jsx_element") nil t jsx-exp-guard))

(defun jtsx-enclosing-jsx-element-at-point (&optional jsx-exp-guard)
  "Get first parent matching `jsx_element' type at point.
If JSX-EXP-GUARD is not nil, do not traverse jsx expression."
  (jtsx-enclosing-jsx-element (jtsx-treesit-node-at (point)) jsx-exp-guard))

(defun jtsx-node-jsx-context-p (node)
  "Check if NODE is JSX."
  (member (treesit-node-type node) jtsx-jsx-ts-keys))

(defun jtsx-jsx-context-at-p (position &optional jsx-exp-guard)
  "Check if inside JSX context at POSITION.
If JSX-EXP-GUARD is not nil, do not traverse jsx expression."
  (when-let ((node (jtsx-treesit-node-at position)))
    (jtsx-enclosing-jsx-node node jtsx-jsx-ts-keys nil t jsx-exp-guard)))

(defun jtsx-jsx-context-p (&optional jsx-exp-guard)
  "Check if in JSX context at point or at region ends.
If JSX-EXP-GUARD is not nil, do not traverse jsx expression."
  (and (jtsx-jsx-context-at-p (point) jsx-exp-guard)
       (or (not (region-active-p)) (jtsx-jsx-context-at-p (mark) jsx-exp-guard))))

(defun jtsx-jsx-attribute-context-at-p (position)
  "Check if inside a JSX element attribute at POSITION."
  (when-let ((node (jtsx-treesit-node-at position)))
    (when (jtsx-enclosing-jsx-node node '("jsx_attribute") nil nil t)
      t)))

(defun jtsx-jsx-attribute-context-p ()
  "Check if in JSX element attribute context at point or at region ends."
  (let ((jsx-attr-at-point (jtsx-jsx-attribute-context-at-p (point))))
    (if (not (region-active-p))
        jsx-attr-at-point
      (let ((jsx-attr-at-mark (jtsx-jsx-attribute-context-at-p (mark))))
        (cond ((and jsx-attr-at-point jsx-attr-at-mark) t)
              ((and (not jsx-attr-at-point) (not jsx-attr-at-mark)) nil)
              ;; Point and mark contexts mismatch : attribute context takes precedence as it should
              ;; rather be the expected behaviour and the comment syntax is javascript compatible.
              (t t))))))

(defun jtsx-js-nested-in-jsx-context-at-p (position)
  "Check if inside JS nested in JSX context at POSITION.

The logic is quite basic : if `jtsx-jsx-context-at-p' returns t without
`jsx-exp-guard' but nil with `jsx-exp-guard' enabled we are likely to be
inside JS nested in JSX context else not."
  (when (and (not (jtsx-jsx-context-at-p position t)) (jtsx-jsx-context-at-p position)) t))

(defun jtsx-js-nested-in-jsx-context-p ()
  "Check if inside JS nested in JSX context at point or at region ends.

If region is active, we assume that having one of the regions ends in nested JS
in JSX context means we are in nested JS in JSX context.  It enables to cover
that kind of case:
<A>{[1, 2, 3].map((val)=>{ return <B attr={val} />})}</A>."
  (or (jtsx-js-nested-in-jsx-context-at-p (point))
       (and (region-active-p) (jtsx-js-nested-in-jsx-context-at-p (mark)))))

(defun jtsx-comment-jsx-dwim (arg)
  "Comment or uncomment JSX at point or in region.
See `comment-dwim' documentation for ARG usage."
  (let ((comment-start "{/* ")
        (comment-end " */}")
        (comment-use-syntax nil)
        (comment-start-skip "\\(?:{?/\\*+\\)\\s-*")
        (comment-end-skip "\\s-*\\(\\*+/}?\\)"))
    (comment-dwim arg)))

(defun jtsx-comment-jsx-attribute-dwim (arg)
  "Comment or uncomment JSX element attribute at point or in region.
See `comment-dwim' documentation for ARG usage."
  (let ((comment-start "/* ")
        (comment-end " */")
        (comment-use-syntax nil)
        (comment-start-skip "\\(?:/\\*+\\)\\s-*")
        (comment-end-skip "\\s-*\\(\\*+/\\)"))
    (comment-dwim arg)))

(defun jtsx-comment-js-nested-in-jsx-dwim (arg)
  "Comment or uncomment JSX at point or in region.
See `comment-dwim' documentation for ARG usage."
  (let ((comment-use-syntax nil)
        (comment-start-skip "\\(?://+\\|{?/\\*+\\)\\s-*")
        (comment-end-skip "\\s-*\\(\\s>\\|\\*+/}?\\)"))
    (comment-dwim arg)))

(defun jtsx-comment-dwim (arg)
  "Add support for commenting/uncommenting inside JSX.
See `comment-dwim' documentation for ARG usage."
  (interactive "*P")
  (if (region-active-p)
      (cond
       ;; Inside JSX attribute context ?
       ((jtsx-jsx-attribute-context-p)
        (jtsx-comment-jsx-attribute-dwim arg))
       ;; Inside JS nested in JSX context ?
       ((jtsx-js-nested-in-jsx-context-p)
        (jtsx-comment-js-nested-in-jsx-dwim arg))
       ;; Inside JSX context ?
       ((jtsx-jsx-context-p) ; Do not traverse jsx expressions
        (jtsx-comment-jsx-dwim arg))
       ;; General case
       (t (comment-dwim arg)))
    (let ((position (line-end-position)))
      (cond
       ;; Inside JSX attribute context ?
       ((jtsx-jsx-attribute-context-at-p position)
        (jtsx-comment-jsx-attribute-dwim arg))
       ;; Inside JSX context ?
       ((jtsx-jsx-context-at-p position t) ; Do not traverse jsx expressions
        (jtsx-comment-jsx-dwim arg))
       ;; Inside JS nested in JSX context ?
       ((jtsx-js-nested-in-jsx-context-at-p position)
        (jtsx-comment-js-nested-in-jsx-dwim arg))
       ;; General case
       (t (comment-dwim arg))))))

(defun jtsx-jump-jsx-opening-tag ()
  "Jump to the opening tag of the JSX element."
  (interactive "^")
  (let ((enclosing-element (jtsx-enclosing-jsx-element-at-point)))
    (if enclosing-element
        (goto-char (1+ (treesit-node-start enclosing-element))) ; +1 to jump right after the "<"
      (message "No JSX opening element found"))))

(defun jtsx-jump-jsx-closing-tag ()
  "Jump to the closing tag of the JSX element."
  (interactive "^")
  (let ((enclosing-element (jtsx-enclosing-jsx-element-at-point)))
    (if enclosing-element
        (goto-char (1- (treesit-node-end enclosing-element))) ; -1 to jump right before the "/>"
      (message "No JSX closing element found"))))

(defun jtsx-jump-jsx-element-tag-dwim ()
  "Jump either to the opening or the closing tag of the JSX element."
  (interactive "^")
  (let ((enclosing-element (jtsx-enclosing-jsx-element-at-point)))
    (if enclosing-element
        (let ((start (treesit-node-start enclosing-element))
              (end (treesit-node-end enclosing-element)))
          (if (> (point) (+ start (/ (- end start) 2)))
              (jtsx-jump-jsx-opening-tag) ; We are closer to the closing tag.
            (jtsx-jump-jsx-closing-tag))) ; We are closer to the opening tag.
      (message "No JSX element found"))))

(defun jtsx-rename-jsx-identifier (node new-name &optional move-cursor)
  "Rename the NODE named tag to NEW-NAME.
If MOVE-CURSOR is t, let the cursor at the end of the insertion."
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (cl-assert (and node-start node-end))
    (let ((new-identifier-end-pos (save-excursion
                                    (delete-region node-start node-end)
                                    (goto-char node-start)
                                    (insert new-name)
                                    (point))))
      (when move-cursor
        (goto-char new-identifier-end-pos)))
    t))

(defun jtsx-rename-jsx-fragment (node new-name &optional move-cursor)
  "Rename the NODE fragment to NEW-NAME.
If MOVE-CURSOR is t, let the cursor at the end of the insertion."
  (let ((node-end (treesit-node-end node)))
    (cl-assert node-end nil "Unable to retrieve end of node")
    (let ((new-identifier-end-pos (save-excursion
                                    (goto-char (1- node-end)) ; -1 to be inside <> or </>
                                    (insert new-name)
                                    (point))))
      (when move-cursor
        (goto-char new-identifier-end-pos)))
    t))

(defun jtsx-jsx-fragment-p (node)
  "Check if NODE is a JSX fragment."
  (and (member (treesit-node-type node) jtsx-jsx-ts-element-tag-keys)
       ;; JSX Fragment tags only have 2 children : "<" (or "</") and ">".
       ;; Other JSX elemeny tags have at least one additinal child which is the identifier.
       (eq (treesit-node-child-count node) 2)))

(defun jtsx-rename-jsx-element-tag-at-point (new-name child-field-name)
  "Rename a JSX element tag to NEW-NAME at point.
CHILD-FIELD-NAME identify the tag to rename (`open_tag' or `close_tag')."
  (cl-assert (member child-field-name '("open_tag" "close_tag"))
             t "Unexpected child-field-name: %s")
  ;; Node and parent node are not passed as argument because they must be as up to
  ;; date as possible since the function alters the buffer and hense the treesit
  ;; tree.
  ;; Note that treesit parser is robust enough to be not too confused by mismaching
  ;; element tag identifiers.
  (let* ((node (jtsx-treesit-node-at (point)))
         ;; Field names can be wrong because of mismatching element tag identifiers, so
         ;; using types is safer here.
         (current-tag-node-type (treesit-node-type (treesit-node-parent node)))
         ;; We want to move the cursor only for the tag the cursor is already into.
         (move-cursor (or
                       (and (equal current-tag-node-type "jsx_opening_element")
                            (equal child-field-name "open_tag"))
                       (and (equal current-tag-node-type "jsx_closing_element")
                           (equal child-field-name "close_tag"))))
         (element-node (jtsx-enclosing-jsx-element node)))
    (cl-assert element-node nil "Unable to retrieve the enclosing jsx_element node")
    (let* ((tag-node (treesit-node-child-by-field-name element-node child-field-name))
           (fragment (jtsx-jsx-fragment-p tag-node))
           (node-to-rename (if fragment tag-node
                             ;; Get identifier node
                             (treesit-node-child-by-field-name tag-node "name"))))
      (cl-assert node-to-rename)
      (if fragment (jtsx-rename-jsx-fragment node-to-rename new-name move-cursor)
        (jtsx-rename-jsx-identifier node-to-rename new-name move-cursor)))))

(defun jtsx-rename-jsx-element (new-name)
  "Rename a JSX element to NEW-NAME at point.
Point can be in the opening or closing."
  (interactive "sRename element to: ")
  (let* ((node (jtsx-treesit-node-at (point)))
         (parent-node (treesit-node-parent node))
         (parent-node-type (treesit-node-type parent-node)))
    (unless (and (member (treesit-node-type node) '("identifier" ">"))
                 (cond ((equal parent-node-type "jsx_self_closing_element")
                        (jtsx-rename-jsx-identifier node new-name t))
                       ((member parent-node-type jtsx-jsx-ts-element-tag-keys)
                        (jtsx-rename-jsx-element-tag-at-point new-name "open_tag")
                        (jtsx-rename-jsx-element-tag-at-point new-name "close_tag"))))
      (message "No JSX element to rename"))))

(defun jtsx-treesit-syntax-error-in-descendants-p (node)
  "Check recursively if there are errors reported by treesit in NODE descendants."
  (let ((children-nodes (treesit-node-children node))
        (index 0))
    (catch 'syntax-error-found
      (while (< index (length children-nodes))
        (let ((child-node (nth index children-nodes)))
          (when (or (equal (treesit-node-type child-node) "ERROR")
                    ;; Can happen in the TSX tree-sitter parser in that situation:
                    ;; <>
                    ;;   <div
                    ;;   <p>
                    ;;   </p>
                    ;; </>
                    ;; In that case <p> is recognized as a type argument and the missing ">"
                    ;; is registered by the parser as having a start and end at the same position.
                    (and (equal (treesit-node-type child-node) ">")
                         (eq (treesit-node-start child-node)  (treesit-node-end child-node)))
                    (jtsx-treesit-syntax-error-in-descendants-p child-node))
            (throw 'syntax-error-found t))
        (setq index (1+ index)))))))

(defun jtsx-treesit-syntax-error-in-ancestors-p (node)
  "Check recursively if there are errors reported by treesit in NODE ancestors."
  (jtsx-enclosing-jsx-node node '("ERROR")))

(defun jtsx-jsx-element-tag-name (node)
  "Return the NODE tag name."
    (if-let (identifier-node (treesit-node-child-by-field-name node "name"))
      (buffer-substring-no-properties
       (treesit-node-start identifier-node)
       (treesit-node-end identifier-node))
      ""))

(defun jtsx-empty-opening-tag-name-p (opening-tag-node)
  "Return whether the OPENING-TAG-NODE has an empty tag name or not."
  (let ((identifier-node (treesit-node-child-by-field-name opening-tag-node "name")))
    (and identifier-node
         (not (eq (treesit-node-start opening-tag-node)
                  (1- (treesit-node-start identifier-node)))))))

(defun jtsx-synchronize-jsx-element-tags ()
  "Synchronize jsx element tags depending on the cursor position.

Under some circumtances, the synchronization can fail.  Right after editing a
 tag name and before synchronization, the code is syntactically wrong, thus the
 tree-sitter parser can be lost.  For example in that situation, where `A' has
 just been erased form the opening tag: < attribute=\"text\"></A>."
  (when (and jtsx-enable-jsx-element-tags-auto-sync (jtsx-command-modified-buffer-p))
    (let* ((node (jtsx-treesit-node-at (point)))
           (parent-node (treesit-node-parent node))
           (parent-node-type (treesit-node-type parent-node)))
      (when (and (member (treesit-node-type node) '("identifier" ">" "<"))
                 (member parent-node-type `(,@jtsx-jsx-ts-element-tag-keys "ERROR"))
                 ;; Syntax must be clean to prevent unexpected behaviours.
                 (not (jtsx-treesit-syntax-error-in-descendants-p parent-node))
                 (not (jtsx-treesit-syntax-error-in-ancestors-p parent-node)))
        (let* ((element-node (treesit-node-parent parent-node))
               (opening-tag-node (treesit-node-child-by-field-name element-node "open_tag"))
               (closing-tag-node (treesit-node-child-by-field-name element-node "close_tag")))
          (if (jtsx-empty-opening-tag-name-p opening-tag-node)
              ;; Handle tree-sitter edge cases < attribute></x>
              (cond ((eq (point) (1+ (treesit-node-start opening-tag-node)))
                     ;; `x' has just been erased in the opening tag and tree-sitter assumes now that
                     ;; `attribute' is the tag name even if `attribute' is preceded by some spaces.
                     ;; Here we alter this behaviour by considering `attribute' as an attribute as
                     ;; soon as it remains space right before it.
                     (jtsx-rename-jsx-element-tag-at-point "" "close_tag"))
                    ((eq (point) (1- (treesit-node-end closing-tag-node)))
                     ;; Opposite case : `x' has just been entered
                     (save-excursion (goto-char (1+ (treesit-node-start opening-tag-node)))
                                     (insert (jtsx-jsx-element-tag-name closing-tag-node)))))
            ;; General case
            (let ((opening-tag-name (jtsx-jsx-element-tag-name opening-tag-node))
                  (closing-tag-name (jtsx-jsx-element-tag-name closing-tag-node)))
              (unless (equal opening-tag-name closing-tag-name)
                (let* ((inside-opening-tag-p (equal parent-node-type "jsx_opening_element"))
                       (tag-to-rename (if inside-opening-tag-p "close_tag" "open_tag"))
                       (tag-name (if (equal tag-to-rename "open_tag")
                                     closing-tag-name
                                   opening-tag-name)))
                  (jtsx-rename-jsx-element-tag-at-point tag-name tag-to-rename))))))))))

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
 `:new-pos'."
  (let* ((current-node (jtsx-enclosing-jsx-node (jtsx-treesit-node-at (point))
                                                jtsx-jsx-ts-element-tag-keys
                                                jtsx-jsx-ts-root-keys
                                                t
                                                t)) ; Do not traverse jsx expression
         (current-node-type (treesit-node-type current-node))
         (enclosing-node (jtsx-enclosing-jsx-node current-node
                                                  jtsx-jsx-ts-root-keys
                                                  nil
                                                  t))
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
            (new-pos (if backward
                         (treesit-node-start node-final-candidate)
                       (treesit-node-end node-final-candidate))))
        (cl-assert (and node-start node-end new-pos))
        (list :node-start node-start :node-end node-end :new-pos new-pos)))))

(defun jtsx-goto-line (line)
  "Go to the beginning of LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun jtsx-bolc-at-p (pos)
  "Check if POS is at the beginning of the line content."
  (eq pos (save-excursion (goto-char pos)
                          (back-to-indentation)
                          (point))))

(defun jtsx-eol-at-p (pos)
  "Check if POS is at the end of the line."
  (eq pos (save-excursion (goto-char pos)
                          (pos-eol))))

(defun jtsx-inline-content-p (start-pos end-pos)
  "Return t if the content between START-POS and END-POS is inline.
The content is considered inline if there are some none whitespaces before
or after it."
  (or (not (jtsx-bolc-at-p start-pos)) (not (jtsx-eol-at-p end-pos))))

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
                 (new-pos (plist-get res :new-pos))
                 ;; Node is inline if after or before content on its own line
                 (inline-node (jtsx-inline-content-p node-start node-end))
                 ;; New position is inline if surrounded by content on its own line
                 (inline-new-pos (and (not (jtsx-bolc-at-p new-pos)) (not (jtsx-eol-at-p new-pos))))
                 (delete-region-start (if inline-node
                                        node-start
                                      ;; Extend region to include whitespaces and newlines
                                      (save-excursion (goto-char node-start)
                                                      (if backward
                                                          (pos-bol)
                                                        (forward-line -1) (pos-eol)))))
                 (delete-region-end (if inline-node
                                      node-end
                                    ;; Extend region to include whitespaces and newlines
                                    (save-excursion (goto-char node-end)
                                                    (when backward (forward-line 1))
                                                    (point))))
                 ;; Copy and delete region are usefull when we want to delete whitespaces and
                 ;; newlines but we do not want to paste them (eg inline new position)
                 (copy-region-start (if inline-new-pos node-start delete-region-start))
                 (copy-region-end (if inline-new-pos node-end delete-region-end))
                 (cursor-rel-pos (- (point) copy-region-start)))
            (copy-region-as-kill copy-region-start copy-region-end)
            (let ((del-region (lambda () (delete-region delete-region-start delete-region-end))))
              (when backward (funcall del-region))
              (goto-char new-pos)
              (yank '(1))
              (unless backward (funcall del-region)))
            (forward-char cursor-rel-pos) ; Try to prevent undesired cursor movements
            (indent-region (save-excursion (goto-char (if backward new-pos delete-region-start))
                                           (pos-bol))
                           (save-excursion (goto-char (if backward delete-region-end new-pos))
                                           (pos-eol))))
        (message "No move in this direction"))
    (message "Not inside jsx context")))

(defun jtsx-move-jsx-element-tag-forward ()
  "Move a JSX element tag (opening or closing) forward."
  (interactive)
  (jtsx-move-jsx-element nil nil))

(defun jtsx-move-jsx-element-tag-backward ()
  "Move a JSX element tag (opening or closing) backward."
  (interactive)
  (jtsx-move-jsx-element nil t))

(defun jtsx-move-jsx-element-forward ()
  "Move a JSX element (or any JSX node) forward."
  (interactive)
  (jtsx-move-jsx-element t nil))

(defun jtsx-move-jsx-element-backward ()
  "Move a JSX element (or any JSX node) backward."
  (interactive)
  (jtsx-move-jsx-element t t))

(defun jtsx-move-jsx-element-step-in-forward ()
  "Move a JSX element (or any JSX node) forward.
Step into sibling elements if possible."
  (interactive)
  (jtsx-move-jsx-element t nil t))

(defun jtsx-move-jsx-element-step-in-backward ()
  "Move a JSX element (or any JSX node) backward.
Step into sibling elements if possible."
  (interactive)
  (jtsx-move-jsx-element t t t))

(defun jtsx-jsx-electric-closing-element (n)
  "Insert `>' and the associated closing tag (`</xxx>') if expected.
N is a numeric prefix argument.  If greater than 1, insert N times `>', but
 never insert closing tag."
  (interactive "p")
  (insert-char (char-from-name "GREATER-THAN SIGN") n t)
  ;; Check jsx context inside the tag
  (when (and (= n 1) jtsx-enable-jsx-electric-closing-element (jtsx-jsx-context-at-p (1- (point))))
    (when-let ((node (jtsx-treesit-node-at (1- (point))))) ; Safer to get node inside the tag
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
            ;; This logic is quite basic, but not sure we can really do better with treesit
            ;; informations about syntax issues.
            (when (jtsx-treesit-syntax-error-in-ancestors-p node)
              (save-excursion (insert closing-tag)))))))))

(defun jtsx-inside-empty-inline-jsx-element-p ()
  "Return t if inside an empty inline jsx element.
For example:
<></>
<A></A>
<A
  attribute
></A>"
  (when (jtsx-jsx-context-p)
    (when-let* ((node (jtsx-treesit-node-at (point))))
      (when (equal (treesit-node-type node) "</")
        (when-let* ((element-node (jtsx-enclosing-jsx-element node))
                    (open-tag (treesit-node-child-by-field-name element-node "open_tag"))
                    (close-tag (treesit-node-child-by-field-name element-node "close_tag"))
                    (open-tag-end-line (line-number-at-pos (treesit-node-end open-tag)))
                    (close-tag-start-line (line-number-at-pos (treesit-node-start close-tag)))
                    ;; The function is called after newline insertion, so close tag is one line
                    ;; after the opening one
                    (inline-element-node (eq (- close-tag-start-line open-tag-end-line) 1)))
          ;; Check that the element has no children others than open and close tag
          (eq (treesit-node-child-count element-node) 2))))))

(defun jtsx-electric-open-newline-between-jsx-element-tags-psif ()
  "Honor `jtsx-enable-electric-open-newline-between-jsx-element-tags'.
Member of `post-self-insert-hook'."
  (when (and jtsx-enable-electric-open-newline-between-jsx-element-tags
             (eq last-command-event ?\n)
             (jtsx-jsx-context-p)
             (jtsx-inside-empty-inline-jsx-element-p))
    (save-excursion (newline 1 t))))

(defun jtsx-trimmed-region ()
  "Return the trimmed region as a plist.
Keys are `:start' and `:end'."
  (let* ((start-pos (min (point) (mark)))
         (end-pos (max (point) (mark)))
         (trimmed-start-pos (save-excursion
                              (goto-char start-pos)
                              (jtsx-goto-content-forward)
                              (point)))
         (trimmed-end-pos (save-excursion
                            (goto-char end-pos)
                            (jtsx-goto-content-backward)
                            (point))))
    (if (< trimmed-start-pos trimmed-end-pos)
        `(:start ,trimmed-start-pos :end ,trimmed-end-pos)
      ;; Something is going wrong, fallback to initial region
      `(:start ,start-pos :end ,end-pos))))

(defun jtsx-region-to-wrap ()
  "Return the expected region to be wrapped as a plist.
Keys are `:start' and `:end'."
  (let* ((region (if (region-active-p) (jtsx-trimmed-region) `(:start ,(point) :end ,(point))))
         (start-pos (plist-get region :start))
         (end-pos (plist-get region :end))
         (start-element (jtsx-enclosing-jsx-node (jtsx-treesit-node-at start-pos)
                                                 jtsx-jsx-ts-root-keys nil t))
         (end-element (if (region-active-p)
                          (jtsx-enclosing-jsx-node (jtsx-treesit-node-at end-pos)
                                                   jtsx-jsx-ts-root-keys nil t)
                        start-element))
         (start-element-type (treesit-node-type start-element))
         (end-element-type (treesit-node-type end-element)))
    (cl-assert (and start-element end-element))
    (if (and
         (region-active-p)
         (equal start-element-type "jsx_text")
         (equal end-element-type "jsx_text"))
        ;; Handle specific case: selection inside a text node (eg to wrap a text with `strong'
        ;; tags)
        `(:start ,start-pos :end ,end-pos)
      ;; General case: use treesit tree to get or adjust the expected region to be wrapped
      `(:start ,(treesit-node-start start-element) :end ,(treesit-node-end end-element)))))

(defun jtsx-wrap-in-jsx-element (element-name)
  "Wrap JSX nodes in a JSX element.
Nodes are selected by a region if there is an active one.  Else the node at
 point is used.
ELEMENT-NAME is the name of the new wrapping element."
  (interactive "sJSX element name: ")
  (if (jtsx-jsx-context-p)
      (let* ((region-to-wrap (jtsx-region-to-wrap))
             (start-pos (plist-get region-to-wrap :start))
             (end-pos (plist-get region-to-wrap :end))
             (new-cursor-pos nil)
             (inline-element (jtsx-inline-content-p start-pos end-pos))
             (opening-line (line-number-at-pos start-pos))
             (closing-line (+ (line-number-at-pos end-pos)
                              (if inline-element 0 1))) ; +1 for insertion if not inline
             (opening-tag (format "<%s>" element-name))
             (closing-tag (format "</%s>" element-name)))
        (save-excursion
          (if inline-element (goto-char end-pos) (jtsx-goto-line closing-line))
          (insert closing-tag)
          (if (not inline-element) (newline))
          (if inline-element (goto-char start-pos) (jtsx-goto-line opening-line))
          (insert opening-tag)
          (setq new-cursor-pos (1- (point)))
          (if (not inline-element) (newline)))
        ;; Let the cursor ready to add attributes in the wrapping element
        (goto-char new-cursor-pos)
        ;; Finally indent modified region
        (indent-region (save-excursion (jtsx-goto-line opening-line) (pos-bol))
                       (save-excursion (jtsx-goto-line (+ closing-line (if inline-element 0 1)))
                                       (pos-eol))))
    (message "Not inside jsx context")))

(defun jtsx-unwrap-jsx ()
  "Unwrap JSX nodes wrapped in the node at point."
  (interactive)
  (if (jtsx-jsx-context-p)
      (if-let ((node (jtsx-enclosing-jsx-element-at-point t)))
          (let* ((opening-tag (treesit-node-child-by-field-name node "open_tag"))
                 (closing-tag (treesit-node-child-by-field-name node "close_tag"))
                 (opening-start-pos (treesit-node-start opening-tag))
                 (opening-end-pos (treesit-node-end opening-tag))
                 (closing-start-pos (treesit-node-start closing-tag))
                 (closing-end-pos (treesit-node-end closing-tag)))
            (cl-assert (and opening-start-pos
                            opening-end-pos
                            closing-start-pos
                            closing-end-pos))
            (let* ((inline-opening-tag (jtsx-inline-content-p opening-start-pos opening-end-pos))
                   (inline-closing-tag (jtsx-inline-content-p closing-start-pos closing-end-pos))
                   (final-opening-start-pos (if inline-opening-tag
                                                opening-start-pos
                                              (save-excursion (goto-char opening-start-pos)
                                                              (forward-line -1)
                                                              (pos-eol))))
                   (final-opening-end-pos opening-end-pos)
                   (final-closing-start-pos closing-start-pos)
                   (final-closing-end-pos (if inline-closing-tag
                                              closing-end-pos
                                            (save-excursion (goto-char closing-end-pos)
                                                            (forward-line 1)
                                                            (point))))
                   (wrapped-region-size (- final-closing-start-pos final-opening-end-pos)))
            (delete-region final-closing-start-pos final-closing-end-pos)
            (delete-region final-opening-start-pos final-opening-end-pos)
            (indent-region final-opening-start-pos (+ final-opening-start-pos
                                                      wrapped-region-size))
            (goto-char (1+ opening-start-pos)))) ; +1 to ensure to be inside the children
        (message "Not able to retrieve the wrapping node"))
    (message "Not inside jsx context")))

(defun jtsx-delete-jsx-node ()
  "Delete a JSX node at point and its children."
  (interactive)
  (if (jtsx-jsx-context-p)
      (if-let ((node (jtsx-enclosing-jsx-node (jtsx-treesit-node-at (point))
                                              jtsx-jsx-ts-root-keys
                                              nil
                                              t)))
          (let ((start-pos (treesit-node-start node))
                (end-pos (treesit-node-end node)))
            (cl-assert (and start-pos end-pos))
            ;; Use kill-region to save the content into the clipboard.
            (kill-region start-pos end-pos))
        (message "Not able to retrieve the node to delete"))
    (message "Not inside jsx context")))

(defun jtsx-delete-jsx-attribute ()
  "Delete a JSX attribute at point."
  (interactive)
  (if (jtsx-jsx-context-p)
      (if-let ((node (jtsx-enclosing-jsx-node (jtsx-treesit-node-at (point))
                                              '("jsx_attribute")
                                              nil
                                              t)))
          (let* ((start-pos (treesit-node-start node))
                 (end-pos (treesit-node-end node))
                 (previous-node (treesit-node-prev-sibling node))
                 (prev-node-end-pos (treesit-node-end previous-node)))
            (cl-assert (and start-pos end-pos))
            (cl-assert (and prev-node-end-pos))
            (let ((effective-start-pos (if (eq (line-number-at-pos start-pos)
                                               (line-number-at-pos prev-node-end-pos))
                                           ;; Remove the previous space
                                           prev-node-end-pos
                                         start-pos)))
              ;; Use kill-region to save the content into the clipboard.
              (kill-region effective-start-pos end-pos)))
        (message "Not able to retrieve the node to delete"))
    (message "Not inside jsx context")))

(defun jtsx-guess-jsx-attributes-new-orientation (element)
  "Try to guess the new expected oriention of JSX ELEMENT attributes.
Returns either horizontal or vertical symbol."
  (let* ((element-children (treesit-filter-child
                            element
                            (lambda (node)
                              (not (member (treesit-node-type node) '("<"))))))
         (separator-count (1- (length element-children)))
         (newline-count 0)
         (index 0))
    (while (< index separator-count)
      (let* ((current-child (nth index element-children))
             (next-child (nth (1+ index) element-children))
             (sep-start (treesit-node-end current-child))
             (sep-end (treesit-node-start next-child)))
        (cl-assert (and sep-start sep-end))
        (when (save-excursion
              (goto-char sep-start)
              (re-search-forward "\n" sep-end t))
          (setq newline-count (1+ newline-count)))
        (setq index (1+ index))))
    (if (< separator-count (* newline-count 2))
        ;; A majority of separators are newlines, the expected new orientation is horizontal
        'horizontal
      ;; A majority of separators are spaces, the expected new orientation is vertical
      'vertical)))

(defun jtsx-rearrange-jsx-attributes-core (element separator &optional skip-last-sep)
  "Rearrange JSX ELEMENT attributes using SEPARATOR.
If SKIP-LAST-SEP is not nil, SEPARATOR will not be inserted before the last
ELEMENT child."
  (cl-assert (member separator '(" " "\n")))
  (let* ((element-children (treesit-filter-child
                            element
                            (lambda (node)
                              (not (member (treesit-node-type node) '("<"))))))
         (element-count (length element-children))
         (index 0)
         (rearranged-region "")
         (new-position (point)))
    (cl-assert (> element-count 1)) ; At least one attribute and >
    ;; Generate the rearranged attributes
    (while (< index element-count)
      (let* ((child (nth index element-children))
             (child-start (treesit-node-start child))
             (child-end (treesit-node-end child)))
        (cl-assert (and child-start child-end))
        ;; Save cursor position if relevant
        (if (and (>= (point) child-start) (<= (point) child-end))
            (let ((first-child-pos (treesit-node-start (car element-children)))
                  (position-offset-in-current-child (- (point) child-start)))
              (cl-assert first-child-pos)
              (setq new-position (+ first-child-pos
                                    (length rearranged-region)
                                    position-offset-in-current-child
                                    (length separator)))))
        ;; Increment the rearranged region
        (setq rearranged-region (concat
                                 rearranged-region
                                 (if (or (= index 0)
                                         (and (= index (1- element-count)) skip-last-sep))
                                     ""
                                   separator)
                                 (buffer-substring-no-properties child-start child-end)))
        (setq index (1+ index))))
    ;; Replace old attributes by the rearranged ones and reindent
    (let ((region-start (treesit-node-start (nth 0 element-children)))
          (region-end (treesit-node-end (nth (1- element-count) element-children))))
      (cl-assert (and region-start region-end))
      (delete-region region-start region-end)
      (goto-char region-start)
      (insert rearranged-region)
      (goto-char new-position) ; Try keep cursor position consistent
      (indent-region region-start (+ region-start (length rearranged-region))))))

(defun jtsx-rearrange-jsx-attributes (&optional orientation)
  "Rearrange the orientation of JSX attributes.
ORIENTATION is horizontal or vertical symbol.  If it is nil, a smart detection
of the new expected orientation is performed."
  (if (jtsx-jsx-context-p)
      (if-let* ((root-element (jtsx-enclosing-jsx-node
                               (jtsx-treesit-node-at (point))
                               '("jsx_element" "jsx_self_closing_element"))))
          (let ((element (if (equal (treesit-node-type root-element) "jsx_element")
                             (treesit-node-child-by-field-name root-element "open_tag")
                           root-element)))
            (cl-assert element nil "Not able to retieve the opening tag")
            (if-let ((attributes (treesit-filter-child
                                  element
                                  (lambda (node) (equal (treesit-node-type node)
                                                        "jsx_attribute")))))
                (pcase (or orientation (jtsx-guess-jsx-attributes-new-orientation element))
                  ('horizontal (jtsx-rearrange-jsx-attributes-core element
                                                               " "
                                                               (equal (treesit-node-type element)
                                                                      "jsx_opening_element")))
                  ('vertical (jtsx-rearrange-jsx-attributes-core element "\n"))
                  (_ (error "Unknown orientation")))
              (message "No attribute found")))
        (message "Not inside a jsx element"))
    (message "Not inside jsx context")))

(defun jtsx-toggle-jsx-attributes-orientation ()
  "Toggle the orientation of JSX attributes."
  (interactive)
  (jtsx-rearrange-jsx-attributes))

(defun jtsx-rearrange-jsx-attributes-horizontally ()
  "Rearrange the orientation of JSX attributes horizontally."
  (interactive)
  (jtsx-rearrange-jsx-attributes 'horizontal))

(defun jtsx-rearrange-jsx-attributes-vertically ()
  "Rearrange the orientation of JSX attributes vertically."
  (interactive)
  (jtsx-rearrange-jsx-attributes 'vertical))

(defun jtsx-forward-sexp-base (&optional arg interactive)
  "ARG INTERACTIVE."
  (if (or
       ;; Starting Emacs 30, treesit set its own function, which has
       ;; some issues. Bug report:
       ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66988 Use the
       ;; default one instead.
       (>= 30 emacs-major-version)
       ;; Prevent recursive call
       (eq forward-sexp-function #'jtsx-forward-sexp))
      (let ((forward-sexp-function nil))
        (forward-sexp arg interactive))
    (forward-sexp arg interactive)))

(defun jtsx-forward-sexp (&optional arg interactive)
  "Make `forward-sexp' compatible with Hideshow in JSX.
See `forward-sexp' documentation for informations about ARG and
INTERACTIVE arguments."
  (interactive "^p\nd")
  (when (or (eq arg 0)
            (and (> arg 0) (eq (point) (point-max)))
            (and (< arg 0) (eq (point) (point-min))))
    ;; Let base function handle these unexpected cases
    (jtsx-forward-sexp-base arg interactive))
  (let* ((index (abs arg))
         (step (/ arg index)))
    (while (> index 0)
      (let* ((pos (save-excursion (jtsx-goto-none-empty-content arg)
                                  (point)))
             (node (jtsx-treesit-node-at (- pos (if (> arg 0) 0 1))))
             (node-type (treesit-node-type node))
             (parent-node (treesit-node-parent node))
             (parent-node-type (treesit-node-type parent-node)))
        (cond (;; Handle going over a JSX element
               (or (and (> arg 0)
                        (equal node-type "<")
                        (member parent-node-type '("jsx_opening_element"
                                                   "jsx_self_closing_element")))
                   (and (< arg 0)
                        (member node-type '(">" "/>"))
                        (member parent-node-type '("jsx_closing_element"
                                                   "jsx_self_closing_element"))))
               (let* ((enclosing-node (if (equal parent-node-type "jsx_self_closing_element")
                                          parent-node
                                        (treesit-node-parent parent-node)))
                      (new-pos (if (> arg 0)
                                   (treesit-node-end enclosing-node)
                                 (treesit-node-start enclosing-node))))
                 (cl-assert new-pos)
                 (goto-char new-pos)))
              (;; Handle going out JSX element children: no next sexp
               (or (and (> arg 0)
                        (equal node-type "</")
                        (equal parent-node-type "jsx_closing_element"))
                   (and (< arg 0)
                        (equal node-type ">")
                        (equal parent-node-type "jsx_opening_element")))
               (user-error (if (> arg 0) "No next sexp" "No previous sexp")))
              (;; Handle going out of JSX opening and (self) closing element
               (or (and (> arg 0)
                        (member node-type '(">" "/>"))
                        (member parent-node-type '("jsx_closing_element"
                                                   "jsx_self_closing_element")))
                   (and (< arg 0)
                        (member node-type '("<" "</"))
                        (member parent-node-type '("jsx_opening_element"
                                                   "jsx_self_closing_element"))))
               (let ((new-pos (if (> arg 0)
                                  (treesit-node-end parent-node)
                                (treesit-node-start parent-node))))
                 (cl-assert new-pos)
                 (goto-char new-pos)))
              (t
               ;; Other cases: use base function
               (jtsx-forward-sexp-base step interactive)))
        (setq index (1- index))))))

(define-obsolete-function-alias 'jtsx-hs-forward-sexp 'jtsx-forward-sexp "jtsx 0.5.1")

(defun jtsx-backward-up-list
    (&optional arg escape-strings no-syntax-crossing should-push-mark)
  "Adding `backward-up-list' support when inside JSX block.
If SHOULD-PUSH-MARK is non-nil (as it is interactively), call
`push-mark' before moving point to another position.
Note that ARG is ignored inside JSX context.  For ESCAPE-STRINGS and
NO-SYNTAX-CROSSING, Please see `backward-up-list'."
  (interactive "^p\nd\nd\nd")
  (if (jtsx-jsx-context-p)
      (when-let* ((node (treesit-node-at (point)))
                  (enclosing-node
                   (jtsx-enclosing-jsx-node node '("jsx_element"
                                                   "jsx_self_closing_element")))
                  (parent-node (treesit-node-parent enclosing-node))
                  (start-pos (treesit-node-start parent-node)))
        (when should-push-mark (push-mark nil t nil))
        (goto-char start-pos))
    (let ((orig-pos (point)))
      (condition-case nil
          (progn
            (backward-up-list arg escape-strings no-syntax-crossing)
            (when should-push-mark (push-mark orig-pos t nil)))
        ((scan-error user-error)
         (goto-char orig-pos))))))

(defun jtsx-hs-looking-at-block-start-p ()
  "Return non-nil if the point is at the block start."
  (and (looking-at hs-block-start-regexp)
       (not (equal (treesit-node-type (treesit-node-at (point))) "comment"))))

(defun jtsx-hs-find-block-beginning ()
  "Enhance `hs-find-block-beginning' for JSX."
  (if-let* ((enclosing-node
             (jtsx-enclosing-jsx-node (jtsx-treesit-node-at (point)) jtsx-hs-block-keys))
            (start-pos (treesit-node-start enclosing-node)))
      (goto-char start-pos)
    ;; Use hideshow default function if something goes wrong
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

(defun jtsx-customize-indent-rules (ts-lang-key indent-var-name)
  "Customize treesit indent rules for TS-LANG-KEY language.
INDENT-VAR-NAME is the name of the indent offset variable."
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
  (setq-local treesit-simple-indent-rules jtsx-ts-indent-rules))

(defun jtsx-configure-mode-base (mode
                                 mode-map
                                 ts-lang-key
                                 indent-var-name)
  "Base function for JSX/TSX Major mode configuration.
MODE, MODE-MAP, TS-LANG-KEY, INDENT-VAR-NAME variables allow customization
 depending on the mode."
  ;; Customize indentation
  (jtsx-customize-indent-rules ts-lang-key indent-var-name)

  ;; Use maximum level of syntax highlighting if enabled
  (when jtsx-enable-all-syntax-highlighting-features
    (setq-local treesit-font-lock-level 4))

  ;; Apply treesit customization
  (treesit-major-mode-setup)

  ;; Bind keys
  (define-key mode-map [remap comment-dwim] 'jtsx-comment-dwim)
  (define-key mode-map ">" #'jtsx-jsx-electric-closing-element)

  ;; Add hook for electric new line
  (add-hook 'post-self-insert-hook #'jtsx-electric-open-newline-between-jsx-element-tags-psif nil t)

  ;; Add hook to save the value of `jtsx-save-buffer-chars-modified-tick'
  (add-hook 'pre-command-hook #'jtsx-save-buffer-chars-modified-tick nil t)

  ;; Add hook for automatic synchronization of jsx element tags.
  ;; `DEPTH' value explanation: some completion packages rely on `buffer-chars-modified-tick'
  ;; function to check if completion process is outdated. `jtsx-synchronize-jsx-element-tags' can
  ;; alter `buffer-chars-modified-tick', so it should occur before the completion process starts,
  ;; in order not to conflict with it.
  ;; As soon as `DEPTH' is below to 0 (default value), this is enough to run the hook prior to any
  ;; other `post-command-hook' that have a default `DEPTH' value. This seems to be the case for many
  ;; popular completion packages : `company-mode', `corfu', `vertico', `auto-complete'.
  (add-hook 'post-command-hook #'jtsx-synchronize-jsx-element-tags -1 t)

  ;; Use jtsx-forward-sexp
  (setq-local forward-sexp-function #'jtsx-forward-sexp)

  ;; JSX folding with Hideshow
  (add-to-list 'hs-special-modes-alist
               `(,mode
                 "{\\|(\\|[[]\\|\\(?:<>\\)\\|<[^/>][^>]*>"
                 "}\\|)\\|[]]\\|</[^>]*>"
                 ;; "/[*/]"
                 "\\({/[/*]\\)\\|\\(/[/*]\\)"
                 jtsx-forward-sexp
                 nil
                 jtsx-hs-find-block-beginning
                 nil
                 jtsx-hs-looking-at-block-start-p)))

(defun jtsx-font-lock-compatibility-function-expression (ts-lang-key)
  "Handle tree-sitter grammar breaking change for `function' expression.

TS-LANG-KEY can be `javascript', `typescript' or `tsx'.  Starting from
version 0.20.2 of the javascript grammar and version 0.20.4 of the
typescript/tsx grammar, `function' becomes `function_expression'."
  (condition-case nil
      (progn (treesit-query-capture ts-lang-key '((function_expression) @cap))
             ;; New version of the grammar
             'function_expression)
    (treesit-query-error
    ;; Old version of the grammar
    'function)))

;; Modified copy from lisp/progmodes/js.el of Emacs sources.
;; Hard code javascript font lock settings to back port some fixes into Emacs 29.1 and 29.2.
;; Should not be used for later versions.
(defun jtsx-jsx-mode-font-lock-settings ()
  "Tree-sitter font-lock settings for javascript."
  (let ((func-exp (jtsx-font-lock-compatibility-function-expression 'javascript)))
    (treesit-font-lock-rules
     :language 'javascript
     :feature 'comment
     '([(comment) (hash_bang_line)] @font-lock-comment-face)

     :language 'javascript
     :feature 'constant
     '(((identifier) @font-lock-constant-face
        (:match "\\`[A-Z_][0-9A-Z_]*\\'" @font-lock-constant-face))

       [(true) (false) (null)] @font-lock-constant-face)

     :language 'javascript
     :feature 'keyword
     `([,@js--treesit-keywords] @font-lock-keyword-face
       [(this) (super)] @font-lock-keyword-face)

     :language 'javascript
     :feature 'string
     '((regex pattern: (regex_pattern)) @font-lock-regexp-face
       (string) @font-lock-string-face)

     :language 'javascript
     :feature 'string-interpolation
     :override t
     '((template_string) @js--fontify-template-string
       (template_substitution ["${" "}"] @font-lock-misc-punctuation-face))

     :language 'javascript
     :feature 'definition
     `((,func-exp
        name: (identifier) @font-lock-function-name-face)

       (class_declaration
        name: (identifier) @font-lock-type-face)

       (function_declaration
        name: (identifier) @font-lock-function-name-face)

       (method_definition
        name: (property_identifier) @font-lock-function-name-face)

       (formal_parameters
        [(identifier) @font-lock-variable-name-face
         (array_pattern (identifier) @font-lock-variable-name-face)
         (object_pattern (shorthand_property_identifier_pattern) @font-lock-variable-name-face)])

       (variable_declarator
        name: (identifier) @font-lock-variable-name-face)

       (variable_declarator
        name: (identifier) @font-lock-function-name-face
        value: [(,func-exp) (arrow_function)])

       (variable_declarator
        name: [(array_pattern (identifier) @font-lock-variable-name-face)
               (object_pattern
                (shorthand_property_identifier_pattern) @font-lock-variable-name-face)])

       ;; full module imports
       (import_clause (identifier) @font-lock-variable-name-face)
       ;; named imports with aliasing
       (import_clause (named_imports (import_specifier
                                      alias: (identifier) @font-lock-variable-name-face)))
       ;; named imports without aliasing
       (import_clause (named_imports (import_specifier
                                      !alias
                                      name: (identifier) @font-lock-variable-name-face)))

       ;; full namespace import (* as alias)
       (import_clause (namespace_import (identifier) @font-lock-variable-name-face)))

     :language 'javascript
     :feature 'assignment
     '((assignment_expression
        left: (_) @js--treesit-fontify-assignment-lhs))

     :language 'javascript
     :feature 'function
     '((call_expression
        function: [(identifier) @font-lock-function-call-face
                   (member_expression
                    property:
                    (property_identifier) @font-lock-function-call-face)]))

     :language 'javascript
     :feature 'jsx
     '((jsx_opening_element name: (_) @font-lock-function-call-face)
       (jsx_closing_element name: (_) @font-lock-function-call-face)
       (jsx_self_closing_element name: (_) @font-lock-function-call-face)
       (jsx_attribute (property_identifier) @font-lock-constant-face))

     :language 'javascript
     :feature 'property
     '(((property_identifier) @font-lock-property-use-face)
       (pair value: (identifier) @font-lock-variable-use-face)
       ((shorthand_property_identifier) @font-lock-property-use-face))

     :language 'javascript
     :feature 'number
     '((number) @font-lock-number-face
       ((identifier) @font-lock-number-face
        (:match "\\`\\(?:NaN\\|Infinity\\)\\'" @font-lock-number-face)))

     :language 'javascript
     :feature 'operator
     `([,@js--treesit-operators] @font-lock-operator-face
       (ternary_expression ["?" ":"] @font-lock-operator-face))

     :language 'javascript
     :feature 'bracket
     '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

     :language 'javascript
     :feature 'delimiter
     '((["," "." ";" ":"]) @font-lock-delimiter-face)

     :language 'javascript
     :feature 'escape-sequence
     :override t
     '((escape_sequence) @font-lock-escape-face))))

;; Copy from lisp/progmodes/typescript-ts-mode.el of Emacs sources.
(defun jtsx-tsx-mode-font-lock-compatibility-bb1f97b (language)
  "Font lock rules helper, to handle different releases of tree-sitter-tsx.
Check if a node type is available, then return the right font lock rules.
Argument LANGUAGE is either `typescript' or `tsx'."
  ;; handle commit bb1f97b
  ;; Warning: treesitter-query-capture says both node types are valid,
  ;; but then raises an error if the wrong node type is used. So it is
  ;; important to check with the new node type (member_expression)
  (condition-case nil
      (progn (treesit-query-capture language '((jsx_opening_element (member_expression) @capture)))
             '((jsx_opening_element
                [(member_expression (identifier)) (identifier)]
                @typescript-ts-jsx-tag-face)

               (jsx_closing_element
                [(member_expression (identifier)) (identifier)]
                @typescript-ts-jsx-tag-face)

               (jsx_self_closing_element
                [(member_expression (identifier)) (identifier)]
                @typescript-ts-jsx-tag-face)))
    (treesit-query-error
     '((jsx_opening_element
        [(nested_identifier (identifier)) (identifier)]
        @typescript-ts-jsx-tag-face)

       (jsx_closing_element
        [(nested_identifier (identifier)) (identifier)]
        @typescript-ts-jsx-tag-face)

       (jsx_self_closing_element
        [(nested_identifier (identifier)) (identifier)]
        @typescript-ts-jsx-tag-face)))))

;; Modified copy from lisp/progmodes/typescript-ts-mode.el of Emacs sources.
;; Hard code typescript/tsx font lock settings to back port some fixes into Emacs 29.1 and 29.2.
;; Should not be used for later versions.
(defun jtsx-tsx-mode-font-lock-settings (language)
  "Tree-sitter font-lock settings.
Argument LANGUAGE is either `typescript' or `tsx'."
  (let ((func-exp (jtsx-font-lock-compatibility-function-expression language)))
    (treesit-font-lock-rules
     :language language
     :feature 'comment
     `([(comment) (hash_bang_line)] @font-lock-comment-face)

     :language language
     :feature 'constant
     `(((identifier) @font-lock-constant-face
        (:match "\\`[A-Z_][0-9A-Z_]*\\'" @font-lock-constant-face))
       [(true) (false) (null)] @font-lock-constant-face)

     :language language
     :feature 'keyword
     `([,@typescript-ts-mode--keywords] @font-lock-keyword-face
       [(this) (super)] @font-lock-keyword-face)

     :language language
     :feature 'string
     `((regex pattern: (regex_pattern)) @font-lock-regexp-face
       (string) @font-lock-string-face
       (template_string) @js--fontify-template-string
       (template_substitution ["${" "}"] @font-lock-misc-punctuation-face))

     :language language
     :override t ;; for functions assigned to variables
     :feature 'declaration
     `((,func-exp
        name: (identifier) @font-lock-function-name-face)
       (function_declaration
        name: (identifier) @font-lock-function-name-face)
       (function_signature
        name: (identifier) @font-lock-function-name-face)

       (method_definition
        name: (property_identifier) @font-lock-function-name-face)
       (method_signature
        name: (property_identifier) @font-lock-function-name-face)
       (required_parameter (identifier) @font-lock-variable-name-face)
       (optional_parameter (identifier) @font-lock-variable-name-face)

       (variable_declarator
        name: (identifier) @font-lock-function-name-face
        value: [(,func-exp) (arrow_function)])

       (variable_declarator
        name: (identifier) @font-lock-variable-name-face)

       (enum_declaration (identifier) @font-lock-type-face)

       (extends_clause value: (identifier) @font-lock-type-face)
       ;; extends React.Component<T>
       (extends_clause value: (member_expression
                               object: (identifier) @font-lock-type-face
                               property: (property_identifier) @font-lock-type-face))

       (arrow_function
        parameter: (identifier) @font-lock-variable-name-face)

       (variable_declarator
        name: (array_pattern
               (identifier)
               (identifier) @font-lock-function-name-face)
        value: (array (number) (,func-exp)))

       (catch_clause
        parameter: (identifier) @font-lock-variable-name-face)

       ;; full module imports
       (import_clause (identifier) @font-lock-variable-name-face)
       ;; named imports with aliasing
       (import_clause (named_imports (import_specifier
                                      alias: (identifier) @font-lock-variable-name-face)))
       ;; named imports without aliasing
       (import_clause (named_imports (import_specifier
                                      !alias
                                      name: (identifier) @font-lock-variable-name-face)))

       ;; full namespace import (* as alias)
       (import_clause (namespace_import (identifier) @font-lock-variable-name-face)))

     :language language
     :feature 'identifier
     `((nested_type_identifier
        module: (identifier) @font-lock-type-face)

       (type_identifier) @font-lock-type-face

       (predefined_type) @font-lock-type-face

       (new_expression
        constructor: (identifier) @font-lock-type-face)

       (enum_body (property_identifier) @font-lock-type-face)

       (enum_assignment name: (property_identifier) @font-lock-type-face)

       (variable_declarator
        name: (identifier) @font-lock-variable-name-face)

       (for_in_statement
        left: (identifier) @font-lock-variable-name-face)

       (arrow_function
        parameters:
        [(_ (identifier) @font-lock-variable-name-face)
         (_ (_ (identifier) @font-lock-variable-name-face))
         (_ (_ (_ (identifier) @font-lock-variable-name-face)))]))

     :language language
     :feature 'property
     `((property_signature
        name: (property_identifier) @font-lock-property-name-face)
       (public_field_definition
        name: (property_identifier) @font-lock-property-name-face)

       (pair key: (property_identifier) @font-lock-property-use-face)

       ((shorthand_property_identifier) @font-lock-property-use-face))

     :language language
     :feature 'expression
     `((assignment_expression
        left: [(identifier) @font-lock-function-name-face
               (member_expression
                property: (property_identifier) @font-lock-function-name-face)]
        right: [(,func-exp) (arrow_function)]))

     :language language
     :feature 'function
     '((call_expression
        function:
        [(identifier) @font-lock-function-call-face
         (member_expression
          property: (property_identifier) @font-lock-function-call-face)]))

     :language language
     :feature 'pattern
     `((pair_pattern
        key: (property_identifier) @font-lock-property-use-face
        value: [(identifier) @font-lock-variable-name-face
                (assignment_pattern left: (identifier) @font-lock-variable-name-face)])

       (array_pattern (identifier) @font-lock-variable-name-face)

       ((shorthand_property_identifier_pattern) @font-lock-variable-name-face))

     :language language
     :feature 'jsx
     (append (jtsx-tsx-mode-font-lock-compatibility-bb1f97b language)
             `((jsx_attribute (property_identifier) @typescript-ts-jsx-attribute-face)))

     :language language
     :feature 'number
     `((number) @font-lock-number-face
       ((identifier) @font-lock-number-face
        (:match "\\`\\(?:NaN\\|Infinity\\)\\'" @font-lock-number-face)))

     :language language
     :feature 'operator
     `([,@typescript-ts-mode--operators] @font-lock-operator-face
       (ternary_expression ["?" ":"] @font-lock-operator-face))

     :language language
     :feature 'bracket
     '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

     :language language
     :feature 'delimiter
     '((["," "." ";" ":"]) @font-lock-delimiter-face)

     :language language
     :feature 'escape-sequence
     :override t
     '((escape_sequence) @font-lock-escape-face))))

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

(defun jtsx-define-obsolete-mode-variable-alias (obsolete-mode current-mode when var-suffix)
  "Create an alias for a variable of CURRENT-MODE and mark it obsolete.
The alias name format is :  [OBSOLETE-MODE]-[var-suffix].
WHEN indicates when the variable starts to be obsolete."
  (let ((append-to-symb (lambda (symb suffix) (intern (concat (symbol-name symb) "-" suffix)))))
    (define-obsolete-variable-alias
      (funcall append-to-symb obsolete-mode var-suffix)
      (funcall append-to-symb current-mode var-suffix)
      when)))

(defun jtsx-define-obsolete-mode-alias (obsolete-mode current-mode when)
  "Create OBSOLETE-MODE as an alias to CURRENT-MODE and mark it obsolete.
WHEN indicates when the mode starts to be obsolete."
  (define-obsolete-function-alias obsolete-mode current-mode when)
  (jtsx-define-obsolete-mode-variable-alias obsolete-mode current-mode when "map")
  (jtsx-define-obsolete-mode-variable-alias obsolete-mode current-mode when "hook")
  (jtsx-define-obsolete-mode-variable-alias obsolete-mode current-mode when "syntax-table")
  (jtsx-define-obsolete-mode-variable-alias obsolete-mode current-mode when "abbrev-table"))

;;;###autoload
(define-derived-mode jtsx-jsx-mode js-ts-mode "JSX"
  "Major mode extending `js-ts-mode'."
  :group 'jtsx
  (let ((ts-lang-key 'javascript))
    (when (treesit-ready-p ts-lang-key)
      ;; js-ts-mode mode sets auto-mode-alist when loaded
      (jtsx-prioritize-mode-if-present 'jtsx-jsx-mode)

      ;; Do a deep copy of javascript indent rules variable, to
      ;; prevent side effects as we will modify it.
      (setq-local jtsx-ts-indent-rules
                  (jtsx-deep-copy-indent-rules
                   'javascript js--treesit-indent-rules))

      (jtsx-ts-remove-indent-rule
       ts-lang-key
       '((node-is "switch_\\(?:case\\|default\\)") parent-bol 0))

      (when (version= emacs-version "29.1")
        ;; Fix indentation bug.
        ;; (see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=65134)
        (jtsx-ts-remove-indent-rule
         ts-lang-key
         '(js-jsx--treesit-indent-compatibility-bb1f97b))
        (mapc (lambda (rule) (jtsx-ts-add-indent-rule 'javascript rule))
              (js-jsx--treesit-indent-compatibility-bb1f97b)))

      (when (version<= emacs-version "29.2")
        ;; Fix some font lock bugs
        ;; (see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67684)
        ;; (see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=68879)
        (setq-local treesit-font-lock-settings
                    (jtsx-jsx-mode-font-lock-settings)))

      (jtsx-configure-mode-base
       'jtsx-jsx-mode
       jtsx-jsx-mode-map
       ts-lang-key
       'js-indent-level))))

;; Keep old jsx-mode for backward compatibility but mark it as obsolete.
(jtsx-define-obsolete-mode-alias 'jsx-mode 'jtsx-jsx-mode "jtsx 0.2.1")

(defun jtsx-add-support-for-switch-indent-option (ts-lang-key)
  "Add support for switch/case indentation option for TS-LANG-KEY language."
  ;; Remove specific indent rule for `case' and `default' (introduced by commit ab12628) which
  ;; defeats `jtsx-switch-indent-offset' option.
  (jtsx-ts-remove-indent-rule ts-lang-key  '((or (node-is "case")
                                                 (node-is "default"))
                                             parent-bol typescript-ts-mode-indent-offset)))

(defun jtsx-typescript-tsx-configure-mode-common(ts-lang-key)
  "Common part of `jtsx-typescript-mode' and `jtsx-tsx-mode'.
TS-LANG-KEY is the treesit language key."
  (setq-local jtsx-ts-indent-rules
              (typescript-ts-mode--indent-rules ts-lang-key))
  (jtsx-add-support-for-switch-indent-option ts-lang-key)
  (when (version<= emacs-version "29.2")
    ;; Fix a font lock bug
    ;; (see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=69024)
    (setq-local treesit-font-lock-settings
                (jtsx-tsx-mode-font-lock-settings ts-lang-key))))

;;;###autoload
(define-derived-mode jtsx-tsx-mode tsx-ts-mode "TSX"
  "Major mode extending `tsx-ts-mode'."
  :group 'jtsx
  (let ((ts-lang-key 'tsx))
    (when (treesit-ready-p ts-lang-key)
      (jtsx-typescript-tsx-configure-mode-common ts-lang-key)

      (jtsx-configure-mode-base
       'jtsx-tsx-mode
       jtsx-tsx-mode-map
       ts-lang-key
       'typescript-ts-mode-indent-offset))))

;; Keep old tsx-mode for backward compatibility but mark it as obsolete.
(jtsx-define-obsolete-mode-alias 'tsx-mode 'jtsx-tsx-mode "jtsx 0.2.1")

;; typescript-ts-mode package sets auto-mode-alist when loaded
(jtsx-prioritize-mode-if-present 'jtsx-tsx-mode)

;;;###autoload
(define-derived-mode jtsx-typescript-mode typescript-ts-mode "TS"
  "Major mode extending `typescript-ts-mode'."
  :group 'jtsx
  ;; TODO: Maybe we should serve JSX blocks in ts files too?
  ;;       Although not a good practice, such files do exit in the wild.
  (let ((ts-lang-key 'typescript))
    (when (treesit-ready-p ts-lang-key)
      (jtsx-typescript-tsx-configure-mode-common ts-lang-key)
      (jtsx-customize-indent-rules ts-lang-key
                                   'typescript-ts-mode-indent-offset)
      (when jtsx-enable-all-syntax-highlighting-features
        (setq-local treesit-font-lock-level 4))
      (treesit-major-mode-setup))))

;; typescript-ts-mode package sets auto-mode-alist when loaded
(jtsx-prioritize-mode-if-present 'jtsx-typescript-mode)

;;;###autoload
(defun jtsx-install-treesit-language (ts-lang-key)
  "Wrapper around `treesit-install-language-grammar' with preset sources.
TS-LANG-KEY is the language to be installed."
  ;; Known bug : calling `treesit-install-language-grammar' multiple times for the same language
  ;; seems buggy.
  ;; Fixed by commit 1098c114b74 in emacs 29.2
  ;; (bug https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66673)
  (interactive (list (intern (completing-read "Language: " '(javascript jsdoc tsx typescript)))))
  (unless (alist-get ts-lang-key treesit-language-source-alist)
    (let ((source (pcase ts-lang-key
                    ('javascript '("https://github.com/tree-sitter/tree-sitter-javascript"
                                   "master"
                                   "src"))
                    ('jsdoc '("https://github.com/tree-sitter/tree-sitter-jsdoc"
                              "master"
                              "src"))
                    ('tsx '("https://github.com/tree-sitter/tree-sitter-typescript"
                            "master"
                            "tsx/src"))
                    ('typescript '("https://github.com/tree-sitter/tree-sitter-typescript"
                                   "master"
                                   "typescript/src"))
                    (_ nil))))
      (cl-assert source nil (format "Not expected language: %s" ts-lang-key))
      (let ((lang-source (cons ts-lang-key source)))
        (if treesit-language-source-alist
            (add-to-list 'treesit-language-source-alist lang-source)
          (setq treesit-language-source-alist (list lang-source))))))
  (funcall-interactively #'treesit-install-language-grammar ts-lang-key))

(provide 'jtsx)
;;; jtsx.el ends here
