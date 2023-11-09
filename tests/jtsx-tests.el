;;; jtsx-tests.el --- jtsx tests -*- lexical-binding: t -*-

;; Copyright (C) 2023 Loïc Lemaître

;; Author: Loïc Lemaître <loic.lemaitre@gmail.com>
;; Maintainer: Loïc Lemaître <loic.lemaitre@gmail.com

;;; Commentary:
;; Tests suite for jtsx package.
;; It uses ERT testing tool.

;;; Code:

(require 'jtsx)
(require 'ert)

(defconst indent-offset 2)

;; HELPERS
(defun do-command-into-buffer (initial-content
                               customize command return &optional mode)
  "Return the result of RETURN on a temp buffer after having run COMMAND on it.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (with-temp-buffer
    (let ((indent-tabs-mode nil)
          (js-indent-level indent-offset)
          (typescript-ts-mode-indent-offset indent-offset))
      (if mode (funcall mode) (tsx-mode))
      (hs-minor-mode)
      (transient-mark-mode)
      (insert initial-content)
      (goto-char 0)
      (funcall customize)
      (funcall command)
      (funcall return))))

(defun do-command-into-buffer-ret-position (initial-content
                                            customize command &optional mode)
  "Return the point position in a temp buffer after having run COMMAND on it.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (let ((return (lambda () (point))))
    (do-command-into-buffer initial-content customize command return mode)))

(defun do-command-into-buffer-ret-content (initial-content
                                           customize command &optional mode)
  "Return the content of a temp buffer after having run COMMAND on it.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (let ((return (lambda () (buffer-substring-no-properties 1 (point-max)))))
    (do-command-into-buffer initial-content customize command return mode)))

(defun comment-dwim-into-buffer (initial-content customize &optional mode)
  "Return the content of a temp buffer after having commented a part.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (let ((command (lambda () (call-interactively #'jtsx-comment-dwim))))
    (do-command-into-buffer-ret-content initial-content customize command mode)))

(defun indent-all-into-buffer (initial-content &optional mode)
  "Return the content of a temp buffer after having indenting current line.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (let ((command (lambda () (call-interactively #'indent-for-tab-command))))
    (do-command-into-buffer-ret-content initial-content 'mark-whole-buffer command mode)))

(defun jump-jsx-opening-element-into-buffer (initial-content customize &optional mode)
  "Return point in a temp buffer after having jump to the opening element.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (do-command-into-buffer-ret-position initial-content customize
                                       'jtsx-jump-jsx-opening-tag mode))

(defun jump-jsx-closing-element-into-buffer (initial-content customize &optional mode)
  "Return point in a temp buffer after having jump to the closing element.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (do-command-into-buffer-ret-position initial-content customize
                                       'jtsx-jump-jsx-closing-tag mode))

(defun jump-jsx-element-dwim-into-buffer (initial-content customize &optional mode)
  "Return point in a temp buffer after having jump to the dwim element.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (do-command-into-buffer-ret-position initial-content customize
                                       'jtsx-jump-jsx-element-tag-dwim mode))

(defun rename-jsx-element-into-buffer (initial-content customize &optional mode new-name)
  "Return the content of a temp buffer after having renamed a jsx element.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode.
NEW-NAME is the new name for the jsx element."
  (let ((command (lambda () (jtsx-rename-jsx-element (or new-name "B")))))
 (do-command-into-buffer-ret-content initial-content customize command mode)))

(defun move-forward-jsx-element-end-into-buffer (initial-content customize &optional mode)
  "Return content of temp bufffer after having moved a jsx element end forward.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (do-command-into-buffer-ret-content initial-content customize
                                      'jtsx-move-jsx-element-tag-forward mode))

(defun move-backward-jsx-element-end-into-buffer (initial-content customize &optional mode)
  "Return content of temp buffer after having moved a jsx element end backward.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (do-command-into-buffer-ret-content initial-content customize
                                      'jtsx-move-jsx-element-tag-backward mode))


(defun move-forward-jsx-element-into-buffer (initial-content customize &optional mode)
  "Return the content of a temp buffer after having moved a jsx element forward.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (do-command-into-buffer-ret-content initial-content customize
                                      'jtsx-move-jsx-element-forward mode))

(defun move-backward-jsx-element-into-buffer (initial-content customize &optional mode)
  "Return the content of a temp buffer after having moved a jsx element backward.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (do-command-into-buffer-ret-content initial-content customize
                                      'jtsx-move-jsx-element-backward mode))

(defun move-forward-jsx-element-step-in-into-buffer (initial-content customize &optional mode)
  "Return content of temp buffer after moving a jsx element forward with step-in.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (do-command-into-buffer-ret-content initial-content customize
                                      'jtsx-move-jsx-element-step-in-forward mode))

(defun move-backward-jsx-element-step-in-into-buffer (initial-content customize &optional mode)
  "Return content of temp buffer after moving a jsx element backward with step-in.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (do-command-into-buffer-ret-content initial-content customize
                                      'jtsx-move-jsx-element-step-in-backward mode))

(defun add-electric-closing-element-into-buffer (initial-content customize &optional mode)
  "Return the content of a temp buffer after adding electric closing tag.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (let ((command (lambda () (call-interactively #'jtsx-jsx-electric-closing-element))))
    (do-command-into-buffer-ret-content initial-content customize command mode)))

(defun wrap-in-jsx-element-into-buffer (initial-content customize &optional mode element-name)
  "Return the content of a temp buffer after wrapping some JSX with JSX element.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode.
ELEMENT-NAME is the name of the wrapping element."
  (let ((command (lambda () (jtsx-wrap-in-jsx-element (or element-name "W")))))
    (do-command-into-buffer-ret-content initial-content customize command mode)))

(defun hs-forward-sexp-into-buffer (initial-content customize &optional mode)
  "Return point in a temp buffer after forwarding sexp.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (let ((command (lambda () (call-interactively #'jtsx-hs-forward-sexp))))
  (do-command-into-buffer-ret-position initial-content customize command mode)))

(defun hs-find-block-beginning-into-buffer (initial-content customize &optional mode)
  "Return point in a temp buffer after finding block beginning.
Initialize the buffer with INITIAL-CONTENT and customized it with CUSTOMIZE.
Turn this buffer in MODE mode if supplied or defaults to tsx-mode."
  (do-command-into-buffer-ret-position initial-content customize 'jtsx-hs-find-block-beginning
                                       mode))

(defun find-and-set-region (re &optional count)
  "Find the region matching RE and set it.  COUNT is the COUNTth match."
  (when (re-search-forward re nil nil count)
    (goto-char (match-beginning 0))
    (set-mark (match-end 0))))

;; TEST COMMENTS
(ert-deftest jtsx-test-comment-js-region ()
  (let ((set-region #'(lambda () (find-and-set-region "var")))
        (content "let var;")
        (result "let // var\n;"))
    (should (equal (comment-dwim-into-buffer content set-region #'jsx-mode) result))
    (should (equal (comment-dwim-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-comment-jsx-text-region ()
  (let ((set-region #'(lambda () (find-and-set-region "Hello")))
        (content "(<A>Hello</A>);")
        (result "(<A>{/* Hello */}</A>);"))
    (should (equal (comment-dwim-into-buffer content set-region #'jsx-mode) result))
    (should (equal (comment-dwim-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-comment-jsx-attribute-region ()
  (let ((set-region #'(lambda () (find-and-set-region "disabled={false}")))
        (content "(<A disabled={false}>Hello</A>);")
        (result "(<A /* disabled={false} */>Hello</A>);"))
    (should (equal (comment-dwim-into-buffer content set-region #'jsx-mode) result))
    (should (equal (comment-dwim-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-comment-jsx-boolean-attribute-region ()
  (let ((set-region #'(lambda () (find-and-set-region "disabled")))
        (content "(<A disabled>Hello</A>);")
        (result "(<A /* disabled */>Hello</A>);"))
    (should (equal (comment-dwim-into-buffer content set-region #'jsx-mode) result))
    (should (equal (comment-dwim-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-comment-jsx-attribute-region-in-self-closing-tag ()
  (let ((set-region #'(lambda () (find-and-set-region "disabled={false}")))
        (content "(<A disabled={false} />);")
        (result "(<A /* disabled={false} */ />);"))
    (should (equal (comment-dwim-into-buffer content set-region #'jsx-mode) result))
    (should (equal (comment-dwim-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-comment-jsx-boolean-attribute-region-in-self-closing-tag ()
  (let ((set-region #'(lambda () (find-and-set-region "disabled")))
        (content "(<A disabled />);")
        (result "(<A /* disabled */ />);"))
    (should (equal (comment-dwim-into-buffer content set-region #'jsx-mode) result))
    (should (equal (comment-dwim-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-comment-jsx-expression-region ()
  (let ((set-region #'(lambda () (find-and-set-region "{'test'}")))
        (content "(<A>{'test'}</A>);")
        (result "(<A>{/* {'test'} */}</A>);"))
    (should (equal (comment-dwim-into-buffer content set-region #'jsx-mode) result))
    (should (equal (comment-dwim-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-comment-jsx-opening-element-region ()
  (let ((set-region #'(lambda () (find-and-set-region "A")))
        (content "(<A></A>);")
        (result "(<{/* A */}></A>);"))
    (should (equal (comment-dwim-into-buffer content set-region #'jsx-mode) result))
    (should (equal (comment-dwim-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-comment-jsx-closing-element-region ()
  (let ((set-region #'(lambda () (find-and-set-region "A" 2)))
        (content "(<A></A>);")
        (result "(<A></{/* A */}>);"))
    (should (equal (comment-dwim-into-buffer content set-region #'jsx-mode) result))
    (should (equal (comment-dwim-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-comment-js-enclosing-jsx-at-point ()
  (let ((set-point #'(lambda () (goto-char 0)))
        (content "(<A></A>);")
        (result "(<A></A>);                      // "))
    (should (equal (comment-dwim-into-buffer content set-point #'jsx-mode) result))
    (should (equal (comment-dwim-into-buffer content set-point #'tsx-mode) result))))

(ert-deftest jtsx-test-comment-jsx-at-point ()
  (let ((set-point #'(lambda () (progn (goto-char 0) (forward-line 1))))
        (content "(\n<A>\n</A>\n);")
        (result "(\n<A>                             {/*  */}\n</A>\n);"))
    (should (equal (comment-dwim-into-buffer content set-point #'jsx-mode) result))
    (should (equal (comment-dwim-into-buffer content set-point #'tsx-mode) result))))

;; TEST INDENTATION
(ert-deftest jtsx-test-no-indent-jsx-switch-case ()
  (let ((jtsx-switch-indent-offset 0)
        (content "switch (x) {\ncase true:\nbreak;\ndefault:\nbreak;\n};")
        (result "switch (x) {\ncase true:\n  break;\ndefault:\n  break;\n};"))
    (should (equal (indent-all-into-buffer content #'jsx-mode) result))
    (should (equal (indent-all-into-buffer content #'tsx-mode) result))))

(ert-deftest jtsx-test-indent-jsx-switch-case ()
  (let ((jtsx-switch-indent-offset indent-offset)
        (content "switch (x) {\ncase true:\nbreak;\ndefault:\nbreak;\n};")
        (result "switch (x) {\n  case true:\n    break;\n  default:\n    break;\n};"))
    (should (equal (indent-all-into-buffer content #'jsx-mode) result))
    (should (equal (indent-all-into-buffer content #'tsx-mode) result))))

(ert-deftest jtsx-test-indent-statement-block-regarding-parent ()
  (let ((jtsx-indent-statement-block-regarding-standalone-parent nil)
        (content "function test(a,\nb) {\nreturn a + b;\n}")
        (result "function test(a,\n  b) {\n    return a + b;\n  }"))
    (should (equal (indent-all-into-buffer content #'jsx-mode) result))
    (should (equal (indent-all-into-buffer content #'tsx-mode) result))))

(ert-deftest jtsx-test-indent-statement-block-regarding-standalone-parent ()
  (let ((jtsx-indent-statement-block-regarding-standalone-parent t)
        (content "function test(a,\nb) {\nreturn a + b;\n}")
        (result "function test(a,\n  b) {\n  return a + b;\n}"))
    (should (equal (indent-all-into-buffer content #'jsx-mode) result))
    (should (equal (indent-all-into-buffer content #'tsx-mode) result))))

;; TEST JUMPS
(ert-deftest jtsx-test-jump-opening-element-starting-from-closing ()
  (let ((move-point (lambda () (goto-char 6))) ;; inside "</A>"
        (content "(<A></A>);")
        (result 3)) ;; right after first "<"
    (should (equal (jump-jsx-opening-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (jump-jsx-opening-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-jump-opening-element-starting-from-children ()
  (let ((move-point (lambda () (goto-char 5))) ;; A children
        (content "(<A></A>);")
        (result 3)) ;; right after first "<"
    (should (equal (jump-jsx-opening-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (jump-jsx-opening-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-jump-opening-element-nested-context ()
  (let ((move-point (lambda () (goto-char 13))) ;; in last "</A>"
        (content "(<A><A></A></A>);")
        (result 3)) ;; right after first "<"
    (should (equal (jump-jsx-opening-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (jump-jsx-opening-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-jump-closing-element-starting-from-opening ()
  (let ((move-point (lambda () (goto-char 6))) ;; inside "</A>"
        (content "(<A></A>);")
        (result 8)) ;; right before last ">"
    (should (equal (jump-jsx-closing-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (jump-jsx-closing-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-jump-closing-element-starting-from-children ()
  (let ((move-point (lambda () (goto-char 5))) ;; A children
        (content "(<A></A>);")
        (result 8)) ;; right before last ">"
    (should (equal (jump-jsx-closing-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (jump-jsx-closing-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-jump-closing-element-nested-context ()
  (let ((move-point (lambda () (goto-char 3))) ;; in the first "<A>"
        (content "(<A><A></A></A>);")
        (result 15)) ;; right before last ">"
    (should (equal (jump-jsx-closing-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (jump-jsx-closing-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-jump-element-dwim-from-opening ()
  (let ((move-point (lambda () (goto-char 3))) ;; inside "<A>"
        (content "(<A></A>);")
        (result 8)) ;; right before last ">"
    (should (equal (jump-jsx-element-dwim-into-buffer content move-point #'jsx-mode) result))
    (should (equal (jump-jsx-element-dwim-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-jump-element-dwim-from-closing ()
  (let ((move-point (lambda () (goto-char 7))) ;; inside "</A>"
        (content "(<A></A>);")
        (result 3)) ;; right after first "<"
    (should (equal (jump-jsx-element-dwim-into-buffer content move-point #'jsx-mode) result))
    (should (equal (jump-jsx-element-dwim-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-jump-element-dwim-from-children-1 ()
  (let ((move-point (lambda () (goto-char 6))) ;; inside "XX"
        (content "(<A>XXYY</A>);")
        (result 12)) ;; right before last ">"
    (should (equal (jump-jsx-element-dwim-into-buffer content move-point #'jsx-mode) result))
    (should (equal (jump-jsx-element-dwim-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-jump-element-dwim-from-children-2 ()
  (let ((move-point (lambda () (goto-char 8))) ;; inside "YY"
        (content "(<A>XXYY</A>);")
        (result 3)) ;; right after first "<"
    (should (equal (jump-jsx-element-dwim-into-buffer content move-point #'jsx-mode) result))
    (should (equal (jump-jsx-element-dwim-into-buffer content move-point #'tsx-mode) result))))

;; TEST RENAME JSX ELEMENT
(ert-deftest jtsx-test-rename-jsx-element-from-opening-left ()
  (let ((move-point #'(lambda () (goto-char 3)))
        (content "(<A>Hello</A>);")
        (result "(<B>Hello</B>);"))
    (should (equal (rename-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (rename-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-rename-jsx-element-from-opening-middle ()
  (let ((move-point #'(lambda () (goto-char 4)))
        (content "(<AA>Hello</AA>);")
        (result "(<B>Hello</B>);"))
    (should (equal (rename-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (rename-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-rename-jsx-element-from-opening-right ()
  (let ((move-point #'(lambda () (goto-char 4)))
        (content "(<A>Hello</A>);")
        (result "(<B>Hello</B>);"))
    (should (equal (rename-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (rename-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-rename-jsx-element-from-closing-left ()
  (let ((move-point #'(lambda () (goto-char 12)))
        (content "(<A>Hello</A>);")
        (result "(<B>Hello</B>);"))
    (should (equal (rename-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (rename-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-rename-jsx-element-from-closing-middle ()
  (let ((move-point #'(lambda () (goto-char 14)))
        (content "(<AA>Hello</AA>);")
        (result "(<B>Hello</B>);"))
    (should (equal (rename-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (rename-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-rename-jsx-element-from-closing-right ()
  (let ((move-point #'(lambda () (goto-char 13)))
        (content "(<A>Hello</A>);")
        (result "(<B>Hello</B>);"))
    (should (equal (rename-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (rename-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-rename-jsx-fragment-from-opening ()
  (let ((move-point #'(lambda () (goto-char 3)))
        (content "(<>Hello</>);")
        (result "(<B>Hello</B>);"))
    (should (equal (rename-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (rename-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-rename-jsx-fragment-from-closing ()
  (let ((move-point #'(lambda () (goto-char 11)))
        (content "(<>Hello</>);")
        (result "(<B>Hello</B>);"))
    (should (equal (rename-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (rename-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-rename-jsx-mixed-from-opening ()
  (let ((move-point #'(lambda () (goto-char 3)))
        (content "(<A>Hello</>);")
        (result "(<B>Hello</B>);"))
    (should (equal (rename-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (rename-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-rename-jsx-mixed-from-closing ()
  (let ((move-point #'(lambda () (goto-char 12)))
        (content "(<A>Hello</>);")
        (result "(<B>Hello</B>);"))
    (should (equal (rename-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (rename-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-rename-jsx-element-to-fragment ()
  (let ((move-point #'(lambda () (goto-char 3)))
        (content "(<A>Hello</A>);")
        (result "(<>Hello</>);"))
    (should (equal (rename-jsx-element-into-buffer content move-point #'jsx-mode "") result))
    (should (equal (rename-jsx-element-into-buffer content move-point #'tsx-mode "") result))))

(ert-deftest jtsx-test-rename-jsx-element-ignored ()
  (let ((move-point #'(lambda () (goto-char 10)))
        (content "(<A>Hello</A>);")
        (result "(<A>Hello</A>);"))
    (should (equal (rename-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (rename-jsx-element-into-buffer content move-point #'tsx-mode) result))))

;; TEST MOVE JSX OPENING OR CLOSING ELEMENT
(ert-deftest jtsx-test-move-jsx-opening-element-forward ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 2)))
        (content "(\n  <>\n    <A>\n      <B>\n      </B>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <B>\n    </B>\n    <A>\n    </A>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-end-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-forward-jsx-element-end-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-opening-element-forward-multiline ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 2)))
        (content "(\n  <>\n    <A\n      show>\n      <B>\n      </B>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <B>\n    </B>\n    <A\n      show>\n    </A>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-end-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-forward-jsx-element-end-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-opening-element-forward-multiline-from-attribute ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 3)))
        (content "(\n  <>\n    <A\n      show>\n      <B>\n      </B>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <B>\n    </B>\n    <A\n      show>\n    </A>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-end-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-forward-jsx-element-end-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-opening-element-forward-failed ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 5)))
        (content "(\n  <>\n    <A>\n      <B>\n      </B>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <A>\n      <B>\n      </B>\n    </A>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-end-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-forward-jsx-element-end-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-closing-element-backward ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 5)))
        (content "(\n  <>\n    <A>\n      <B>\n      </B>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <A>\n    </A>\n    <B>\n    </B>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-end-into-buffer content move-point #'jsx-mode)
                   result))
    (should (equal (move-backward-jsx-element-end-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-closing-element-backward-multiline ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 4)))
        (content "(\n  <>\n    <B>\n    </B>\n    <A\n      show>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <A\n      show>\n      <B>\n      </B>\n    </A>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-end-into-buffer content move-point #'jsx-mode)
                   result))
    (should (equal (move-backward-jsx-element-end-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-closing-element-backward-multiline-from-attribute ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 5)))
        (content "(\n  <>\n    <B>\n    </B>\n    <A\n      show>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <A\n      show>\n      <B>\n      </B>\n    </A>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-end-into-buffer content move-point #'jsx-mode)
                   result))
    (should (equal (move-backward-jsx-element-end-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-closing-element-backward-failed ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 2)))
        (content "(\n  <>\n    <A>\n      <B>\n      </B>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <A>\n      <B>\n      </B>\n    </A>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-end-into-buffer content move-point #'jsx-mode)
                   result))
    (should (equal (move-backward-jsx-element-end-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-opening-element-forward-from-attribute-exp-failed () ;; Bug fix
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 2)))
        (content "(\n  <A attr={(\n    <C></C>\n  )}>\n    <B />\n  </A>\n);")
        (result "(\n  <A attr={(\n    <C></C>\n  )}>\n    <B />\n  </A>\n);"))
    (should (equal (move-forward-jsx-element-end-into-buffer content move-point #'jsx-mode)
                   result))
    (should (equal (move-forward-jsx-element-end-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-opening-element-backward-from-attribute-exp-failed () ;; Bug fix
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 2)))
        (content "(\n  <A attr={(\n    <C></C>\n  )}>\n    <B />\n  </A>\n);")
        (result "(\n  <A attr={(\n    <C></C>\n  )}>\n    <B />\n  </A>\n);"))
    (should (equal (move-backward-jsx-element-end-into-buffer content move-point #'jsx-mode)
                   result))
    (should (equal (move-backward-jsx-element-end-into-buffer content move-point #'tsx-mode)
                   result))))

;; TEST MOVE JSX FULL ELEMENT
(ert-deftest jtsx-test-move-jsx-element-forward-from-opening ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 2)))
        (content "(\n  <>\n    <A>\n    </A>\n    <B>\n    </B>\n  </>\n);")
        (result "(\n  <>\n    <B>\n    </B>\n    <A>\n    </A>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-forward-from-opening-attribute ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 2)))
        (content "(\n  <>\n    <A\n      show>\n    </A>\n    <B>\n    </B>\n  </>\n);")
        (result "(\n  <>\n    <B>\n    </B>\n    <A\n      show>\n    </A>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-forward-from-closing ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 3)))
        (content "(\n  <>\n    <A>\n    </A>\n    <B>\n    </B>\n  </>\n);")
        (result "(\n  <>\n    <B>\n    </B>\n    <A>\n    </A>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-backward-from-opening ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 4)))
        (content "(\n  <>\n    <A>\n    </A>\n    <B>\n    </B>\n  </>\n);")
        (result "(\n  <>\n    <B>\n    </B>\n    <A>\n    </A>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-backward-from-opening-attribute ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 5)))
        (content "(\n  <>\n    <A>\n    </A>\n    <B\n      show>\n    </B>\n  </>\n);")
        (result "(\n  <>\n    <B\n      show>\n    </B>\n    <A>\n    </A>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-backward-from-closing ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 5)))
        (content "(\n  <>\n    <A>\n    </A>\n    <B>\n    </B>\n  </>\n);")
        (result "(\n  <>\n    <B>\n    </B>\n    <A>\n    </A>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

;; TEST MOVE JSX FULL ELEMENT STEP IN
(ert-deftest jtsx-test-move-jsx-element-step-in-forward-from-opening ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 2)))
        (content "(\n  <>\n    <A>\n    </A>\n    <B>\n    </B>\n  </>\n);")
        (result "(\n  <>\n    <B>\n      <A>\n      </A>\n    </B>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-step-in-into-buffer content move-point #'jsx-mode)
                   result))
    (should (equal (move-forward-jsx-element-step-in-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-element-step-in-forward-from-opening-attribute ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 2)))
        (content "(\n  <>\n    <A\n      show>\n    </A>\n    <B>\n    </B>\n  </>\n);")
        (result "(\n  <>\n    <B>\n      <A\n        show>\n      </A>\n    </B>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-step-in-into-buffer content move-point #'jsx-mode)
                   result))
    (should (equal (move-forward-jsx-element-step-in-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-element-step-in-forward-from-closing ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 3)))
        (content "(\n  <>\n    <A>\n    </A>\n    <B>\n    </B>\n  </>\n);")
        (result "(\n  <>\n    <B>\n      <A>\n      </A>\n    </B>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-step-in-into-buffer content move-point #'jsx-mode)
                   result))
    (should (equal (move-forward-jsx-element-step-in-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-element-step-in-backward-from-opening ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 4)))
        (content "(\n  <>\n    <A>\n    </A>\n    <B>\n    </B>\n  </>\n);")
        (result "(\n  <>\n    <A>\n      <B>\n      </B>\n    </A>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-step-in-into-buffer content move-point #'jsx-mode)
                   result))
    (should (equal (move-backward-jsx-element-step-in-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-element-step-in-backward-from-opening-attribute ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 5)))
        (content "(\n  <>\n    <A>\n    </A>\n    <B\n      show>\n    </B>\n  </>\n);")
        (result "(\n  <>\n    <A>\n      <B\n        show>\n      </B>\n    </A>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-step-in-into-buffer content move-point #'jsx-mode)
                   result))
    (should (equal (move-backward-jsx-element-step-in-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-move-jsx-element-step-in-backward-from-closing ()
  (let ((move-point #'(lambda () (goto-char 0) (forward-line 5)))
        (content "(\n  <>\n    <A>\n    </A>\n    <B>\n    </B>\n  </>\n);")
        (result "(\n  <>\n    <A>\n      <B>\n      </B>\n    </A>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-step-in-into-buffer content move-point #'jsx-mode)
                   result))
    (should (equal (move-backward-jsx-element-step-in-into-buffer content move-point #'tsx-mode)
                   result))))

;; TEST MOVE JSX FULL ELEMENT STEP OUT
(ert-deftest jtsx-test-move-jsx-element-step-out-forward-from-opening ()
  (let ((jtsx-jsx-element-move-allow-step-out t)
        (move-point #'(lambda () (goto-char 0) (forward-line 5)))
        (content
         "(\n  <>\n    <A>\n      <B>\n      </B>\n      <C>\n      </C>\n    </A>\n  </>\n);")
        (result
         "(\n  <>\n    <A>\n      <B>\n      </B>\n    </A>\n    <C>\n    </C>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-step-out-forward-from-opening-attribute ()
  (let ((jtsx-jsx-element-move-allow-step-out t)
        (move-point #'(lambda () (goto-char 0) (forward-line 6)))
        (content
         "(\n  <>\n    <A>\n      <B>\n      </B>\n      <C\n        show>\n      </C>\n\    </A>\n\
  </>\n);")
        (result
         "(\n  <>\n    <A>\n      <B>\n      </B>\n    </A>\n    <C\n      show>\n    </C>\n\
  </>\n);"))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-step-out-forward-from-closing ()
  (let ((jtsx-jsx-element-move-allow-step-out t)
        (move-point #'(lambda () (goto-char 0) (forward-line 6)))
        (content
         "(\n  <>\n    <A>\n      <B>\n      </B>\n      <C>\n      </C>\n    </A>\n  </>\n);")
        (result
         "(\n  <>\n    <A>\n      <B>\n      </B>\n    </A>\n    <C>\n    </C>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-step-out-backward-from-opening ()
  (let ((jtsx-jsx-element-move-allow-step-out t)
        (move-point #'(lambda () (goto-char 0) (forward-line 3)))
        (content
         "(\n  <>\n    <A>\n      <B>\n      </B>\n      <C>\n      </C>\n    </A>\n  </>\n);")
        (result
         "(\n  <>\n    <B>\n    </B>\n    <A>\n      <C>\n      </C>\n    </A>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-step-out-backward-from-opening-attribute ()
  (let ((jtsx-jsx-element-move-allow-step-out t)
        (move-point #'(lambda () (goto-char 0) (forward-line 4)))
        (content
         "(\n  <>\n    <A>\n      <B\n        show>\n      </B>\n      <C>\n      </C>\n    </A>\n\
  </>\n);")
        (result
         "(\n  <>\n    <B\n      show>\n    </B>\n    <A>\n      <C>\n      </C>\n    </A>\n\
  </>\n);"))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-step-out-backward-from-closing ()
  (let ((jtsx-jsx-element-move-allow-step-out t)
        (move-point #'(lambda () (goto-char 0) (forward-line 4)))
        (content
         "(\n  <>\n    <A>\n      <B>\n      </B>\n      <C>\n      </C>\n    </A>\n  </>\n);")
        (result
         "(\n  <>\n    <B>\n    </B>\n    <A>\n      <C>\n      </C>\n    </A>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-step-out-forward-from-opening-failed ()
  (let ((jtsx-jsx-element-move-allow-step-out nil)
        (move-point #'(lambda () (goto-char 0) (forward-line 5)))
        (content
         "(\n  <>\n    <A>\n      <B>\n      </B>\n      <C>\n      </C>\n    </A>\n  </>\n);")
        (result
         "(\n  <>\n    <A>\n      <B>\n      </B>\n      <C>\n      </C>\n    </A>\n  </>\n);"))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-step-out-backward-from-opening-failed ()
  (let ((jtsx-jsx-element-move-allow-step-out nil)
        (move-point #'(lambda () (goto-char 0) (forward-line 3)))
        (content
         "(\n  <>\n    <A>\n      <B>\n      </B>\n      <C>\n      </C>\n    </A>\n  </>\n);")
        (result
         "(\n  <>\n    <A>\n      <B>\n      </B>\n      <C>\n      </C>\n    </A>\n  </>\n);"))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-step-out-forward-from-jsx-expression-failed ()
  (let ((jtsx-jsx-element-move-allow-step-out t)
        (move-point #'(lambda () (goto-char 0) (forward-line 3)))
        (content "(\n  <>\n    {(\n      <A />\n    )}\n  </>\n);")
        (result "(\n  <>\n    {(\n      <A />\n    )}\n  </>\n);"))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-forward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-move-jsx-element-step-out-backward-from-jsx-expression-failed ()
  (let ((jtsx-jsx-element-move-allow-step-out t)
        (move-point #'(lambda () (goto-char 0) (forward-line 3)))
        (content "(\n  <>\n    {(\n      <A />\n    )}\n  </>\n);")
        (result "(\n  <>\n    {(\n      <A />\n    )}\n  </>\n);"))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (move-backward-jsx-element-into-buffer content move-point #'tsx-mode) result))))

;; TEST ELECTRIC CLOSING ELEMENT
(ert-deftest jtsx-test-add-electric-closing-element ()
  (let ((jtsx-enable-jsx-electric-closing-element t)
        (move-point #'(lambda () (goto-char 14)))
        (content "(\n  <>\n    <A\n  </>\n);")
        (result "(\n  <>\n    <A></A>\n  </>\n);"))
    (should (equal (add-electric-closing-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (add-electric-closing-element-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-add-electric-closing-element-fragment ()
  (let ((jtsx-enable-jsx-electric-closing-element t)
        (move-point #'(lambda () (goto-char 13)))
        (content "(\n  <>\n    <\n  </>\n);")
        (result "(\n  <>\n    <></>\n  </>\n);"))
    (should (equal (add-electric-closing-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (add-electric-closing-element-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-add-electric-closing-element-after-attribute ()
  (let ((jtsx-enable-jsx-electric-closing-element t)
        (move-point #'(lambda () (goto-char 19)))
        (content "(\n  <>\n    <A show\n  </>\n);")
        (result "(\n  <>\n    <A show></A>\n  </>\n);"))
    (should (equal (add-electric-closing-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (add-electric-closing-element-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-add-electric-closing-element-nested ()
  (let ((jtsx-enable-jsx-electric-closing-element t)
        (move-point #'(lambda () (goto-char 24)))
        (content "(\n  <>\n    <A>\n      <A\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <A>\n      <A></A>\n    </A>\n  </>\n);"))
    (should (equal (add-electric-closing-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (add-electric-closing-element-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-do-not-add-electric-closing-element-if-present ()
  (let ((jtsx-enable-jsx-electric-closing-element t)
        (move-point #'(lambda () (goto-char 14)))
        (content "(\n  <>\n    <A\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <A>\n    </A>\n  </>\n);"))
    (should (equal (add-electric-closing-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (add-electric-closing-element-into-buffer content move-point #'tsx-mode)
                   result))))

(ert-deftest jtsx-test-do-not-add-electric-closing-element-if-disabled ()
  (let ((jtsx-enable-jsx-electric-closing-element nil)
        (move-point #'(lambda () (goto-char 14)))
        (content "(\n  <>\n    <A\n  </>\n);")
        (result "(\n  <>\n    <A>\n  </>\n);"))
    (should (equal (add-electric-closing-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (add-electric-closing-element-into-buffer content move-point #'tsx-mode)
                   result))))

;; TEST WRAPPING WITH JSX ELEMENT
(ert-deftest jtsx-test-wrap-self-closing-element-at-point ()
  (let ((move-point #'(lambda () (goto-char 11)))
        (content "(\n  <>\n    <A />\n  </>\n);")
        (result "(\n  <>\n    <W>\n      <A />\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-multiline-self-closing-element-at-point ()
  (let ((move-point #'(lambda () (goto-char 11)))
        (content "(\n  <>\n    <A\n      show />\n  </>\n);")
        (result "(\n  <>\n    <W>\n      <A\n        show />\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-self-closing-element-region ()
  (let ((set-region #'(lambda () (find-and-set-region "<A />")))
        (content "(\n  <>\n    <A />\n  </>\n);")
        (result "(\n  <>\n    <W>\n      <A />\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-self-closing-element-trimmed-region ()
  (let ((set-region #'(lambda () (find-and-set-region "\n    <A />\n  ")))
        (content "(\n  <>\n    <A />\n  </>\n);")
        (result "(\n  <>\n    <W>\n      <A />\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-expression-at-point ()
  (let ((move-point #'(lambda () (goto-char 11)))
        (content "(\n  <>\n    {true}\n  </>\n);")
        (result "(\n  <>\n    <W>\n      {true}\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-multiline-expression-at-point ()
  (let ((move-point #'(lambda () (goto-char 11)))
        (content "(\n  <>\n    {\n      true\n    }\n  </>\n);")
        (result "(\n  <>\n    <W>\n      {\n        true\n      }\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-expression-element-region ()
  (let ((set-region #'(lambda () (find-and-set-region "{true}")))
        (content "(\n  <>\n    {true}\n  </>\n);")
        (result "(\n  <>\n    <W>\n      {true}\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-text-at-point ()
  (let ((move-point #'(lambda () (goto-char 11)))
        (content "(\n  <>\n    HELLO\n  </>\n);")
        (result "(\n  <>\n    <W>\n      HELLO\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-text-at-region ()
  (let ((set-region #'(lambda () (find-and-set-region "HELLO")))
        (content "(\n  <>\n    HELLO\n  </>\n);")
        (result "(\n  <>\n    <W>\n      HELLO\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-element-at-point-from-opening ()
  (let ((move-point #'(lambda () (goto-char 11)))
        (content "(\n  <>\n    <A>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <W>\n      <A>\n      </A>\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-multiline-element-at-point-from-opening ()
  (let ((move-point #'(lambda () (goto-char 11)))
        (content "(\n  <>\n    <A\n      show>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <W>\n      <A\n        show>\n      </A>\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-multiline-element-at-point-from-opening-attribute ()
  (let ((move-point #'(lambda () (goto-char 22)))
        (content "(\n  <>\n    <A\n      show>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <W>\n      <A\n        show>\n      </A>\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-element-at-point-from-closing ()
  (let ((move-point #'(lambda () (goto-char 21)))
        (content "(\n  <>\n    <A>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <W>\n      <A>\n      </A>\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-element-region ()
  (let ((set-region #'(lambda () (find-and-set-region "<A>\n    </A>")))
        (content "(\n  <>\n    <A>\n    </A>\n  </>\n);")
        (result "(\n  <>\n    <W>\n      <A>\n      </A>\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-element-region-root ()
  (let ((set-region #'(lambda () (find-and-set-region "<A>\n  </A>")))
        (content "(\n  <A>\n  </A>\n);")
        (result "(\n  <W>\n    <A>\n    </A>\n  </W>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'tsx-mode) result))))

(ert-deftest jtsx-test-wrap-jsx-region ()
  (let ((set-region #'(lambda () (find-and-set-region "<A>\n    </A>\n    <B>\n    </B>")))
        (content "(\n  <>\n    <A>\n    </A>\n    <B>\n    </B>\n  </>\n);")
        (result
         "(\n  <>\n    <W>\n      <A>\n      </A>\n      <B>\n      </B>\n    </W>\n  </>\n);"))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'jsx-mode) result))
    (should (equal (wrap-in-jsx-element-into-buffer content set-region #'tsx-mode) result))))

;; TEST HIDESHOW CUSTOMIZATION
(ert-deftest jtsx-test-hs-forward-sexp-jsx-element ()
  (let ((move-point #'(lambda () (goto-char 2)))
        (content "(<A></A>);")
        (result 9))
    (should (equal (hs-forward-sexp-into-buffer content move-point #'jsx-mode) result))
    (should (equal (hs-forward-sexp-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-hs-forward-sexp-parenthesis ()
  (let ((move-point #'(lambda () (goto-char 1)))
        (content "(<A></A>);")
        (result 10))
    (should (equal (hs-forward-sexp-into-buffer content move-point #'jsx-mode) result))
    (should (equal (hs-forward-sexp-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-hs-forward-sexp-brace ()
  (let ((move-point #'(lambda () (goto-char 2)))
        (content "({true});")
        (result 8))
    (should (equal (hs-forward-sexp-into-buffer content move-point #'jsx-mode) result))
    (should (equal (hs-forward-sexp-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-hs-find-element-beginning-from-opening ()
  (let ((move-point #'(lambda () (goto-char 4)))
        (content "(<A></A>);")
        (result 2))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'jsx-mode) result))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-hs-find-element-beginning-from-closing ()
  (let ((move-point #'(lambda () (goto-char 7)))
        (content "(<A></A>);")
        (result 2))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'jsx-mode) result))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-hs-find-element-beginning-from-children ()
  (let ((move-point #'(lambda () (goto-char 5)))
        (content "(<A></A>);")
        (result 2))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'jsx-mode) result))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-hs-find-element-beginning-from-not-foldable-children ()
  (let ((move-point #'(lambda () (goto-char 6)))
        (content "(<A><B /></A>);")
        (result 2))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'jsx-mode) result))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-hs-find-element-beginning-from-closing-with-foldable-children ()
  (let ((move-point #'(lambda () (goto-char 14)))
        (content "(<A><B></B></A>);")
        (result 2))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'jsx-mode) result))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-hs-find-element-beginning-from-closing-nested-element ()
  (let ((move-point #'(lambda () (goto-char 10)))
        (content "(<A><B></B></A>);")
        (result 5))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'jsx-mode) result))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-hs-find-expression-beginning ()
  (let ((move-point #'(lambda () (goto-char 7)))
        (content "({true});")
        (result 2))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'jsx-mode) result))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'tsx-mode) result))))

(ert-deftest jtsx-test-hs-find-expression-with-nested-element ()
  (let ((move-point #'(lambda () (goto-char 12)))
        (content "({(<A></A>)});")
        (result 2))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'jsx-mode) result))
    (should (equal (hs-find-block-beginning-into-buffer content move-point #'tsx-mode) result))))

;;; jtsx-tests.el ends here
