(package "jtsx"
         "0.4.2"
         "Extends JSX/TSX built-in support")

(website-url "https://github.com/llemaitre19/jtsx")
(keywords "languages")

(package-file "jtsx.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "29.1")

;; Install tree-sitter languages before the tests
(defun install-treesit-languages ()
  (setq treesit-language-source-alist
        '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master"
                      "typescript/src")))
  (funcall-interactively #'treesit-install-language-grammar 'javascript)
  (funcall-interactively #'treesit-install-language-grammar 'tsx)
  (funcall-interactively #'treesit-install-language-grammar 'typescript))

(add-hook 'eask-before-test/ert-hook #'install-treesit-languages)

;; Define command to install the package in the local emacs config (~/.emacs.d/elpa)
(eask-defcommand install-in-emacs-local-config
  (let ((package-dir (file-name-directory (locate-library "jtsx")))
        (target-dir (expand-file-name "~/.emacs.d/elpa/")))
    (copy-directory package-dir target-dir)
    (message "Installed in %s." (concat target-dir
                                        (file-name-nondirectory
                                         (directory-file-name package-dir))))))
