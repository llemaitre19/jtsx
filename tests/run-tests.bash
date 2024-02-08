#! /bin/bash

# Run tests locally (as close as possible to Github Worflows).
# It requires Nix (https://nixos.org/download)

init_dir="/tmp/.emacs.d_jtsx_tests"
emacs_versions=("emacs-29-1" "emacs-29-2" "emacs-snapshot")

# Ensure that Emacs init dir is empty
rm -rf $init_dir

# Run tests on each Emacs versions
for version in ${emacs_versions[@]}; do
    emacs_nix="nix run --accept-flake-config github:purcell/nix-emacs-ci#$version --"
    printf "RUN PACKAGE-LINT ON '%s'.\n" $version
    if ! $emacs_nix --batch \
         --eval "(progn \
                (require 'package) \
                (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
                (package-initialize) \
                (unless package-archive-contents \
                (package-refresh-contents)) \
                (unless (package-installed-p 'package-lint) \
                        (package-install 'package-lint)))" \
         --eval "(require 'package-lint)" \
         -f package-lint-batch-and-exit ../jtsx.el; then
       printf "PACKAGE LINT FAILURE ON '%s'.\n" $version
       exit
    fi
    printf "RUN TESTS ON '%s'.\n" $version
    if ! $emacs_nix --init-directory=$init_dir --batch -l ert -l ../jtsx.el -l jtsx-tests.el \
         --eval "(jtsx-install-treesit-language 'javascript)" \
         --eval "(jtsx-install-treesit-language 'tsx)" \
         -f ert-run-tests-batch-and-exit; then
        printf "TESTS FAILURE ON '%s'.\n" $version
        exit
    fi
done

echo "TESTS SUCCESS!"
