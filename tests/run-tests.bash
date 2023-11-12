#! /bin/bash

# Run tests locally (as close as possible to Github Worflows).
# It requires Nix (https://nixos.org/download)

init_dir="/tmp/.emacs.d_jtsx_tests"
emacs_versions=("emacs-29-1" "emacs-snapshot")

# Ensure that Emacs init dir is empty
rm -rf $init_dir

# Run tests on each Emacs versions
for version in ${emacs_versions[@]}; do
    if ! nix run --accept-flake-config github:purcell/nix-emacs-ci#$version -- -Q \
         --init-directory=$init_dir --batch -l ert -l ../jtsx.el -l jtsx-tests.el \
         --eval "(jtsx-install-treesit-language 'javascript)" \
         --eval "(jtsx-install-treesit-language 'tsx)" \
         -f ert-run-tests-batch-and-exit; then
        printf "TESTS FAILURE ON '%s'.\n" $version
        exit
    fi
done

echo "TESTS SUCCESS!"
