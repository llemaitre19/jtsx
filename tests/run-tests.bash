#! /bin/bash

# Run tests locally (as close as possible to Github Worflows).
# It requires:
# - Eask CLI (https://emacs-eask.github.io/)
# - Nix package manager (https://nixos.org/download)
# - Enabling Nix 'nix-command' and 'flakes' experimental features (https://nixos.org/manual/nix/stable/command-ref/conf-file#conf-experimental-features)

emacs_versions=("emacs-29-1" "emacs-29-2" "emacs-snapshot")

for version in ${emacs_versions[@]}; do
    printf "###### RUN TESTS ON '%s'. ######\n" $version
    emacs_nix="github:purcell/nix-emacs-ci#$version"
    if ! nix shell $emacs_nix nixpkgs#eask --command make -C ../ ci; then
       printf "###### TESTS FAILURE ON '%s'. ######\n" $version
       exit
    fi
done

echo "###### TESTS SUCCESS! ######"
