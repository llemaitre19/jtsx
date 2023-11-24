<!-- markdownlint-disable MD001 MD013 -->

# Changelog

## master

* Wrapping an inline jsx element is now supported.
* Fix wrapping jsx nested into a jsx element attribute.
* Fix commenting jsx nested into a jsx element attribute.
* Set maximum level of syntax highlighting provided by `js-ts-mode` and `tsx-ts-mode` by default. Can be disabled with `jtsx-enable-all-syntax-highlighting-features` custom variable.

## 0.2.1 (2023-11-21)

* Remame `jsx-mode` to `jtsx-jsx-mode` and `tsx-mode` to `jtsx-tsx-mode`. Keep `jsx-mode` and `tsx-mode` for backward compatibility but mark them as obsolete.
