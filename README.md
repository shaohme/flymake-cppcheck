# flymake-cppcheck

A Flymake backend for validating c/c++ files for Emacs (26+), using
[cppcheck](https://cppcheck.sourceforge.io/)

## Installation

`flymake-cppcheck` is not available on MELPA, so you have to add
it using your `load-path` manually.

## Usage

Add the following to your `.emacs` files for Emacs to load the backend
when visiting a c/c++ file

```elisp
(require 'flymake-cppcheck)

(add-hook 'cc-mode-hook 'flymake-cppcheck-setup)
```

Remember to enable `flymake-mode` as well, preferably after.

## License

Distributed under the GNU General Public License, version 3.
