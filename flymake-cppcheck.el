;;; flymake-cppcheck.el --- C/C++ code linter with cppcheck  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Martin Kjær Jørgensen (shaohme) <mkj@gotu.dk>
;;
;; Author: Martin Kjær Jørgensen <mkj@gotu.dk>
;; Created: 15 December 2021
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/shaohme/flymake-cppcheck
;;; Commentary:

;; This package adds Markdown syntax checker using cppcheck.
;; Make sure 'cppcheck' executable is on your path.

;; SPDX-License-Identifier: GPL-3.0-or-later

;; flymake-cppcheck is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; flymake-cppcheck is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with flymake-cppcheck.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'flymake)

(defgroup flymake-cppcheck nil
  "Cppcheck backend for Flymake."
  :prefix "flymake-cppcheck-"
  :group 'tools)

(defcustom flymake-cppcheck-path
  (executable-find "cppcheck")
  "The path to the `cppcheck' executable."
  :type 'string)

(defvar-local flymake-cppcheck--proc nil)

(defun flymake-cppcheck (report-fn &rest _args)
  "Flymake backend for cppcheck report using REPORT-FN."
  (unless (and flymake-cppcheck-path
               (file-executable-p flymake-cppcheck-path))
    (error "Could not find cppcheck executable"))

  (when (process-live-p flymake-cppcheck--proc)
    (kill-process flymake-cppcheck--proc)
    (setq flymake-cppcheck--proc nil))

  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq
       flymake-cppcheck--proc
       (make-process
        :name "flymake-cppcheck"
        :noquery t
        :buffer (generate-new-buffer " *flymake-cppcheck*")
        :command (list flymake-cppcheck-path "--enable=style" "-j 1" "--language=c" "--platform=native" "--std=c89" "--template='{file}:{line}:{column}:{severity}:{id}:{message}'" "--quiet" (format "%s" (buffer-file-name)))
        :sentinel
        (lambda (proc _event)
          (when (eq 'exit (process-status proc))
            (unwind-protect
                (if (with-current-buffer source (eq proc flymake-cppcheck--proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (let ((diags))
                        (while (search-forward-regexp ".+?:\\([0-9]+\\):\\([0-9]+\\):\\(.*\\):\\(.*\\):\\(.*\\)$" nil t)
                          (let ((region (flymake-diag-region source (string-to-number (match-string 1)) (string-to-number (match-string 2))))
                                (error-type (match-string 3)))
                            ;; expect `region' to only have 2 values (start . end)
                            (push (flymake-make-diagnostic source
                                                           (car region)
                                                           (cdr region)
                                                           (cond ((equal error-type "error") :error)
                                                                 ((equal error-type "style") :warning)
                                                                 ((equal error-type "warning") :warning)
                                                                 ((equal error-type "information") :info)
                                                                 ((equal error-type "performance") :info)
                                                                 ((equal error-type "portability") :info))
                                                           (format "%s:%s" (match-string 4) (match-string 5))) diags)))
                        (funcall report-fn (reverse diags))))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              (kill-buffer (process-buffer proc))))))))))

;;;###autoload
(defun flymake-cppcheck-setup ()
  "Enable cppcheck flymake backend."
  (add-hook 'flymake-diagnostic-functions #'flymake-cppcheck nil t))

(provide 'flymake-cppcheck)
;;; flymake-cppcheck.el ends here
