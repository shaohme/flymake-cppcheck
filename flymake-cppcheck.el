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

(defcustom flymake-cppcheck-program
  (executable-find "cppcheck")
  "The path to the `cppcheck' executable."
  :type 'string)

(defcustom flymake-cppcheck-additional-checks
  "warning,style,performance,portability,information"
  "Additional checks to enable."
  :type 'string)

(defcustom flymake-cppcheck-language
  nil
  "Force particular language."
  :type 'string)

(defcustom flymake-cppcheck-jobs
  nil
  "Number of jobs to check with."
  :type 'integer)

(defcustom flymake-cppcheck-platform
  nil
  "Target platform to determine types and sizes."
  :type 'string)

(defcustom flymake-cppcheck-std
  nil
  "Target C/C++ standard."
  :type 'string)

(defvar-local flymake-cppcheck--proc nil)

(defun flymake-cppcheck (report-fn &rest _args)
  "Flymake backend for cppcheck report using REPORT-FN."
  (if (not flymake-cppcheck-program)
      (error "No cppcheck program name set"))
  (let ((flymake-cppcheck--executable-path (executable-find flymake-cppcheck-program)))
    (if (or (null flymake-cppcheck--executable-path)
            (not (file-executable-p flymake-cppcheck--executable-path)))
        (error "Could not find '%s' executable" flymake-cppcheck-program))
    (when (process-live-p flymake-cppcheck--proc)
      (kill-process flymake-cppcheck--proc)
      (setq flymake-cppcheck--proc nil))
    (let ((source (current-buffer))
          (cmd (list flymake-cppcheck-program
                     (if flymake-cppcheck-additional-checks (format "%s=%s" "--enable" flymake-cppcheck-additional-checks) "")
                     (if flymake-cppcheck-jobs (format "%s %d" "-j" flymake-cppcheck-jobs) "")
                     (if flymake-cppcheck-language (format "%s=%s" "--language" flymake-cppcheck-language) "")
                     (if flymake-cppcheck-platform (format "%s=%s" "--platform" flymake-cppcheck-platform) "")
                     (if flymake-cppcheck-std (format "%s=%s" "--std" flymake-cppcheck-std) "")
                     "--suppress=missingIncludeSystem"
                     "--suppress=unusedFunction"
                     "--quiet"
                     "--template='{file}:{line}:{column}:{severity}:{id}:{message}'"
                     (format "%s" (buffer-file-name)))))
      (message "OUT: %s" cmd)
      (save-restriction
        (widen)
        (setq
         flymake-cppcheck--proc
         (make-process
          :name "flymake-cppcheck"
          :noquery t
          :buffer (generate-new-buffer " *flymake-cppcheck*")
          :command cmd
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
                                                                   ((equal error-type "portability") :info)
                                                                   (t :warning))
                                                             (format "%s:%s" (match-string 4) (match-string 5))) diags)))
                          (funcall report-fn (reverse diags))))
                    (flymake-log :warning "Canceling obsolete check %s"
                                 proc))
                (kill-buffer (process-buffer proc)))))))))))

;;;###autoload
(defun flymake-cppcheck-setup ()
  "Enable cppcheck flymake backend."
  (add-hook 'flymake-diagnostic-functions #'flymake-cppcheck nil t))

(provide 'flymake-cppcheck)
;;; flymake-cppcheck.el ends here
