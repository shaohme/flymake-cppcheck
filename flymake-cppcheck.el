;;; flymake-cppcheck.el --- C/C++ code linter with cppcheck  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Martin Kjær Jørgensen (shaohme) <mkj@gotu.dk>
;;
;; Author: Martin Kjær Jørgensen <mkj@gotu.dk>
;; Created: 15 December 2021
;; Version: 0.1.5
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/shaohme/flymake-cppcheck
;;; Commentary:

;; This package adds Markdown syntax checker using cppcheck.
;; Make sure 'cppcheck' executable is on your path.

;; `flymake-cppcheck' is not tested on Windows systems and might not
;; work properly.

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
(require 'json)


(defgroup flymake-cppcheck nil
  "Cppcheck backend for Flymake."
  :prefix "flymake-cppcheck-"
  :group 'tools)

(defcustom flymake-cppcheck-program (executable-find "cppcheck")
  "The path to the cppcheck executable."
  :type 'string)

(defcustom flymake-cppcheck-use-headers t
  "Tell 'cppcheck' to use headers when checking.
If non-nil `flymake-cppcheck' will try include headers when
checking buffers.  This can be useful to disable if header checks
in 'cppcheck' produces too many errors or otherwise fails."
  :type 'boolean)

(defcustom flymake-cppcheck-additional-checks '(warning style performance portability information)
  "Additional checks to enable."
  :type '(repeat (symbol))
  :options '((const warning)
             (const style)
             (const performance)
             (const portability)
             (const information)))

(defcustom flymake-cppcheck-language nil
  "Force particular language."
  :type '(choice (const c)
                 (const c++)))

(defcustom flymake-cppcheck-jobs 4
  "Number of jobs to check with."
  :type 'integer)

(defcustom flymake-cppcheck-std nil
  "Target C/C++ standard."
  :type '(choice (const c89)
                 (const c99)
                 (const c11)
                 (const c++03)
                 (const c++11)
                 (const c++14)
                 (const c++17)
                 (const c++20)))

(defcustom flymake-cppcheck-max-configs 32
  "Maximum number of configurations to check in a file before skipping it."
  :type 'integer)

(defcustom flymake-cppcheck-global-suppress '("missingIncludeSystem" "*:*include/tau/*")
  "Suppress list added to all buffers."
  :type 'list)


(defvar-local flymake-cppcheck--proc nil)


(defun shell-command-line-to-argument-list (command-line)
  (let (args arg inquote)
    (with-temp-buffer
      (insert command-line)
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at "\\s-+")
               (cond ((not inquote)
                      (when arg (push arg args))
                      (setq arg nil))
                     (t (setq arg (concat arg (match-string 0))))))
              ((looking-at "['\"]")
               (let ((ch (match-string 0)))
                 (cond ((not inquote)
                        (setq inquote ch)
                        (setq arg (or arg "")))
                       ((equal inquote ch)
                        (setq inquote nil))
                       (t (setq arg (concat arg ch))))))
              ((or (looking-at "\\\\\\(.?\\)") (looking-at "\\(.\\)"))
               (setq arg (concat (or arg "") (match-string 1)))))
        (goto-char (match-end 0)))
      (when inquote (error "Missing closing quote"))
      (when arg (push arg args))
      (nreverse args))))


(defun flymake-cppcheck (report-fn &rest _args)
  "Flymake backend for cppcheck report using REPORT-FN."
  ;; break if no cppcheck program is set
  (if (not flymake-cppcheck-program)
      (error "No cppcheck program name set"))
  ;; check if cppcheck exists
  (let ((flymake-cppcheck--executable-path (executable-find flymake-cppcheck-program)))
    (if (or (null flymake-cppcheck--executable-path)
            (not (file-executable-p flymake-cppcheck--executable-path)))
        (error "Could not find '%s' executable" flymake-cppcheck-program))
    ;; try derive the cppcheck version. if too old, command line
    ;; args might not work.
    (let* ((output (shell-command-to-string (concat flymake-cppcheck-program " --version")))
           (version (split-string (car (cdr (split-string output " " t))) "\\."))
           (major-version (string-to-number (car version)))
           (minor-version (string-to-number (car (cdr version)))))
      ;; kill already running cppcheck process
      (when (process-live-p flymake-cppcheck--proc)
        (kill-process flymake-cppcheck--proc)
        (setq flymake-cppcheck--proc nil))
      (if (and (= major-version 2)
               (< minor-version 9))
          (error "Cppcheck version %d.%d too old" major-version minor-version))
      ;; prepare cppcheck command for later
      (let* ((source (current-buffer))
             (source-file-name (buffer-file-name source))
             (cppcheck-args (list
                             ;; (shell-command-to-string "echo | cc -v -x c -E -") could be
                             ;; used to find system includes if needed
                             ;; file includes are expected to be full file paths to have -I appended to them
                             (if flymake-cppcheck-additional-checks (format "--enable=%s" (mapconcat 'symbol-name flymake-cppcheck-additional-checks ",")) "")
                             (if flymake-cppcheck-jobs (format "-j %d" flymake-cppcheck-jobs) "")
                             ;; assume a language. if not try deduce it from file extension
                             (format "%s=%s" "--language" (if flymake-cppcheck-language
                                                              flymake-cppcheck-language
                                                            (if (and source-file-name
                                                                     (member (file-name-extension source-file-name)  '("cpp" "cc" "C" "c++" "cxx" "hpp" "hh"))) "c++" "c")))
                             (if flymake-cppcheck-std (format "--std=%s" flymake-cppcheck-std) "")
                             (if flymake-cppcheck-max-configs (format "--max-configs=%d" flymake-cppcheck-max-configs) "")
                             (if (and (= major-version 2)
                                      (>= minor-version 11))
                                 "--check-level=exhaustive")
                             ;; enable inline suppressions in source files
                             "--inline-suppr"
                             ;; do not clutter stdout
                             "--quiet"
                             "--template='{file}:{line}:{column}:{severity}:{id}:{message}'"
                             (format "%s" source-file-name))))
        ;; if no includes or no std is customized try search for
        ;; sensible values from project files
        (dolist (x flymake-cppcheck-global-suppress)
          (setq cppcheck-args (append (list (format "--suppress=%s" x)) cppcheck-args)))
        (when (or flymake-cppcheck-use-headers (not flymake-cppcheck-std))
          ;; if "compile-commands.json" are found try deduce headers from it
          (let* ((comp-com-filename "compile_commands.json")
                 (comp-com-dir (locate-dominating-file source-file-name comp-com-filename)))
            ;; prepare full file name and check for readability
            (when (and comp-com-filename source-file-name)
              (let ((comp-com-file (expand-file-name comp-com-filename comp-com-dir)))
                (when (file-readable-p comp-com-file)
                  ;; file output is expected to be a json list.
                  (let* ((comp-com-json (json-read-file comp-com-file))
                         ;; search through it for objects with `file'
                         ;; property to check if it matches our current
                         ;; file
                         (file-obj (seq-find (lambda (elt) (if (string= (cdr (assoc 'file elt)) source-file-name) t nil)) comp-com-json)))
                    ;; json might not contain object matching the file
                    ;; being checked
                    (when file-obj
                      (let ((allargs (vconcat (shell-command-line-to-argument-list (cdr (assoc 'command file-obj))) (cdr (assoc 'arguments file-obj)))))
                        (when flymake-cppcheck-use-headers
                          ;; search `arguments' and `command' properties for all
                          ;; arguments starting with "-I". these are
                          ;; assumed to be header arguments.
                          ;; both are searched because not all build tools, like CMake, separate command from args.
                          ;; TODO: sometimes -I args have whitespace in between, like so "-I /a/b/c"
                          (setq cppcheck-args (append (seq-filter (lambda (elt) (string-prefix-p "-I" elt)) allargs) cppcheck-args)))
                        (setq cppcheck-args (append (seq-filter (lambda (elt) (string-prefix-p "-D" elt)) allargs) cppcheck-args))
                        (when (not flymake-cppcheck-std)
                          (let* ((std-out (car (seq-filter (lambda (elt) (string-prefix-p "-std=" elt)) allargs)))
                                 (std-arg (if std-out (car (cdr (split-string std-out "=")))
                                            nil)))
                            (if std-arg
                                ;; TODO: filter out incompatible std
                                ;; args. cppcheck might not be up to date
                                ;; with latest compilers.
                                (push (format "--std=%s" (cond ((string-prefix-p "gnu" std-arg) (format "c%s" (car (cdr (split-string std-arg "gnu")))))
                                                               (t std-arg))) cppcheck-args))))))))))))
        ;; lastly put the cppcheck program first in the args list for
        ;; execution later.
        (push flymake-cppcheck-program cppcheck-args)
        (save-restriction
          (widen)
          (setq
           flymake-cppcheck--proc
           (make-process
            :name "flymake-cppcheck"
            :noquery t
            :buffer (generate-new-buffer " *flymake-cppcheck*")
            :command cppcheck-args
            :sentinel
            (lambda (proc event)
              (when (eq 'exit (process-status proc))
                (unwind-protect
                    (if (with-current-buffer source (eq proc flymake-cppcheck--proc))
                        (with-current-buffer (process-buffer proc)
                          (let ((diags))
                            (if (string-prefix-p "exited abnormally" event)
                                (flymake-log :error (format "event='%s' command_list='%s' output='%s'" (string-trim event) cppcheck-args (buffer-string)) proc)
                              (progn
                                (goto-char (point-min))
                                (while (search-forward-regexp "\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\):\\(.*\\):\\(.*\\)$" nil t)
                                  (let* ((file-path (match-string 1)) ;full file path or special name
                                         ;; expect "region" to only have 2 values (start . end)
                                         (region (flymake-diag-region source (string-to-number (match-string 2)) (string-to-number (match-string 3))))
                                         (error-type-string (match-string 4)))
                                    ;; cppcheck will sometimes output
                                    ;; errors on other files than the
                                    ;; current buffer. filter those away.
                                    ;; "nofile" entries are ignored for now.
                                    (when (string-suffix-p source-file-name file-path)
                                      ;; do not treat "noValidConfiguration"
                                      ;; differently for now.
                                      (push (flymake-make-diagnostic source
                                                                     (car region)
                                                                     (cdr region)
                                                                     (cond ((equal error-type-string "error") :error)
                                                                           ((equal error-type-string "style") :note)
                                                                           ((equal error-type-string "warning") :warning)
                                                                           ((equal error-type-string "information") :note)
                                                                           ((equal error-type-string "performance") :note)
                                                                           ((equal error-type-string "portability") :note)
                                                                           (t :warning))
                                                                     (format "%s:%s" (match-string 5) (match-string 6))) diags))))))
                            (funcall report-fn (reverse diags))))
                      (flymake-log :debug "Canceling obsolete check %s" proc))
                  (kill-buffer (process-buffer proc))))))))))))

;;;###autoload
(defun flymake-cppcheck-setup ()
  "Enable cppcheck flymake backend."
  (add-hook 'flymake-diagnostic-functions #'flymake-cppcheck nil t))

(provide 'flymake-cppcheck)
;;; flymake-cppcheck.el ends here
