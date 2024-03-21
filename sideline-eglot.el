;;; sideline-eglot.el --- Show eglot information with sideline  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-sideline/sideline-eglot
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (eglot "1.12.29") (sideline "0.1.0") (ht "2.4"))
;; Keywords: convenience eglot

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Show eglot information with sideline.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'ht)
(require 'eglot)
(require 'sideline)

(defgroup sideline-eglot nil
  "Show eglot information with sideline."
  :prefix "sideline-eglot-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-sideline/sideline-eglot"))

(defcustom sideline-eglot-code-actions-prefix "💡 "
  "Prefix to insert before the code action title.
This can be used to insert, for example, an unicode character: 💡"
  :type '(choice
          string
          (const :tag "Disabled" nil))
  :group 'sideline-eglot)

(defmacro sideline-eglot--inhibit-timeout (&rest body)
  "Execute BODY and avoid eglot timeout.."
  (declare (indent 0) (debug t))
  `(progn
     (advice-add 'y-or-n-p :override #'always)
     ,@body
     (advice-remove 'y-or-n-p #'always)))

(defvar sideline-eglot--ht-candidates nil
  "Holds candidates.")

(defvar sideline-eglot--callback)

(defun sideline-eglot--async-candidates (callback &rest _)
  "Request eglot's CALLBACK candidates."
  (setq sideline-eglot--callback callback)
  (jsonrpc-async-request
   (eglot-current-server)
   :textDocument/codeAction
   (list :textDocument (eglot--TextDocumentIdentifier)
         :range (list :start (eglot--pos-to-lsp-position (point))
                      :end (eglot--pos-to-lsp-position nil))
         :context
         `(:diagnostics
           [,@(cl-loop for diag in (flymake-diagnostics (point) nil)
                       when (cdr (assoc 'eglot-lsp-diag
                                        (eglot--diag-data diag)))
                       collect it)]))
   :success-fn
   (lambda (resp)
     (let ((actions (append resp nil)))
       (if sideline-eglot--ht-candidates
           (ht-clear sideline-eglot--ht-candidates)
         (setq sideline-eglot--ht-candidates (ht-create)))
       (dolist (row actions)
         (ht-set sideline-eglot--ht-candidates
                 (concat sideline-eglot-code-actions-prefix
                         (cl-getf row :title))
                 row))
       (funcall sideline-eglot--callback (ht-keys sideline-eglot--ht-candidates))))
   :deferred :textDocument/codeAction))

;;;###autoload
(defun sideline-eglot (command)
  "Eglot backend for sideline.

Argument COMMAND is required in sideline backend."
  (cl-case command
    (`candidates
     (when (eglot-current-server) ; check connection
       (cons :async #'sideline-eglot--async-candidates)))
    (`action
     (lambda (candidate &rest _)
       (when-let*
           ((matching-code-action (ht-get sideline-eglot--ht-candidates candidate))
            (command (cl-getf matching-code-action :command))
            (server (eglot-current-server)))
         (sideline-eglot--inhibit-timeout
          (eglot-execute-command server
                                 (cl-getf command :command)
                                 (cl-getf command :arguments))))))))

(provide 'sideline-eglot)
;;; sideline-eglot.el ends here
