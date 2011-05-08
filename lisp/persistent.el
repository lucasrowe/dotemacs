;;; -*-Emacs-Lisp-*-
;;; persistent.el --- save variables persistently across sessions.

;; Copyright (C) Ashvin Goel, ash...@ficus.cs.ucla.edu.

;; Author: Ashvin Goel ash...@ficus.cs.ucla.edu.
;; Maintainer: Ashvin Goel
;; Created: August, 1995
;; Version: 1.1
;; Keywords: persistent, save session.

;;; Copyright (C) 1995
;;;%Header
;;; Saves variables persistently across sessions.
;;; Thanks to the saveplace package which does similar things.

;;; This file is part of GNU Emacs.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.
;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;;
;;; This package will save variables across emacs sessions. Some common
;;; variables to save are shown below. Typically I like to save history
;;; variables so that I can use history across sessions.
;;;
;;; Set the following in your session startup.
;;;
;;; (setq persistent-session-list '(read-expression-history
;;;                                extended-command-history
;;;                                find-tag-history
;;;                                query-replace-history
;;;                                grep-history
;;;                                minibuffer-history
;;;                                file-name-history
;;;                                compile-history))
;;; (require 'persistent)
;;; (persistent-session-load-from-alist)

(defvar persistent-session-list nil
  "*List of variables to be saved.
Look at persistent-session-alist, persistent-session-file.")

(defvar persistent-session-file "~/.emacs-persistent"
  "*Name of the file that records `persistent-session-alist' value.")

(defvar persistent-session-alist nil
  "Alist of variables and their saved values.
This alist is saved in the file `persistent-session-file'.")

(defvar persistent-session-loaded nil
  "Non-nil means that the `persistent-session-file' has been loaded.")

(defvar persistent-session-size 32
  "Max number of elements stored for each element stored by
`persistent-session-save-alist-to-file'.
Truncation does not occur if set to zero.")

(defun persistent-session-load-from-alist ()
  "Go through `persistent-session-alist' and set variables in
`persistent-session-list'."
  (interactive)
  (or persistent-session-loaded (persistent-session-load-alist-from-file))
  (mapcar (function
           (lambda (persistent-session)
             (set persistent-session
                  (cdr (assoc persistent-session persistent-session-alist)
                       ))))
          persistent-session-list))

(defun persistent-session-load-alist-from-file ()
  "Load `persistent-session-alist' from `persistent-session-file'."
  (if (not persistent-session-loaded)
      (progn
        (setq persistent-session-loaded t)
        (let ((file (expand-file-name persistent-session-file)))
          ;; make sure that the alist does not get overwritten, and then
          ;; load it if it exists:
          (if (file-readable-p file)
              (save-excursion
                (message (format "Loading persistent session from %s..."
                                 persistent-session-file))
                ;; don't want to use find-file because we have been
                ;; adding hooks to it.
                (set-buffer (get-buffer-create " *Saved Persistent Session*"))
                (delete-region (point-min) (point-max))
                (insert-file-contents file)
                (goto-char (point-min))
                (setq persistent-session-alist
                      (car (read-from-string
                            (buffer-substring (point-min) (point-max)))))
                (kill-buffer (current-buffer))
                (message (format "Loading persistent session from %s... done."
                                 file))
                t)
            t)
          nil))))

(defun persistent-session-truncate-alist ()
  "Truncate elements of `persistent-session-alist' to `persistent-session-size'."
  (if (> persistent-session-size 0)
      (mapcar (function
               (lambda (persistent-session)
                 (if (> (length persistent-session)
                        (+ persistent-session-size 1))
                     (setcdr (nthcdr persistent-session-size
                                     persistent-session) nil))))
              persistent-session-alist)))

(defun persistent-session-save-to-alist ()
  "Go through `persistent-session-list',
saving the variables to `persistent-session-alist'."
  (or persistent-session-loaded (persistent-session-load-alist-from-file))
  (mapcar (function
           (lambda (persistent-session)
             (setq persistent-session-alist
                   (delete
                    (assoc persistent-session persistent-session-alist)
                    persistent-session-alist))
             (setq persistent-session-alist
                   (cons (cons persistent-session
                               (if (boundp persistent-session)
                                   (eval persistent-session) nil))
                         persistent-session-alist))))
          persistent-session-list))

(defun persistent-session-save-alist-to-file ()
  "Save `persistent-session-alist' to `persistent-session-file'."
  (interactive)
  (let ((file (expand-file-name persistent-session-file)))
    (save-excursion
      (message (format "Saving persistent session to %s..." file))
      (set-buffer (get-buffer-create " *Saved Persistent Session*"))
      (delete-region (point-min) (point-max))
      (if (file-readable-p file)
          (insert-file-contents file))
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (persistent-session-save-to-alist)
      (persistent-session-truncate-alist)
      (print persistent-session-alist (current-buffer))
      (write-file file)
      (kill-buffer (current-buffer))
      (message (format "Saving persistent session to %s... done." file)))))

(add-hook 'kill-emacs-hook 'persistent-session-save-alist-to-file)

(provide 'persistent) 
