;; lynx.el                -*- Emacs-Lisp -*-

;; lynx in a buffer mode

;; Created:    <Wed Oct 25 15:33:40 EDT 2000>
;; Time-stamp: <Mon Oct 30 18:07:33 EST 2000>
;; Author:     Alex Shinn <foof@debian.org>
;; Version:    0.1
;; Keywords:   lynx,http,web,browser

;; Copyright (C) 2000 Alex Shinn

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


;;; Support Emacs versions without custom
(eval-when-compile
  (if (fboundp 'defgroup)
      (defgroup lynx nil
	"Lynx Mode Settings"
	:group 'applications))
  (unless (fboundp 'defcustom)
    (defmacro defcustom (sym val &optional doc &rest args)
      `(defvar sym val doc))))

;;; Preliminary variables

(defcustom lynx-homepage "http://localhost/"
  "File to load default settings from"
  :group 'lynx
  :type  'string)

;(defcustom lynx-conf-file "~/.lynx.el"
;  "File to load default settings from"
;  :group 'lynx
;  :type  'string)

(defcustom lynx-program "/usr/bin/lynx"
  "Location of lynx program"
  :group 'lynx
  :type  'string)

(defcustom lynx-args nil
  "Arguments to pass to the lynx sub-process"
  :group 'lynx
  :type  'list)

(defcustom lynx-debug-buffer "*lynx-debug*"
  "Buffer to send Lynx debug output to"
  :group 'lynx
  :type  'string)

(defcustom lynx-error-buffer "*lynx-errors*"
  "Buffer to log errors into"
  :group 'lynx
  :type  'string)

;(defvar lynx-process nil
;  "Currently running lynx subprocess")

;(defvar lynx-unparsed-output ""
;  "Last incomplete line passed to lynx output filter")

(defvar lynx-mode-hooks nil
  "Hooks to run when a page is fetched in Lynx mode" )

(defvar lynx-font-keywords nil
  "Font lock keywords for `lynx-mode'." )

;; Just highlight links?
(unless lynx-font-keywords
  (setq lynx-font-keywords
        (list (list "\\[[0-9]+\\]" 0 'font-lock-type-face)) ))

(defvar lynx-mode-syntax-table nil
  "Syntax table used in `lynx-mode' buffers.")

;; Default syntax table?
(if lynx-mode-syntax-table nil
  (setq lynx-mode-syntax-table (make-syntax-table)) )

(defvar lynx-mode-map nil
  "Keymap used in `lynx-mode' buffers.")

(if lynx-mode-map nil
  (setq lynx-mode-map (make-keymap))
  (define-key lynx-mode-map " "            'lynx-open-link)
  (define-key lynx-mode-map "\C-m"         'lynx-open-link)
  (define-key lynx-mode-map "\C-i"         'lynx-next-link)
  (define-key lynx-mode-map [(tab)]        'lynx-next-link)
  (define-key lynx-mode-map [(shift tab)]  'lynx-prev-link)
  (define-key lynx-mode-map ?q             'bury-buffer)
 )

(defun lynx-append-to-buffer (buffer message)
  "Append a string to a buffer"
  (save-excursion
    (set-buffer (or (and (bufferp buffer) buffer)
                    (get-buffer-create buffer)))
    (end-of-buffer)
    (insert message) ))

(defmacro lynx-debug (message)
  "Send debug output to lynx-debug-buffer"
  `(lynx-append-to-buffer ,lynx-debug-buffer ,message) )

(defmacro lynx-error (message)
  "Send error output to lynx-error-buffer"
  `(lynx-append-to-buffer ,lynx-error-buffer ,message) )

(defun lynx-next-link ()
  "Jump to the next link in the buffer"
  (interactive)
  (if (looking-at "\\[\\([0-9]+\\)\\]")
      (forward-char) )
  (if (re-search-forward "[ \t]*\\(\\[\\([0-9]+\\)\\]\\)" nil t)
      (goto-char (match-beginning 1)) ))

(defun lynx-prev-link ()
  "Jump to the previous link in the buffer"
  (interactive)
  (re-search-backward "[ \t]*\\[\\([0-9]+\\)\\]" nil t) )

(defun lynx-open-link (number)
  "Open a numbered link in the current page"
  (interactive
   (list (if (or (looking-at "[ \t]*\\[\\([0-9]+\\)\\]")
                 (re-search-backward "[ \t]*\\[\\([0-9]+\\)\\]" nil t) )
             (buffer-substring (match-beginning 1) (match-end 1))
           ;; Lazy, goto 0 which is an error for now
           0 )))
  (let ((url
         (save-restriction
           (save-excursion
             (widen)
             (end-of-buffer)
             (if (re-search-backward
                  (format "^[ \t]*%s.[ \t]+\\([^ \t]+\\)$" number)
                  nil t)
                 (buffer-substring (match-beginning 1) (match-end 1)) )))))
    (if url
        (lynx-open url)
      (message (format "error: couldn't find link #%s" number)) )))

(defun lynx-open (url)
  (interactive "sOpen url: ")
  ;; Using lynx -dump for now... maybe run in a subprocess later
;  (if lynx-process
;      (progn (set-process-filter lynx-process nil)
;             (delete-process lynx-process)))
;  (setq lynx-process (eval `(start-process "lynx" nil
;                                            ,lynx-program
;                                            "-dump" ,url
;                                            ,@lynx-args)))
;  (set-process-filter lynx-process 'lynx-filter)
  (let ((buf (generate-new-buffer url)))
;    (shell-command (format "%s %s -width %d -dump %s" lynx-program
;                           lynx-args (window-width) url)
;                   buf)
    (switch-to-buffer buf)
    (lynx-mode)
    (let ((process
           (eval `(start-process "lynx" (current-buffer) lynx-program
                                 ,@lynx-args
                                 "-width"
                                 (number-to-string (window-width))
                                 "-dump" url))))
      (set-process-filter process 'lynx-filter)
      (set-process-sentinel process 'lynx-sentinel) )))

(defun lynx-filter (process output)
  "Filter for lynx processes"
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert output) )))

(defun lynx-sentinel (process event)
  "Sentinel for lynx processes"
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (if (re-search-backward "^[ \t]*References[ \t]*$" nil t)
        (narrow-to-region (point-min) (point)) )))

;;; The Lynx Mode

(defun lynx-mode ()
  "Mode for viewing Lynx output"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table lynx-mode-syntax-table)
  (setq major-mode 'lynx-mode
	mode-name "Lynx")
  (use-local-map lynx-mode-map)
  (setq buffer-read-only t)
  (run-hooks 'lynx-mode-hooks) )


(defalias 'lynx 'lynx-open)
(provide 'lynx)
