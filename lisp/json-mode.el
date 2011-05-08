(defvar json-mode-hook nil)

(defconst json-font-lock-keywords
  (list
   '("\\('\\w*'\\)" . font-lock-builtin-face))
   "Highlighting expressions for JSON mode")

(defun json-indent-line () 
  "Indent current line of JSON"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (cond ((looking-at "\\s *[\]\}]")
	     (save-excursion
	       (end-of-line)
	       (backward-sexp)
	       (setq cur-indent (current-indentation))))
	    (t (save-excursion
		 (forward-line -1)
		 (if (looking-at ".*\[{[(\]\\s *$")
		     (setq cur-indent (+ (current-indentation) default-tab-width))
		   (setq cur-indent (current-indentation))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0)))))
   
(define-derived-mode json-mode fundamental-mode "JSON"
  "Major mode for editing JavaScript Object Notation files."
  (set (make-local-variable 'font-lock-defaults) '(json-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'json-indent-line))

(provide 'json-mode)