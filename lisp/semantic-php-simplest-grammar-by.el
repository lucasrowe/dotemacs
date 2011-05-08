;;; semantic-php-simplest-grammar-by.el --- Generated parser support file

;; Copyright (C) 2008 Kevin Kreamer

;; Author: Kevin Kreamer <kkreamer@kkreamer-mac.local>
;; Created: 2008-11-01 22:10:00-0400
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file semantic-php-simplest-grammar.by.

;;; History:
;;

;;; Code:

;;; Prologue
;;
(define-lex-simple-regex-analyzer
	semantic-lex-php-simplest-class-def nil
	"[ \t]*class[ \t]+[a-zA-Z0-9_]+"
	'class-def
	)
  (define-lex-simple-regex-analyzer
	semantic-lex-php-simplest-funct-def nil
	"[ \ta-zA-Z0-9_]*function[ \t]+[a-zA-Z0-9_]+"
	'funct-def
	)
  (define-lex-simple-regex-analyzer
	semantic-lex-php-simplest-notspace nil
	"[^ \t\n]+"
	'notspace
	)
  (define-lex
	semantic-lex-php-simplest
	nil
	semantic-lex-ignore-comments
	semantic-lex-php-simplest-class-def
	semantic-lex-php-simplest-funct-def
	semantic-lex-whitespace
	semantic-lex-newline
	semantic-lex-php-simplest-notspace
	semantic-lex-default-action
	)

;;; Declarations
;;
(defconst semantic-php-simplest-grammar-by--keyword-table
  (semantic-lex-make-keyword-table 'nil 'nil)
  "Table of language keywords.")

(defconst semantic-php-simplest-grammar-by--token-table
  (semantic-lex-make-type-table 'nil 'nil)
  "Table of lexical tokens.")

(defconst semantic-php-simplest-grammar-by--parse-table
  `(
    (bovine-toplevel 
     (script)
     ) ;; end bovine-toplevel

    (script
     (lines
      functions
      ,(semantic-lambda
        (semantic-tag-new-type
         "::"
         "class"
         (semantic-parse-region
          (car
           (car
            (nth 1 vals)))
          (cdr
           (car
            (nth 1 vals)))
          'functions-t
          1) nil))
      )
     (lines
      ,(semantic-lambda
        (semantic-tag-new-type
         "::"
         "class" nil nil))
      )
     (class-def
      lines
      functions
      ,(semantic-lambda
        (semantic-tag-new-type
         (replace-regexp-in-string
          ".*class[ 	]+"
          ""
          (nth 0 vals))
         "class"
         (semantic-parse-region
          (car
           (car
            (nth 2 vals)))
          (cdr
           (car
            (nth 2 vals)))
          'functions-t
          1) nil))
      )
     (class-def
      lines
      ,(semantic-lambda
        (semantic-tag-new-type
         (replace-regexp-in-string
          ".*class[ 	]+"
          ""
          (nth 0 vals))
         "class" nil nil))
      )
     ) ;; end script

    (anything
     (notspace
      anything)
     (whitespace
      anything)
     (notspace)
     (whitespace)
     ) ;; end anything

    (line
     (anything
      newline)
     (anything)
     (newline)
     ) ;; end line

    (lines
     (line
      lines)
     (line)
     ) ;; end lines

    (function
     (funct-def
      lines)
     ) ;; end function

    (functions
     (function
      functions
      ,(semantic-lambda
        (list
         (cons start end)))
      )
     (function
      ,(semantic-lambda
        (list
         (cons start end)))
      )
     ) ;; end functions

    (functions-t
     (funct-def
      lines
      ,(semantic-lambda
        (semantic-tag-new-function
         (replace-regexp-in-string
          "^[ 	]*"
          ""
          (nth 0 vals)) nil nil))
      )
     ) ;; end functions-t
    )
  "Parser table.")

(defun semantic-php-simplest-grammar-by--install-parser ()
  "Setup the Semantic Parser."
  (setq semantic--parse-table semantic-php-simplest-grammar-by--parse-table
        semantic-debug-parser-source "semantic-php-simplest-grammar.by"
        semantic-debug-parser-class 'semantic-bovine-debug-parser
        semantic-flex-keywords-obarray semantic-php-simplest-grammar-by--keyword-table
        ))


;;; Analyzers
;;
(require 'semantic-lex)


;;; Epilogue
;;

(provide 'semantic-php-simplest-grammar-by)

;;; semantic-php-simplest-grammar-by.el ends here
