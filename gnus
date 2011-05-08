;; My gnus init file, which is of type -*-lisp-*-.

(setq gnus-select-method '(nnmbox ""))
(setq nnmbox-mbox-file "~/.emacs.d/fakemail.mbox")

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-topic-mode-hook
	(lambda ()
		(gnus-define-keys gnus-topic-mode-map
			[tab] gnus-topic-read-group)))

(setq gnus-summary-line-format
      "%U%R%z%(%-20,20n%): %I%s\n")
(setq gnus-group-line-format
      "%M%S%5y: %(%g%)\n")
(setq gnus-summary-same-subject "+->")

(add-hook 'gnus-summary-mode-hook
	  (lambda ()
	    (local-set-key [tab] 'gnus-summary-next-unread-article)
	    (local-set-key "E" 'gnus-summary-edit-article)
	    (local-set-key "e" 'gnus-summary-mark-as-expirable)
	    (local-set-key "b" 'gnus-summary-prev-page)
	    (local-set-key "h" 'gnus-summary-show-all-headers)))
(setq gnus-visible-headers
      '("^From:"
      "^Subject"
      "^To"
      "^[Cc][Cc]"
      "^Date"
      "^[Xx]-[Uu]rl"
      "^Mail-Followup-To"))
(setq gnus-sorted-header-list
      '("^Date"
	"^From:"
	"^To"
	"^[Cc][Cc]"
	"^Subject"
	"^Mail-Followup-To"
	"^[Xx]-[Uu]rl"))
(setq gnus-use-adaptive-scoring nil)
(setq mail-header-separator "-- END HEADERS --")

(setq nnmail-expiry-wait 'immediate)
(setq gnus-prompt-before-saving t)

;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

(gnus-demon-add-handler 'gnus-group-get-new-news 15 t)
(gnus-demon-init)

