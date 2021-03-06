;;;; 3-44
;;;; 3-4-2
;;;; 2022/06/30

;;; Consider the problem of transferring an amount from one account to
;;; another. Ben Bitdiddle claims that this can be accomplished with the
;;; following procedure, even if there are multiple people concurrently
;;; transferring money among multiple accounts, using any account mechanism that
;;; serializes deposit and withdrawal transactions, for example, the version of
;;; `make-account` in the text above.

(defun transfer (from-account to-account amount)
  (funcall (funcall from-account 'withdraw) amount)
  (funcall (funcall to-account   'deposit)  amount))

;;; Louis Reasoner claims that there is a problem here, and that we need to use
;;; a more sophisticated method, such as the one required for dealing with the
;;; exchange problem. Is Louis right? If not, what is the essential difference
;;; between the transfer problem and the exchange problem? (You should assume
;;; that the balance in `from-account` is at least `amount.)

;;; Answer from Internet:

;; Louis is wrong.
;; `transfer` accesses each account's balance only once.
