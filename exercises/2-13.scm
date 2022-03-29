;;;; 2-13
;;;; 2-1-4
;;;; 2022/03/29

;;; Show that under the assumption of small percentage tolerances there is a
;;; simple formula for the approximate percentage tolerance of the product of
;;; two intervals in terms of the tolerances of the factors. You may simplify
;;; the problem by assuming that all numbers are positive.

;;; Define: Cx is center of x, Tx is tolerance of x.
;;; a * b = (Ca * Cb * (1 - (0.5 * Ta + 0.5 * Tb) + 0.5 * Ta * 0.5 * Tb),
;;;          Ca * Cb * (1 + (0.5 * Ta + 0.5 * Tb) + 0.5 * Ta * 0.5 * Tb))
;;; Ignore Ta * Tb. Ta * Tb = 0.
;;; a * b = (Ca * Cb, Ca * Cb)

