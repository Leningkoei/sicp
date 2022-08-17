;;;; Rational functions
;;;; 2022/08/13

;;; exercises/2-93
;;; exercises/2-94
;;; exercises/2-95

;;; We can solve the problem exhibited in exercise 2.95 if we use the following
;;; modification of the GCD algorithm (which really works only in the case of
;;; polynomials with integer coefficients). Before performing any polynomial
;;; division in the GCD computation, we multiply the dividend by an integer
;;; constant factor, chosen to guarantee that no fractions will arise during the
;;; division process. Our answer will thus differ from the actual GCD by an
;;; integer constant factor, but this does not matter in the case of reducing
;;; rational functions to lowest terms; the GCD will be used to divide both the
;;; numerator and denominator, so the integer constant factor will cancel not.

;;; More precisely, if `P` and `Q` are polynomials, let `O_1` be the order of
;;; `P` (i.e., the order of the largest term of `P`) and let `O_2` be the order
;;; of `Q`. Let `c` be the leading coefficient of `Q`. Then it can be shown
;;; that, if we multiply `P` by the `integerizing factor `c^(1 + O_1 - O_2), the
;;; resulting polynomial can be divided by `Q` by using the `div-terms`
;;; algorithm without introducing any fractions. The operation of multiplying
;;; the dividend by this constant and then dividing is sometimes called the
;;; `pseudodivision` of `P` by `Q`. The remainder of the division is called the
;;; `pseudoremainder`.

;;; exercises/2-96

;;; thus, here is how to reduce a rational function to lowest terms:
;;;
;;;   * Compute the GCD of the numerator and denominator, using the version of
;;;     `gcd-terms` from exercise 2.96.
;;;
;;;   * When you obtain the GCD, multiply both numerator and denominator by the
;;;     same integerizing factor before dividing through by GCD, so that
;;;     division by the GCD will not introduce any noninteger coefficients. As
;;;     the factor you can use the leading coefficient of the GCD raised to the
;;;     power `1 + O_1 - O_2`, where `O_2` is the order of the GCD and `O_1` is
;;;     the maximum of the orders of the numerator and denominator. This will
;;;     ensure that dividing the numerator and denominator by the GCD will not
;;;     introduce any fractions.
;;;
;;;   * The result of this operation will be a numerator and denominator with
;;;     integer coefficients. The coefficients will normally be very large
;;;     because of all of the integerizing factors, so the last step is to
;;;     remove the redundant factors by computing the (integer) greatest common
;;;     divisor of all the coefficients of numerator and the denominator and
;;;     dividing through by this factor.

;;; exercises/2-97
