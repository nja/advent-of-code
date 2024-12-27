;;;; maths.lisp

(in-package :maths)

(defun egcd (a b)
  "Returns (values gcd s t) where s and t are the Bézout coefficients
such that gcd(a, b) = as + bt.
https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm"
  (loop for q = nil then (truncate pr r)
        for r = b then (- pr (* q r)) and pr = a then r
        for s = 0 then (- ps (* q s)) and ps = 1 then s
        for t* = 1 then (- pt (* q t*)) and pt = 0 then t*
        while (plusp r)
        finally (return (values pr ps pt))))

(defun invmod (a m)
  "Modular multiplicative inverse x such that ax ≡ 1 (mod m). Undefined
unless a and m are coprime."
  (multiple-value-bind (r s) (egcd (mod a m) m)
    (and (= r 1) s)))

(defun exptmod (n exponent modulus)
  "As (mod (expt n exponent) modulus), but more efficient."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))  
  (loop with result = 1
        for i of-type fixnum from 0 below (integer-length exponent)
        for sqr = n then (mod (* sqr sqr) modulus)
        when (logbitp i exponent) do
        (setf result (mod (* result sqr) modulus))
        finally (return result)))
