;;;;"array.scm" Arrays for Scheme
; Copyright (C) 2001, 2003 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;@code{(require 'array)} or @code{(require 'srfi-47)}
;;@ftindex array

;(require 'record)

(define array:rtd
  (make-record-type "array"
		    '(dimensions
		      scales		;list of dimension scales
		      offset		;exact integer
		      store		;data
		      )))

(define array:dimensions
  (let ((dimensions (record-accessor array:rtd 'dimensions)))
    (lambda (array)
      (cond ((vector? array) (list (vector-length array)))
	    ((string? array) (list (string-length array)))
	    (else (dimensions array))))))

(define array:scales
  (let ((scales (record-accessor array:rtd 'scales)))
    (lambda (obj)
      (cond ((string? obj) '(1))
	    ((vector? obj) '(1))
	    (else (scales obj))))))

(define array:store
  (let ((store (record-accessor array:rtd 'store)))
    (lambda (obj)
      (cond ((string? obj) obj)
	    ((vector? obj) obj)
	    (else (store obj))))))

(define array:offset
  (let ((offset (record-accessor array:rtd 'offset)))
    (lambda (obj)
      (cond ((string? obj) 0)
	    ((vector? obj) 0)
	    (else (offset obj))))))

(define array:construct
  (record-constructor array:rtd '(dimensions scales offset store)))

;;@args obj
;;Returns @code{#t} if the @1 is an array, and @code{#f} if not.
(define array?
  (let ((array:array? (record-predicate array:rtd)))
    (lambda (obj) (or (string? obj) (vector? obj) (array:array? obj)))))

;;@noindent
;;@emph{Note:} Arrays are not disjoint from other Scheme types.  Strings
;;and vectors also satisfy @code{array?}.  A disjoint array predicate can
;;be written:
;;
;;@example
;;(define (strict-array? obj)
;;  (and (array? obj) (not (string? obj)) (not (vector? obj))))
;;@end example

;;@body
;;Returns @code{#t} if @1 and @2 have the same rank and dimensions and the
;;corresponding elements of @1 and @2 are @code{equal?}.

;;@body
;;@0 recursively compares the contents of pairs, vectors, strings, and
;;@emph{arrays}, applying @code{eqv?} on other objects such as numbers
;;and symbols.  A rule of thumb is that objects are generally @0 if
;;they print the same.  @0 may fail to terminate if its arguments are
;;circular data structures.
;;
;;@example
;;(equal? 'a 'a)                         @result{}  #t
;;(equal? '(a) '(a))                     @result{}  #t
;;(equal? '(a (b) c)
;;        '(a (b) c))                    @result{}  #t
;;(equal? "abc" "abc")                   @result{}  #t
;;(equal? 2 2)                           @result{}  #t
;;(equal? (make-vector 5 'a)
;;        (make-vector 5 'a))            @result{}  #t
;;@emph{(equal? (make-array (Au32 4) 5 3)
;;        (make-array (Au32 4) 5 3))     @result{}  #t}
;;@emph{(array=? (make-array '#(foo) 3 3)
;;         (make-array '#(foo) 3 3))     @result{}  #t}
;;(equal? (lambda (x) x)
;;        (lambda (y) y))                @result{}  /unspecified/
;;@end example
(define (equal? obj1 obj2)
  (cond ((eqv? obj1 obj2) #t)
	((or (pair? obj1) (pair? obj2))
	 (and (pair? obj1) (pair? obj2)
	      (equal? (car obj1) (car obj2))
	      (equal? (cdr obj1) (cdr obj2))))
	((or (string? obj1) (string? obj2))
	 (and (string? obj1) (string? obj2)
	      (string=? obj1 obj2)))
	((or (vector? obj1) (vector? obj2))
	 (and (vector? obj1) (vector? obj2)
	      (equal? (vector-length obj1) (vector-length obj2))
	      (do ((idx (+ -1 (vector-length obj1)) (+ -1 idx)))
		  ((or (negative? idx)
		       (not (equal? (vector-ref obj1 idx)
				    (vector-ref obj2 idx))))
		   (negative? idx)))))
	((or (array? obj1) (array? obj2))
	 (and (array? obj1) (array? obj2)
	      (equal? (array:dimensions obj1) (array:dimensions obj2))
	      (equal? (array:store obj1) (array:store obj2))))
	(else #f)))

;;@args prototype k1 k2 @dots{}
;;
;;Creates and returns an array of type @1 with dimensions @2, @3,
;;@dots{} and filled with elements from @1.  @1 must be an array,
;;vector, or string.  The implementation-dependent type of the returned
;;array will be the same as the type of @1; except if that would be a
;;vector or string with non-zero origin, in which case some variety of
;;array will be returned.
;;
;;If the @1 has no elements, then the initial contents of the returned
;;array are unspecified.  Otherwise, the returned array will be filled
;;with the element at the origin of @1.
(define (make-array prototype . dimensions)
  (define tcnt (apply * dimensions))
  (do ((dims (reverse (cdr dimensions)) (cdr dims))
       (scales '(1) (cons (* (car dims) (car scales)) scales)))
      ((null? dims)
       (array:construct
	dimensions
	scales
	0
	(if (string? prototype)
	    (case (string-length prototype)
	      ((0) (make-string tcnt))
	      (else (make-string tcnt
				 (string-ref prototype 0))))
	    (let ((pdims (array:dimensions prototype)))
	      (case (apply * pdims)
		((0) (make-vector tcnt))
		(else (make-vector tcnt
				   (apply array-ref prototype
					  (map (lambda (x) 0) pdims)))))))))))
;;@args prototype k1 k2 @dots{}
;;@0 is an alias for @code{make-array}.
(define create-array make-array)

;;@noindent
;;These functions return a prototypical uniform-array enclosing the
;;optional argument (which must be of the correct type).  If the
;;uniform-array type is supported by the implementation, then it is
;;returned; defaulting to the next larger precision type; resorting
;;finally to vector.

(define (make-prototype-checker name pred? creator)
  (lambda args
    (case (length args)
      ((1) (if (pred? (car args))
	       (creator (car args))
	       (slib:error name 'incompatible 'type (car args))))
      ((0) (creator))
      (else (slib:error name 'wrong 'number 'of 'args args)))))

(define (integer-bytes?? n)
  (lambda (obj)
    (and (integer? obj)
	 (exact? obj)
	 (or (negative? n) (not (negative? obj)))
	 (do ((num obj (quotient num 256))
	      (n (+ -1 (abs n)) (+ -1 n)))
	     ((or (zero? num) (negative? n))
	      (zero? num))))))

;;@args z
;;@args
;;Returns a high-precision complex uniform-array prototype.
(define Ac64 (make-prototype-checker 'Ac64 complex? vector))
;;@args z
;;@args
;;Returns a complex uniform-array prototype.
(define Ac32 (make-prototype-checker 'Ac32 complex? vector))

;;@args x
;;@args
;;Returns a high-precision real uniform-array prototype.
(define Ar64 (make-prototype-checker 'Ar64 real? vector))
;;@args x
;;@args
;;Returns a real uniform-array prototype.
(define Ar32 (make-prototype-checker 'Ar32 real? vector))

;;@args n
;;@args
;;Returns an exact signed integer uniform-array prototype with at least
;;64 bits of precision.
(define As64 (make-prototype-checker 'As64 (integer-bytes?? -8) vector))
;;@args n
;;@args
;;Returns an exact signed integer uniform-array prototype with at least
;;32 bits of precision.
(define As32 (make-prototype-checker 'As32 (integer-bytes?? -4) vector))
;;@args n
;;@args
;;Returns an exact signed integer uniform-array prototype with at least
;;16 bits of precision.
(define As16 (make-prototype-checker 'As16 (integer-bytes?? -2) vector))
;;@args n
;;@args
;;Returns an exact signed integer uniform-array prototype with at least
;;8 bits of precision.
(define As8 (make-prototype-checker 'As8 (integer-bytes?? -1) vector))

;;@args k
;;@args
;;Returns an exact non-negative integer uniform-array prototype with at
;;least 64 bits of precision.
(define Au64 (make-prototype-checker 'Au64 (integer-bytes?? 8) vector))
;;@args k
;;@args
;;Returns an exact non-negative integer uniform-array prototype with at
;;least 32 bits of precision.
(define Au32 (make-prototype-checker 'Au32 (integer-bytes?? 4) vector))
;;@args k
;;@args
;;Returns an exact non-negative integer uniform-array prototype with at
;;least 16 bits of precision.
(define Au16 (make-prototype-checker 'Au16 (integer-bytes?? 2) vector))
;;@args k
;;@args
;;Returns an exact non-negative integer uniform-array prototype with at
;;least 8 bits of precision.
(define Au8 (make-prototype-checker 'Au8 (integer-bytes?? 1) vector))

;;@args bool
;;@args
;;Returns a boolean uniform-array prototype.
(define At1 (make-prototype-checker 'At1 boolean? vector))

;;@args array mapper k1 k2 @dots{}
;;@0 can be used to create shared subarrays of other
;;arrays.  The @var{mapper} is a function that translates coordinates in
;;the new array into coordinates in the old array.  A @var{mapper} must be
;;linear, and its range must stay within the bounds of the old array, but
;;it can be otherwise arbitrary.  A simple example:
;;
;;@example
;;(define fred (make-array '#(#f) 8 8))
;;(define freds-diagonal
;;  (make-shared-array fred (lambda (i) (list i i)) 8))
;;(array-set! freds-diagonal 'foo 3)
;;(array-ref fred 3 3)
;;   @result{} FOO
;;(define freds-center
;;  (make-shared-array fred (lambda (i j) (list (+ 3 i) (+ 3 j)))
;;                     2 2))
;;(array-ref freds-center 0 0)
;;   @result{} FOO
;;@end example
(define (make-shared-array array mapper . dimensions)
  (define odl (array:scales array))
  (define rank (length dimensions))
  (define shape
    (map (lambda (dim) (if (list? dim) dim (list 0 (+ -1 dim)))) dimensions))
  (do ((idx (+ -1 rank) (+ -1 idx))
       (uvt (append (cdr (vector->list (make-vector rank 0))) '(1))
	    (append (cdr uvt) '(0)))
       (uvts '() (cons uvt uvts)))
      ((negative? idx)
       (let ((ker0 (apply + (map * odl (apply mapper uvt)))))
	 (array:construct
	  (map (lambda (dim) (+ 1 (- (cadr dim) (car dim)))) shape)
	  (map (lambda (uvt) (- (apply + (map * odl (apply mapper uvt))) ker0))
	       uvts)
	  (apply +
		 (array:offset array)
		 (map * odl (apply mapper (map car shape))))
	  (array:store array))))))

;;@body
;;Returns the number of dimensions of @1.  If @1 is not an array, 0 is
;;returned.
(define (array-rank obj)
  (if (array? obj) (length (array:dimensions obj)) 0))

;;@body
;;Returns a list of dimensions.
;;
;;@example
;;(array-dimensions (make-array '#() 3 5))
;;   @result{} (3 5)
;;@end example
(define array-dimensions array:dimensions)

(define (array:in-bounds? array indices)
  (do ((bnds (array:dimensions array) (cdr bnds))
       (idxs indices (cdr idxs)))
      ((or (null? bnds)
	   (null? idxs)
	   (not (integer? (car idxs)))
	   (not (< -1 (car idxs) (car bnds))))
       (and (null? bnds) (null? idxs)))))

;;@args array index1 index2 @dots{}
;;Returns @code{#t} if its arguments would be acceptable to
;;@code{array-ref}.
(define (array-in-bounds? array . indices)
  (array:in-bounds? array indices))

;;@args array k1 k2 @dots{}
;;Returns the (@2, @3, @dots{}) element of @1.
(define (array-ref array . indices)
  (define store (array:store array))
  (or (array:in-bounds? array indices)
      (slib:error 'array-ref 'bad-indices indices))
  ((if (string? store) string-ref vector-ref)
   store (apply + (array:offset array) (map * (array:scales array) indices))))

;;@args array obj k1 k2 @dots{}
;;Stores @2 in the (@3, @4, @dots{}) element of @1.  The value returned
;;by @0 is unspecified.
(define (array-set! array obj . indices)
  (define store (array:store array))
  (or (array:in-bounds? array indices)
      (slib:error 'array-set! 'bad-indices indices))
  ((if (string? store) string-set! vector-set!)
   store (apply + (array:offset array) (map * (array:scales array) indices))
   obj))
