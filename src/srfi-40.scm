;;; STREAM -- LIBRARY OF SYNTAX AND FUNCTIONS TO MANIPULATE STREAMS

;;; A stream is a new data type, disjoint from all other data types, that
;;; contains a promise that, when forced, is either nil (a single object
;;; distinguishable from all other objects) or consists of an object (the
;;; stream element) followed by a stream.  Each stream element is evaluated
;;; exactly once, when it is first retrieved (not when it is created); once
;;; evaluated its value is saved to be returned by subsequent retrievals
;;; without being evaluated again.

;; STREAM-TYPE -- type of streams
;; STREAM? object -- #t if object is a stream, #f otherwise
(define-record-type stream-type
    (make-stream promise)
  stream?
  (promise stream-promise))

;;; UTILITY FUNCTIONS

;; STREAM-ERROR message -- print message then abort execution
					;  replace this with a call to the native error handler
					;  if stream-error returns, so will the stream library function that called it
(define (stream-error message) (display message) (newline) (car '()))

;;; LOW-LEVEL STREAM FUNCTIONS

;; STREAM-LOW-LEVEL-LAZY -- an "atomic" (delay (force ...))
(define-syntax stream-low-level-lazy
  (syntax-rules ()
    ((stream-low-level-lazy exp)
     (cons 'lazy (lambda () exp)))))

;; STREAM-LOW-LEVEL-EAGER -- make a value into a low-level promise
(define (stream-low-level-eager x)
  (make-stream (cons 'eager x)))

;; STREAM-LOW-LEVEL-DELAY -- make an expression into a low-level promise
(define-syntax stream-low-level-delay
  (syntax-rules ()
    ((stream-low-level-delay exp)
     (stream-low-level-lazy (stream-low-level-eager exp)))))

;; STREAM-LOW-LEVEL-FORCE -- force the value from a low-level promise
(define (stream-low-level-force promise)
  (let ((content (car promise)))
    (case (car content)
      ((eager) (cdr content))
      ((lazy)  (let* ((promise* (stream-promise ((cdr content))))
                      (content  (car promise)))
                 (if (not (eqv? (car content) 'eager))
                     (begin (set-car! content (car (car promise*)))
                            (set-cdr! content (cdr (car promise*)))
                            (set-car! promise* content)))
                 (stream-low-level-force promise))))))


;;; STREAM SYNTAX AND FUNCTIONS

;; STREAM-NULL -- the distinguished nil stream
(define stream-null (make-stream (stream-low-level-delay '())))

;; STREAM-CONS object stream -- primitive constructor of streams
(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons obj strm)
     (make-stream
      (stream-low-level-delay
       (if (not (stream? strm))
	   (stream-error "attempt to stream-cons onto non-stream")
	   (cons obj strm)))))))

;; STREAM-NULL? object -- #t if object is the null stream, #f otherwise
(define (stream-null? obj)
  (and (stream? obj) (null? (stream-low-level-force (stream-promise obj)))))

;; STREAM-PAIR? object -- #t if object is a non-null stream, #f otherwise
(define (stream-pair? obj)
  (and (stream? obj) (not (null? (stream-low-level-force (stream-promise obj))))))

;; STREAM-CAR stream -- first element of stream
(define (stream-car strm)
  (cond ((not (stream? strm)) (stream-error "attempt to take stream-car of non-stream"))
        ((stream-null? strm) (stream-error "attempt to take stream-car of null stream"))
        (else (car (stream-low-level-force (stream-promise strm))))))

;; STREAM-CDR stream -- remaining elements of stream after first
(define (stream-cdr strm)
  (cond ((not (stream? strm)) (stream-error "attempt to take stream-cdr of non-stream"))
        ((stream-null? strm) (stream-error "attempt to take stream-cdr of null stream"))
        (else (cdr (stream-low-level-force (stream-promise strm))))))

;; STREAM-DELAY object -- the essential stream mechanism
(define-syntax stream-delay
  (syntax-rules ()
    ((stream-delay expr)
     (make-stream
      (stream-low-level-lazy expr)))))

;; STREAM object ... -- new stream whose elements are object ...
(define (stream . objs)
  (let loop ((objs objs))
    (stream-delay
     (if (null? objs)
	 stream-null
	 (stream-cons (car objs) (loop (cdr objs)))))))

;; STREAM-UNFOLDN generator seed n -- n+1 streams from (generator seed)
(define (stream-unfoldn gen seed n)
  (define (unfold-result-stream gen seed)
    (let loop ((seed seed))
      (stream-delay
       (call-with-values
	   (lambda () (gen seed))
	 (lambda (next . results)
	   (stream-cons results (loop next)))))))
  (define (result-stream->output-stream result-stream i)
    (stream-delay
     (let ((result (list-ref (stream-car result-stream) i)))
       (cond ((pair? result)
	      (stream-cons (car result)
			   (result-stream->output-stream
			    (stream-cdr result-stream) i)))
	     ((not result)
	      (result-stream->output-stream (stream-cdr result-stream) i))
	     ((null? result) stream-null)
	     (else (stream-error "can't happen"))))))
  (define (result-stream->output-streams result-stream n)
    (let loop ((i 0) (outputs '()))
      (if (= i n)
	  (apply values (reverse outputs))
	  (loop (+ i 1)
		(cons (result-stream->output-stream result-stream i)
		      outputs)))))
  (result-stream->output-streams (unfold-result-stream gen seed) n))

;; STREAM-MAP func stream ... -- stream produced by applying func element-wise
(define (stream-map func . strms)
  (cond ((not (procedure? func)) (stream-error "non-functional argument to stream-map"))
        ((null? strms) (stream-error "no stream arguments to stream-map"))
        ((not (every stream? strms)) (stream-error "non-stream argument to stream-map"))
        (else (let loop ((strms strms))
                (stream-delay
		 (if (any stream-null? strms)
		     stream-null
		     (stream-cons (apply func (map stream-car strms))
				  (loop (map stream-cdr strms)))))))))

;; STREAM-FOR-EACH proc stream ... -- apply proc element-wise for side-effects
(define (stream-for-each proc . strms)
  (cond ((not (procedure? proc)) (stream-error "non-functional argument to stream-for-each"))
        ((null? strms) (stream-error "no stream arguments to stream-for-each"))
        ((not (every stream? strms)) (stream-error "non-stream argument to stream-for-each"))
        (else (let loop ((strms strms))
                (if (not (any stream-null? strms))
                    (begin (apply proc (map stream-car strms))
                           (loop (map stream-cdr strms))))))))

;; STREAM-FILTER pred? stream -- new stream including only items passing pred?
(define (stream-filter pred? strm)
  (cond ((not (procedure? pred?)) (stream-error "non-functional argument to stream-filter"))
        ((not (stream? strm)) (stream-error "attempt to apply stream-filter to non-stream"))
        (else (stream-unfoldn
	       (lambda (s)
		 (values
		  (stream-cdr s)
		  (cond ((stream-null? s) '())
			((pred? (stream-car s)) (list (stream-car s)))
			(else #f))))
	       strm
	       1))))
