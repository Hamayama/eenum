;;
;; testing eenum
;;

(add-load-path "." :relative)
(use gauche.test)
(use math.const) ; for pi
(use srfi-13)    ; for string-tabulate
(use file.util)  ; for null-device

(test-start "eenum")
(use eenum)
(test-module 'eenum)

(define-syntax expr-test
  (syntax-rules ()
    ((_ txt ans expr)
     (test* (string-append txt " : " (format "~s" (quote expr))) ans expr))
    ((_ txt ans expr chk)
     (test* (string-append txt " : " (format "~s" (quote expr))) ans expr chk))))

(define (make-num-str n :optional (s 1))
  (string-tabulate
   (lambda (i) (integer->char (+ #x30 (modulo (+ i s) 10))))
   n))

(define old-out-port (current-output-port))

(test-section "number")
(expr-test "" "0"        (eenum 0))
(expr-test "" "123"      (eenum 123))
(expr-test "" "-123"     (eenum -123))
(expr-test "" "123"      (eenum "  123  "))
(expr-test "" "123"      (eenum "0123"))
(expr-test "" "-123"     (eenum "-0123"))
(expr-test "" "0"        (eenum "00"))

(test-section "exponent")
(expr-test "" "12.3"     (eenum "12.3e0"))
(expr-test "" "123"      (eenum "12.3e1"))
(expr-test "" "1230"     (eenum "12.3e2"))
(expr-test "" "1.23"     (eenum "12.3e-1"))
(expr-test "" "0.123"    (eenum "12.3e-2"))
(expr-test "" "0.0123"   (eenum "12.3e-3"))
(expr-test "" "0"        (eenum "0e1"))
(expr-test "" "0.0"      (eenum "0e-1"))
(expr-test "" "1"        (eenum "0.01e2"))
(expr-test "" "0.0100"   (eenum "1.00e-2"))

(test-section "width")
(expr-test "" " 123"     (eenum "123"     4))
(expr-test "" "0.0123"   (eenum "12.3e-3" 5))
(expr-test "" "0.0123"   (eenum "12.3e-3" 6))
(expr-test "" " 0.0123"  (eenum "12.3e-3" 7))

(test-section "digits")
(expr-test "" "123.0"    (eenum "123"     #f 1))
(expr-test "" "123.00"   (eenum "123"     #f 2))
(expr-test "" "0.012"    (eenum "12.3e-3" #f 3))
(expr-test "" "0.0123"   (eenum "12.3e-3" #f 4))
(expr-test "" "0.01230"  (eenum "12.3e-3" #f 5))

(test-section "plus-sign")
(expr-test "" "123"      (eenum "123"     #f #f #f))
(expr-test "" "123"      (eenum "+123"    #f #f #f))
(expr-test "" "-123"     (eenum "-123"    #f #f #f))
(expr-test "" "+123"     (eenum "123"     #f #f #t))
(expr-test "" "+123"     (eenum "+123"    #f #f #t))
(expr-test "" "-123"     (eenum "-123"    #f #f #t))

(test-section "pad-char")
(expr-test "" "00123"    (eenum "123"     5  #f #f #\0))
(expr-test "" "0-123"    (eenum "-123"    5  #f #f #\0))
(expr-test "" "0+123"    (eenum "123"     5  #f #t #\0))

(test-section "exponent marker")
(expr-test "" "1230"     (eenum "123e1"))
(expr-test "" "1230"     (eenum "123E1"))
(expr-test "" "1230"     (eenum "123s1"))
(expr-test "" "1230"     (eenum "123S1"))
(expr-test "" "1230"     (eenum "123f1"))
(expr-test "" "1230"     (eenum "123F1"))
(expr-test "" "1230"     (eenum "123d1"))
(expr-test "" "1230"     (eenum "123D1"))
(expr-test "" "1230"     (eenum "123l1"))
(expr-test "" "1230"     (eenum "123L1"))

(test-section "incomplete number")
(expr-test "" "+"        (eenum "+"))
(expr-test "" "-"        (eenum "-"))
(expr-test "" "."        (eenum "."))
(expr-test "" "0"        (eenum "0."))
(expr-test "" ".0"       (eenum ".0"))
(expr-test "" "e"        (eenum "e"))
(expr-test "" "0e"       (eenum "0e"))
(expr-test "" "e0"       (eenum "e0"))

(test-section "calculation")
(expr-test "" "+inf.0"   (eenum (/.  1 0)))
(expr-test "" "-inf.0"   (eenum (/. -1 0)))
(expr-test "" "+nan.0"   (eenum (/.  0 0)))
(expr-test "" "0.0"      (eenum (/.  0 +inf.0)))
(expr-test "" "-0.0"     (eenum (/.  0 -inf.0)))
(expr-test "" "31.41592653589793" (eenum (* pi 10)))

(test-section "long pattern (not print)")
(set! old-out-port (current-output-port (open-output-file (null-device))))
(expr-test ""     (string-append (make-num-str 10001) "." (make-num-str  9999 2))
           (eenum (string-append (make-num-str 10000) "." (make-num-str 10000) "e1")))
(expr-test ""     (string-append (make-num-str  9999) "." (make-num-str 10001 0))
           (eenum (string-append (make-num-str 10000) "." (make-num-str 10000) "e-1")))
(expr-test ""                    (x->string (%expt 10 10001))
           (eenum (string-append (x->string (%expt 10 10000)) "e1")))
(expr-test ""     (string-append (x->string (%expt 10  9999)) ".0")
           (eenum (string-append (x->string (%expt 10 10000)) "e-1")))
(current-output-port old-out-port)

(test-section "illegal number")
(expr-test "" ""         (eenum ""))
(expr-test "" ""         (eenum " "))
(expr-test "" "abc"      (eenum "abc"))
(expr-test "" "+123a"    (eenum "+123a"))
(expr-test "" "1 2 3"    (eenum "  1 2 3  "))

(test-end)

