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
     (test* (string-append txt (if (equal? txt "") "" " ")
                           ": " (format "~s" 'expr)) ans expr))
    ((_ txt ans expr chk)
     (test* (string-append txt (if (equal? txt "") "" " ")
                           ": " (format "~s" 'expr)) ans expr chk))))

(define (make-num-str n :optional (s 1))
  (string-tabulate
   (lambda (i) (integer->char (+ #x30 (modulo (+ i s) 10))))
   n))

(test-section "number")
(expr-test "" "0"        (eenum 0))
(expr-test "" "123"      (eenum 123))
(expr-test "" "-123"     (eenum -123))
(expr-test "" "0.01"     (eenum 1/100))
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
(expr-test "" " 123"     (eenum "123"      :w  4))
(expr-test "" "0.0123"   (eenum "12.3e-3"  :w  5))
(expr-test "" "0.0123"   (eenum "12.3e-3"  :w  6))
(expr-test "" " 0.0123"  (eenum "12.3e-3"  :w  7))

(test-section "digits")
(expr-test "" "123.0"    (eenum "123"      :d  1))
(expr-test "" "123.00"   (eenum "123"      :d  2))
(expr-test "" "0.012"    (eenum "12.3e-3"  :d  3))
(expr-test "" "0.0123"   (eenum "12.3e-3"  :d  4))
(expr-test "" "0.01230"  (eenum "12.3e-3"  :d  5))
(expr-test "" "123"      (eenum "123.45"   :d  0))
(expr-test "" "120"      (eenum "123.45"   :d -1))
(expr-test "" "12300"    (eenum "12345"    :d -2))

(test-section "round-mode 'truncate")
(expr-test "" "120"      (eenum "123.456"  :d -1 :rm 'truncate))
(expr-test "" "123"      (eenum "123.456"  :d  0 :rm 'truncate))
(expr-test "" "123.4"    (eenum "123.456"  :d  1 :rm 'truncate))
(expr-test "" "123.45"   (eenum "123.456"  :d  2 :rm 'truncate))
(expr-test "" "-120"     (eenum "-123.456" :d -1 :rm 'truncate))
(expr-test "" "-123"     (eenum "-123.456" :d  0 :rm 'truncate))
(expr-test "" "-123.4"   (eenum "-123.456" :d  1 :rm 'truncate))
(expr-test "" "-123.45"  (eenum "-123.456" :d  2 :rm 'truncate))
(test-section "round-mode 'floor")
(expr-test "" "120"      (eenum "123.456"  :d -1 :rm 'floor))
(expr-test "" "123"      (eenum "123.456"  :d  0 :rm 'floor))
(expr-test "" "123.4"    (eenum "123.456"  :d  1 :rm 'floor))
(expr-test "" "123.45"   (eenum "123.456"  :d  2 :rm 'floor))
(expr-test "" "-130"     (eenum "-123.456" :d -1 :rm 'floor))
(expr-test "" "-124"     (eenum "-123.456" :d  0 :rm 'floor))
(expr-test "" "-123.5"   (eenum "-123.456" :d  1 :rm 'floor))
(expr-test "" "-123.46"  (eenum "-123.456" :d  2 :rm 'floor))
(test-section "round-mode 'ceiling")
(expr-test "" "130"      (eenum "123.456"  :d -1 :rm 'ceiling))
(expr-test "" "124"      (eenum "123.456"  :d  0 :rm 'ceiling))
(expr-test "" "123.5"    (eenum "123.456"  :d  1 :rm 'ceiling))
(expr-test "" "123.46"   (eenum "123.456"  :d  2 :rm 'ceiling))
(expr-test "" "-120"     (eenum "-123.456" :d -1 :rm 'ceiling))
(expr-test "" "-123"     (eenum "-123.456" :d  0 :rm 'ceiling))
(expr-test "" "-123.4"   (eenum "-123.456" :d  1 :rm 'ceiling))
(expr-test "" "-123.45"  (eenum "-123.456" :d  2 :rm 'ceiling))
(test-section "round-mode 'round")
(expr-test "" "120"      (eenum "123.456"  :d -1 :rm 'round))
(expr-test "" "123"      (eenum "123.456"  :d  0 :rm 'round))
(expr-test "" "123.5"    (eenum "123.456"  :d  1 :rm 'round))
(expr-test "" "123.46"   (eenum "123.456"  :d  2 :rm 'round))
(expr-test "" "-120"     (eenum "-123.456" :d -1 :rm 'round))
(expr-test "" "-123"     (eenum "-123.456" :d  0 :rm 'round))
(expr-test "" "-123.5"   (eenum "-123.456" :d  1 :rm 'round))
(expr-test "" "-123.46"  (eenum "-123.456" :d  2 :rm 'round))
(expr-test "" "123.0"    (eenum "123.05"   :d  1 :rm 'round))
(expr-test "" "123.2"    (eenum "123.15"   :d  1 :rm 'round))
(expr-test "" "124.0"    (eenum "123.95"   :d  1 :rm 'round))
(expr-test "" "-123.0"   (eenum "-123.05"  :d  1 :rm 'round))
(expr-test "" "-123.2"   (eenum "-123.15"  :d  1 :rm 'round))
(expr-test "" "-124.0"   (eenum "-123.95"  :d  1 :rm 'round))
(expr-test "" "123.0"    (eenum "123.050"  :d  1 :rm 'round))
(expr-test "" "123.2"    (eenum "123.150"  :d  1 :rm 'round))
(expr-test "" "124.0"    (eenum "123.950"  :d  1 :rm 'round))
(expr-test "" "-123.0"   (eenum "-123.050" :d  1 :rm 'round))
(expr-test "" "-123.2"   (eenum "-123.150" :d  1 :rm 'round))
(expr-test "" "-124.0"   (eenum "-123.950" :d  1 :rm 'round))
(expr-test "" "1000.00"  (eenum "999.995"  :d  2 :rm 'round))
(expr-test "" "-1000.00" (eenum "-999.995" :d  2 :rm 'round))
(test-section "round-mode 'round2")
(expr-test "" "120"      (eenum "123.456"  :d -1 :rm 'round2))
(expr-test "" "123"      (eenum "123.456"  :d  0 :rm 'round2))
(expr-test "" "123.5"    (eenum "123.456"  :d  1 :rm 'round2))
(expr-test "" "123.46"   (eenum "123.456"  :d  2 :rm 'round2))
(expr-test "" "-120"     (eenum "-123.456" :d -1 :rm 'round2))
(expr-test "" "-123"     (eenum "-123.456" :d  0 :rm 'round2))
(expr-test "" "-123.5"   (eenum "-123.456" :d  1 :rm 'round2))
(expr-test "" "-123.46"  (eenum "-123.456" :d  2 :rm 'round2))
(expr-test "" "123.1"    (eenum "123.05"   :d  1 :rm 'round2))
(expr-test "" "123.2"    (eenum "123.15"   :d  1 :rm 'round2))
(expr-test "" "124.0"    (eenum "123.95"   :d  1 :rm 'round2))
(expr-test "" "-123.1"   (eenum "-123.05"  :d  1 :rm 'round2))
(expr-test "" "-123.2"   (eenum "-123.15"  :d  1 :rm 'round2))
(expr-test "" "-124.0"   (eenum "-123.95"  :d  1 :rm 'round2))
(expr-test "" "123.1"    (eenum "123.050"  :d  1 :rm 'round2))
(expr-test "" "123.2"    (eenum "123.150"  :d  1 :rm 'round2))
(expr-test "" "124.0"    (eenum "123.950"  :d  1 :rm 'round2))
(expr-test "" "-123.1"   (eenum "-123.050" :d  1 :rm 'round2))
(expr-test "" "-123.2"   (eenum "-123.150" :d  1 :rm 'round2))
(expr-test "" "-124.0"   (eenum "-123.950" :d  1 :rm 'round2))
(expr-test "" "1000.00"  (eenum "999.995"  :d  2 :rm 'round2))
(expr-test "" "-1000.00" (eenum "-999.995" :d  2 :rm 'round2))

(test-section "pad-char")
(expr-test "" "123"      (eenum "123"      :w  2 :pc #\#))
(expr-test "" "123"      (eenum "123"      :w  3 :pc #\#))
(expr-test "" "#123"     (eenum "123"      :w  4 :pc #\#))
(expr-test "" "##123"    (eenum "123"      :w  5 :pc #\#))
(expr-test "" "#-123"    (eenum "-123"     :w  5 :pc #\#))
(expr-test "" "#+123"    (eenum "123"      :w  5 :pc #\# :ps #t))

(test-section "plus-sign")
(expr-test "" "123"      (eenum "123"      :ps #f))
(expr-test "" "123"      (eenum "+123"     :ps #f))
(expr-test "" "-123"     (eenum "-123"     :ps #f))
(expr-test "" "+123"     (eenum "123"      :ps #t))
(expr-test "" "+123"     (eenum "+123"     :ps #t))
(expr-test "" "-123"     (eenum "-123"     :ps #t))

(test-section "sign-align-left")
(expr-test "" "00123"    (eenum "123"      :w  5 :pc #\0 :sal #t))
(expr-test "" "-0123"    (eenum "-123"     :w  5 :pc #\0 :sal #t))
(expr-test "" "#-123"    (eenum "-123"     :w  5 :pc #\# :sal #f))
(expr-test "" "+0123"    (eenum "123"      :w  5 :pc #\0 :sal #t :ps #t))
(expr-test "" "#+123"    (eenum "123"      :w  5 :pc #\# :sal #f :ps #t))

(test-section "circular-digits")
(expr-test "" "0.33333"        (eenum 1/3  :cd 5))
(expr-test "" "-0.33333"       (eenum -1/3 :cd 5))
(expr-test "" "0.142857142857" (eenum 1/7  :cd 12))
(expr-test "" "0.25"           (eenum 1/4  :cd 10000))
(expr-test "" "299999999.999999999" (eenum 299999999999999999/1000000000))
(expr-test "" (test-error <error>)  (eenum 1/3 :cd 1000001))

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
(expr-test "" "e+"       (eenum "e+"))
(expr-test "" "0e-"      (eenum "0e-"))

(test-section "calculation")
(expr-test "" "+inf.0"   (eenum (/.  1 0)))
(expr-test "" "-inf.0"   (eenum (/. -1 0)))
(expr-test "" "+nan.0"   (eenum (/.  0 0)))
(expr-test "" "0.0"      (eenum (/.  0 +inf.0)))
(expr-test "" "-0.0"     (eenum (/.  0 -inf.0)))
(expr-test "" "31.41592653589793" (eenum (* pi 10)))

(test-section "long pattern (not printed)")
(with-output-to-file (null-device)
  (lambda ()
    (expr-test ""     (string-append (make-num-str 10001) "." (make-num-str  9999 2))
               (eenum (string-append (make-num-str 10000) "." (make-num-str 10000) "e1")))
    (expr-test ""     (string-append (make-num-str  9999) "." (make-num-str 10001 0))
               (eenum (string-append (make-num-str 10000) "." (make-num-str 10000) "e-1")))
    (expr-test ""                    (x->string (%expt 10 10001))
               (eenum (string-append (x->string (%expt 10 10000)) "e1")))
    (expr-test ""     (string-append (x->string (%expt 10  9999)) ".0")
               (eenum (string-append (x->string (%expt 10 10000)) "e-1")))
    ))

(test-section "illegal number")
(expr-test "" ""         (eenum ""))
(expr-test "" ""         (eenum " "))
(expr-test "" "abc"      (eenum "abc"))
(expr-test "" "+123a"    (eenum "+123a"))
(expr-test "" "1 2 3"    (eenum "  1 2 3  "))

(test-end)

