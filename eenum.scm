;; -*- coding: utf-8 -*-
;;
;; eenum.scm
;; 2016-5-15 v1.00
;;
;; ＜内容＞
;;   Gauche で、数値の指数表記を展開した文字列を取得するためのモジュールです。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/eenum
;;
(define-module eenum
  (use srfi-13) ; string-for-each,string-trim-both用
  (export
    eenum
    ))
(select-module eenum)


;; 数値文字列を、符号部、整数部、小数部、指数部の文字列に分解する(内部処理用)
(define (%split-num-str num-st)
  (let ((num-len    (string-length num-st)) ; 数値文字列の長さ
        (int-index  #f) ; 整数部の開始位置
        (frac-index #f) ; 小数部の開始位置
        (exp-index  #f) ; 指数部の開始位置
        (sign-st    #f) ; 符号部の文字列
        (int-st     #f) ; 整数部の文字列
        (frac-st    #f) ; 小数部の文字列
        (exp-st     #f) ; 指数部の文字列
        (err-flag   #f) ; エラーフラグ
        (i          -1) ; 位置
        (mode       0)) ; 解析モード
    ;; 数値文字列の解析
    (string-for-each
     (lambda (c)
       (unless err-flag
         (inc! i)
         (let loop ()
           ;; 解析モードによって場合分け
           (case mode
             ;; 符号のチェック
             ((0)
              (case c
                ((#\+ #\-) (set! int-index 1) (inc! mode))
                (else  (inc! mode) (loop))))
             ;; 先頭のゼロのスキップ
             ((1)
              (case c
                ((#\0))
                (else  (inc! mode) (loop))))
             ;; 整数部のチェック
             ((2)
              (case c
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                ((#\.) (set! frac-index i) (inc! mode))
                ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
                 (set! exp-index i) (set! mode 10))
                (else  (set! err-flag #t))))
             ;; 小数部のチェック
             ((3)
              (case c
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
                 (set! exp-index i) (set! mode 10))
                (else  (set! err-flag #t))))
             ;; 指数部の符号のチェック
             ((10)
              (case c
                ((#\+ #\-) (inc! mode))
                (else  (inc! mode) (loop))))
             ;; 指数部の数値のチェック
             ((11)
              (case c
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                (else  (set! err-flag #t))))
             ))
         ))
     num-st)
    ;; 符号部、整数部、小数部、指数部の文字列を取得
    (unless err-flag
      (set! sign-st (if int-index
                      (substring num-st 0 int-index)
                      ""))
      (set! int-st  (substring num-st (or int-index 0) (or frac-index exp-index num-len)))
      (set! frac-st (if frac-index
                      (substring num-st (+ frac-index 1) (or exp-index num-len))
                      ""))
      (set! exp-st  (if exp-index
                      (substring num-st (+ exp-index 1) num-len)
                      ""))
      )
    ;; 戻り値を多値で返す
    (values sign-st int-st frac-st exp-st)))


;; 数値の指数表記を展開した文字列を取得する
;;   num        数値または数値文字列
;;              (複素数には未対応)
;;   width      全体の文字数 (省略可)
;;              (結果がこの文字数未満であれば、半角スペースを挿入して右寄せにして出力する。
;;               結果がこの文字数より多い場合には、そのまま出力する)
;;   digits     小数点以下の桁数 (省略可)
;;              (結果の小数部がこの桁数より多い場合には、切り捨てる。
;;               結果の小数部がこの桁数より少ない場合には、0を追加する)
;;   plus-sign  正符号(+)を出力するかどうか (省略可)
(define (eenum num :optional (width #f) (digits #f) (plus-sign #f))
  (rlet1 num-st (if (string? num)
                  (string-trim-both num)
                  (x->string num))
    ;; 数値文字列の分解
    (receive (sign-st int-st frac-st exp-st)
        (%split-num-str num-st)
      ;; 分解できたとき
      (when sign-st
        ;; 正符号の処理
        (if plus-sign
          (if (equal? sign-st "")  (set! sign-st "+"))
          (if (equal? sign-st "+") (set! sign-st "")))
        ;; 指数の分だけ整数部と小数部をずらす
        (let1 exp-num (x->integer exp-st)
          (cond
           ((> exp-num 0)
            (let1 frac-len (string-length frac-st)
              (cond
               ((< exp-num frac-len)
                (set! int-st  (string-append int-st (substring frac-st 0 exp-num)))
                (set! frac-st (substring frac-st exp-num frac-len)))
               ((> exp-num frac-len)
                (set! int-st  (string-append int-st frac-st (make-string (- exp-num frac-len) #\0)))
                (set! frac-st ""))
               (else
                (set! int-st  (string-append int-st frac-st))
                (set! frac-st ""))
               )))
           ((< exp-num 0)
            (let1 int-len  (string-length int-st)
              (cond
               ((< (- exp-num) int-len)
                (set! frac-st (string-append (substring int-st (- int-len (- exp-num)) int-len) frac-st))
                (set! int-st  (substring int-st 0 (- int-len (- exp-num)))))
               ((> (- exp-num) int-len)
                (set! frac-st (string-append (make-string (- (- exp-num) int-len) #\0) int-st frac-st))
                (set! int-st  "0"))
               (else
                (set! frac-st (string-append int-st frac-st))
                (set! int-st  "0"))
               )))
           ))
        ;; 小数点以下の桁数指定ありのとき
        (when digits
          (set! digits (x->integer digits))
          (cond
           ((> digits 0)
            (let1 frac-len (string-length frac-st)
              (cond
               ((> frac-len digits)
                (set! frac-st (substring frac-st 0 digits)))
               ((< frac-len digits)
                (set! frac-st (string-append frac-st (make-string (- digits frac-len) #\0))))
               )))
           (else
            (set! frac-st ""))))
        ;; 符号部、整数部、小数部の文字列を結合
        (if (equal? frac-st "")
          (set! num-st (string-append sign-st int-st))
          (set! num-st (string-append sign-st int-st "." frac-st)))
        )
      )
    ;; 全体の文字数指定ありのとき
    (when width
      (set! width (x->integer width))
      (let1 num-len (string-length num-st)
        (if (< num-len width)
          (set! num-st (string-append (make-string (- width num-len) #\space) num-st)))))
    ))


