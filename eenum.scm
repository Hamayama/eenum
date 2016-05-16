;; -*- coding: utf-8 -*-
;;
;; eenum.scm
;; 2016-5-16 v1.04
;;
;; ＜内容＞
;;   Gauche で、数値の指数表記を展開した文字列を取得するためのモジュールです。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/eenum
;;
(define-module eenum
  (use srfi-13) ; string-for-each,string-trim-both,string-skip用
  (export
    eenum
    ))
(select-module eenum)


;; 数値の指数表記を展開した文字列を取得する
;;   num         数値または数値文字列
;;               (複素数には未対応)
;;   width       全体の文字数 (省略可)
;;               (結果がこの文字数未満であれば、pad-char を挿入して右寄せにして出力する。
;;                結果がこの文字数より多い場合には、そのまま出力する)
;;   digits      小数点以下の桁数 (省略可)
;;               (結果の小数部がこの桁数より多い場合には、丸め処理を行う。
;;                結果の小数部がこの桁数より少ない場合には、0を追加する。
;;                もし、負の数を指定した場合には、整数部の丸め処理を行う)
;;   plus-sign   正符号(+)を出力するかどうか (省略可)
;;   pad-char    右寄せ時に挿入する文字 (省略可)
;;   round-mode  丸めモード (省略可)
;;               'truncate 'floor 'ceiling 'round 'round2 のいずれかを指定する
;;               ('round は最近接偶数への丸め。'round2 は四捨五入)
(define (eenum num :optional (width #f) (digits #f) (plus-sign #f) (pad-char #f) (round-mode #f))
  (rlet1 num-st (if (string? num)
                  (string-trim-both num)
                  (x->string num))
    ;; 数値文字列の分解
    (receive (sign-st int-st frac-st exp-st)
        (%split-num-str num-st)
      ;; 分解できたとき
      (when sign-st
        ;; 指数の分だけ整数部と小数部をシフトする
        (let1 exp-num (x->integer exp-st)
          (cond
           ((> exp-num 0)
            ;; 左にシフト
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
            ;; 右にシフト
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
          ;; 数値文字列の丸め処理
          (set!-values (int-st frac-st)
                       (%round-num-str sign-st int-st frac-st digits (or round-mode 'truncate))))
        ;; 整数部の先頭のゼロを削除
        (let1 int-len  (string-length int-st)
          (if (> int-len 0)
            (if-let1 non-zero-index (string-skip int-st #\0)
              (set! int-st  (substring int-st non-zero-index int-len))
              (set! int-st  "0"))))
        ;; 正符号の処理
        (if plus-sign
          (if (equal? sign-st "")  (set! sign-st "+"))
          (if (equal? sign-st "+") (set! sign-st "")))
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
          (set! num-st (string-append (make-string (- width num-len) (or pad-char #\space)) num-st)))))
    ))


;; 数値文字列を、符号部、整数部、小数部、指数部の文字列に分解する(内部処理用)
(define (%split-num-str num-st)
  (let ((num-len    (string-length num-st)) ; 数値文字列の長さ
        (sign-flag  #f) ; 符号の有無
        (zero-flag  #f) ; 先頭のゼロの有無
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
                ((#\+ #\-) (set! sign-flag #t) (inc! mode))
                (else  (inc! mode) (loop))))
             ;; 先頭のゼロのスキップ
             ((1)
              (case c
                ((#\0) (set! zero-flag #t))
                (else  (set! int-index i) (inc! mode) (loop))))
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
      (set! sign-st (if sign-flag
                      (substring num-st 0 1)
                      ""))
      (set! int-st  (if int-index
                      (substring num-st int-index (or frac-index exp-index num-len))
                      ""))
      (if (and zero-flag (equal? int-st "")) (set! int-st "0"))
      (set! frac-st (if frac-index
                      (substring num-st (+ frac-index 1) (or exp-index num-len))
                      ""))
      (set! exp-st  (if exp-index
                      (substring num-st (+ exp-index 1) num-len)
                      ""))
      ;; エラーチェック
      ;;   ・整数部と小数部が両方とも空のときはエラー
      ;;   ・指数マーカーがあって指数部が未完成のときはエラー
      (if (or (and (equal? int-st "") (equal? frac-st ""))
              (and exp-index (or (equal? exp-st "")
                                 (equal? exp-st "+")
                                 (equal? exp-st "-"))))
        (set! err-flag #t))
      )
    ;; 戻り値を多値で返す
    (if err-flag
      (values #f #f #f #f)
      (values sign-st int-st frac-st exp-st))
    ))


;; 数値文字列の丸め処理(内部処理用)
(define (%round-num-str sign-st int-st frac-st digits round-mode)
  ;; 丸め処理
  (case round-mode
    ;; ゼロへの丸め(truncate)のとき(ここでの処理は不要)
    ((truncate))
    ;; その他の丸めのとき(処理が必要)
    ((floor ceiling round round2)
     ;; 余分なゼロを追加(処理を簡単にするため)
     (cond
      ((> digits 0)
       (let1 frac-len (string-length frac-st)
         (if (< frac-len digits)
           (set! frac-st (string-append frac-st (make-string (- digits frac-len) #\0))))))
      ((< digits 0)
       (let1 int-len  (string-length int-st)
         (if (< int-len (+ (- digits) 1))
           (set! int-st (string-append (make-string (- (+ (- digits) 1) int-len) #\0) int-st)))))
      (else
       (if (equal? int-st "") (set! int-st "0")))
      )
     ;; 丸めのための加算値の取得と反映
     (let ((temp-num-st1 (string-append int-st frac-st))
           (int-len      (string-length int-st))
           (frac-len     (string-length frac-st))
           (add-value    0)) ; 加算値
       ;; 加算値の取得
       (case round-mode
         ;; 負の無限大への丸め(floor)のとき
         ((floor)
          (if (and (equal? sign-st "-")
                   (string-skip temp-num-st1 #\0 (+ int-len digits)))
            (set! add-value 1)))
         ;; 正の無限大への丸め(ceiling)のとき
         ((ceiling)
          (if (and (not (equal? sign-st "-"))
                   (string-skip temp-num-st1 #\0 (+ int-len digits)))
            (set! add-value 1)))
         ;; 最近接偶数への丸め(round)のとき
         ((round)
          (case (string-ref temp-num-st1 (+ int-len digits) #\0)
            ((#\6 #\7 #\8 #\9)
             (set! add-value 1))
            ((#\5)
             (if (and (< digits frac-len)
                      (string-skip temp-num-st1 #\0 (+ (+ int-len digits) 1)))
               (set! add-value 1)
               (case (string-ref temp-num-st1 (- (+ int-len digits) 1) #\0)
                 ((#\1 #\3 #\5 #\7 #\9)
                  (set! add-value 1)))))
            ))
         ;; 四捨五入(round2)のとき
         ((round2)
          (case (string-ref temp-num-st1 (+ int-len digits) #\0)
            ((#\5 #\6 #\7 #\8 #\9)
             (set! add-value 1))
            ))
         )
       ;; 加算値の反映
       ;;   整数に変換して加算値を加算し、再度文字列に戻す
       (let* ((temp-num   (+ (x->integer (substring temp-num-st1 0 (+ int-len digits)))
                             add-value))
              (temp-num-st2  (x->string temp-num))
              (temp-num-len2 (string-length temp-num-st2)))
         ;; 整数部と小数部の文字列を取得
         (cond
          ((> digits 0)
           (cond
            ((< temp-num-len2 digits)
             (set! int-st  "0")
             (set! frac-st (string-append (make-string (- digits temp-num-len2) #\0) temp-num-st2)))
            ((> temp-num-len2 digits)
             (set! int-st  (substring temp-num-st2 0 (- temp-num-len2 digits)))
             (set! frac-st (substring temp-num-st2 (- temp-num-len2 digits) temp-num-len2)))
            (else
             (set! int-st  "0")
             (set! frac-st temp-num-st2))
            ))
          ((< digits 0)
           (set! int-st  (string-append temp-num-st2 (make-string (- digits) #\0)))
           (set! frac-st ""))
          (else
           (set! int-st  temp-num-st2)
           (set! frac-st ""))
          ))))
    )
  ;; 桁の切り捨て処理
  (cond
   ((> digits 0)
    (let1 frac-len (string-length frac-st)
      (cond
       ((> frac-len digits)
        (set! frac-st (substring frac-st 0 digits)))
       ((< frac-len digits)
        (set! frac-st (string-append frac-st (make-string (- digits frac-len) #\0))))
       )))
   ((< digits 0)
    (let1 int-len  (string-length int-st)
      (if (< (- digits) int-len)
        (set! int-st (string-append (substring int-st 0 (- int-len (- digits)))
                                    (make-string (- digits) #\0)))
        (set! int-st "0")))
    (set! frac-st ""))
   (else
    (set! frac-st "")))
  ;; 戻り値を多値で返す
  (values int-st frac-st))


