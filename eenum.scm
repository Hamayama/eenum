;; -*- coding: utf-8 -*-
;;
;; eenum.scm
;; 2019-12-16 v1.36
;;
;; ＜内容＞
;;   Gauche で、数値の指数表記を展開した文字列を取得するためのモジュールです。
;;   また、桁数を指定して丸め処理を行った文字列を取得することもできます。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/eenum
;;
(define-module eenum
  (use srfi-13) ; string-for-each,string-trim-both,string-skip用
  (export
    eenum))
(select-module eenum)

;; 循環小数の最大桁数の最大値
(define *max-circular-digits* 1000000)

;; 循環小数の最大桁数のデフォルト値
(define *default-circular-digits* 100)

;; 数値の指数表記を展開した文字列を取得する
;;   num              数値または数値文字列
;;                    (複素数には未対応)
;;   :w   width       全体の文字数 (キーワード引数)
;;                    (結果がこの文字数未満であれば、pad-char を挿入して右寄せにして出力する。
;;                     結果がこの文字数以上の場合には、そのまま出力する)
;;   :d   digits      小数点以下の桁数 (キーワード引数)
;;                    (結果の小数部がこの桁数より多い場合には、丸め処理を行う。
;;                     結果の小数部がこの桁数より少ない場合には、0 を追加する。
;;                     もし、負の数を指定した場合には、整数部の丸め処理を行う)
;;   :rm  round-mode  丸めモード (キーワード引数)
;;                    'truncate 'floor 'ceiling 'round 'round2 のいずれかを指定する
;;                    ('round は最近接偶数への丸め。'round2 は四捨五入)
;;   :pc  pad-char    右寄せ時に挿入するパッド文字 (キーワード引数)
;;   :ps  plus-sign   正符号(+)を出力するかどうか (キーワード引数)
;;   :sal sign-align-left  符号を左寄せで出力するかどうか (キーワード引数)
;;   :cd  circular-digits  循環小数の最大桁数 (キーワード引数)
;;   :en  exponential-notation  指数表記で出力を行うかどうか (キーワード引数)
;;   :ed  exponential-digits    指数表記の整数部の桁数 (キーワード引数)
(define (eenum num
               :key ((:w width) #f) ((:d digits) #f)
               ((:rm round-mode)           'truncate)
               ((:pc pad-char)             #\space)
               ((:ps plus-sign)            #f)
               ((:sal sign-align-left)     #f)
               ((:cd circular-digits)      *default-circular-digits*)
               ((:en exponential-notation) #f)
               ((:ed exponential-digits)   1))
  ;; 引数のチェック
  (if width  (set! width  (x->integer width)))
  (if digits (set! digits (x->integer digits)))
  (unless (memq round-mode '(truncate floor ceiling round round2))
    (error "invalid round-mode:" round-mode))
  (unless (char? pad-char)
    (error "pad-char must be a character, but got" pad-char))
  (set! circular-digits (x->integer circular-digits))
  (if (> circular-digits *max-circular-digits*)
    (error "circular-digits too large:" circular-digits))
  (set! exponential-digits (x->integer exponential-digits))

  ;; 数値文字列への変換
  (rlet1 num-st (%convert-to-num-str num circular-digits)
    ;; 数値文字列の分解
    (receive (split-ok sign-st int-st frac-st exp-st)
        (%split-num-str num-st)
      ;; 分解できたとき
      (when split-ok
        (let ((exp-num     (x->integer exp-st))
              (change-flag #f))
          ;; 指数表記指定のチェック
          (if exponential-notation
            ;; 数値文字列の正規化処理
            (set!-values (int-st frac-st exp-num)
                         (%normalize-num-str int-st frac-st exp-num exponential-digits))
            ;; 数値文字列のシフト処理
            (set!-values (int-st frac-st exp-num)
                         (%shift-num-str int-st frac-st exp-num)))
          ;; 小数点以下の桁数指定ありのとき
          (when digits
            ;; 数値文字列の丸め処理
            (set!-values (change-flag int-st frac-st)
                         (%round-num-str sign-st int-st frac-st digits round-mode)))
          ;; 整数部の先頭のゼロを削除
          (set! int-st (%remove-leading-zero int-st))

          ;; 指数表記指定で、小数点以下の桁数指定ありで、変化していたとき
          ;; (丸めによる最上位桁の繰り上がり対策でもう1回処理する)
          (when (and exponential-notation digits change-flag)
            ;; 数値文字列の正規化処理(2回目)
            (set!-values (int-st frac-st exp-num)
                         (%normalize-num-str int-st frac-st exp-num exponential-digits))
            ;; 数値文字列の丸め処理(2回目)
            ;; (ここはゼロへの丸めとする(最上位桁の繰り上がり後は、もう繰り上がらないため))
            (set!-values (change-flag int-st frac-st)
                         (%round-num-str sign-st int-st frac-st digits 'truncate))
            ;; 整数部の先頭のゼロを削除(2回目)
            (set! int-st (%remove-leading-zero int-st)))

          ;; 正符号の処理
          (if plus-sign
            (if (equal? sign-st "")  (set! sign-st "+"))
            (if (equal? sign-st "+") (set! sign-st "")))
          ;; 符号部、整数部、小数部の文字列を結合
          (if (equal? frac-st "")
            (set! num-st (string-append sign-st int-st))
            (set! num-st (string-append sign-st int-st "." frac-st)))
          ;; 指数部の文字列を結合
          (set! exp-st (x->string exp-num))
          (when (or (eq? exponential-notation 'always)
                    (and (not (= exp-num 0))
                         (string-skip (string-append int-st frac-st) #\0)))
            (set! num-st (string-append num-st "e" exp-st)))
          ))
      ;; 全体の文字数指定ありのとき
      (when width
        ;; 数値文字列のパッド文字挿入処理
        (set! num-st (%insert-pad-char num-st width pad-char sign-align-left split-ok sign-st)))
      )))


;; 数値文字列への変換(内部処理用)
(define (%convert-to-num-str num circular-digits)
  (cond
   ;; 正確数でかつ整数以外のとき
   ((and (exact? num) (not (integer? num)))
    ;; 有理数を循環小数に展開する(ただし最大桁数までで止める)
    (let* ((minus (< num 0))              ; マイナス符号フラグ
           (num1  (if minus (- num) num)) ; 符号をプラスにする
           (n     (numerator   num1))     ; 有理数の分子
           (d     (denominator num1))     ; 有理数の分母
           (q     (quotient  n d))        ; 商
           (r     (remainder n d)))       ; 余り
      ;; 文字列の出力
      (with-output-to-string
        (lambda ()
          ;; 整数部の出力
          (if minus (display #\-))
          (display q)
          ;; 小数のとき
          (unless (= r 0)
            ;; 小数点の出力
            (display #\.)
            ;; 小数部の各桁を求めて出力
            (let loop ((i 0))
              (set! n (* r 10))
              (set! q (quotient  n d))
              (set! r (remainder n d))
              (display (integer->digit q))
              (when (and (not (= r 0)) (< (+ i 1) circular-digits))
                (loop (+ i 1)))))))))
   ;; その他のとき
   (else
    (string-trim-both (x->string num)))
   ))


;; 数値文字列の分解(内部処理用)
;;   ・符号部、整数部、小数部、指数部の文字列に分解する
;;
;; ＜正規表現バージョン＞
;; (長い文字列のときにエラー「stack overrun during matching regexp」が発生するため未使用)
;;(define (%split-num-str num-st)
;;  (let* ((m         (#/^([+\-])?(0+)?(\d+)?(?:\.(\d+)?)?(?:[eEsSfFdDlL]([+\-]?\d*))?$/ num-st))
;;         (sign-st   (or (and m (m 1)) ""))
;;         (zero-flag (or (and m (m 2)) #f))
;;         (int-st    (or (and m (m 3)) ""))
;;         (frac-st   (or (and m (m 4)) ""))
;;         (exp-st    (or (and m (m 5)) ""))
;;         (err-flag  (not (boolean m))))
;;    (unless err-flag
;;      (if (and zero-flag (equal? int-st ""))
;;        (set! int-st "0"))
;;      (if (and (equal? int-st "") (equal? frac-st ""))
;;        (set! err-flag #t))
;;      (if (and m (m 5) (or (equal? exp-st "")
;;                           (equal? exp-st "+")
;;                           (equal? exp-st "-")))
;;        (set! err-flag #t)))
;;    (if err-flag
;;      (values #f #f #f #f #f)
;;      (values #t sign-st int-st frac-st exp-st))))
;;
;; ＜1文字ずつ解析していくバージョン＞
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
      (if (and zero-flag (equal? int-st ""))
        (set! int-st "0"))
      (set! frac-st (if frac-index
                      (substring num-st (+ frac-index 1) (or exp-index num-len))
                      ""))
      (set! exp-st  (if exp-index
                      (substring num-st (+ exp-index 1) num-len)
                      ""))
      ;; エラーチェック
      ;;   ・整数部と小数部が両方とも空のときはエラー
      ;;   ・指数マーカーがあって指数部が未完成のときはエラー
      (if (and (equal? int-st "") (equal? frac-st ""))
        (set! err-flag #t))
      (if (and exp-index (or (equal? exp-st "")
                             (equal? exp-st "+")
                             (equal? exp-st "-")))
        (set! err-flag #t))
      )
    ;; 戻り値を多値で返す(先頭は成功フラグ)
    (if err-flag
      (values #f #f #f #f #f)
      (values #t sign-st int-st frac-st exp-st))))


;; 数値文字列の正規化処理(内部処理用)
(define (%normalize-num-str int-st frac-st exp-num exponential-digits)
  ;; 整数部の桁数が exponential-digits になるように指数部を調整する
  (if-let1 non-zero-index (string-skip (string-append int-st frac-st) #\0)
    (let ((exp-num1  (- (+ non-zero-index exponential-digits) (string-length int-st)))
          (exp-dummy 0))
      ;; 数値文字列のシフト処理
      (set!-values (int-st frac-st exp-dummy)
                   (%shift-num-str int-st frac-st exp-num1))
      ;; 指数部の調整
      (set! exp-num (+ exp-num (- exp-num1)))))
  ;; 戻り値を多値で返す
  (values int-st frac-st exp-num))


;; 数値文字列のシフト処理(内部処理用)
(define (%shift-num-str int-st frac-st exp-num)
  ;; 指数の分だけ整数部と小数部をシフトする
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
   )
  ;; 戻り値を多値で返す(指数部は0とする)
  (values int-st frac-st 0))


;; 数値文字列の丸め処理(内部処理用)
(define (%round-num-str sign-st int-st frac-st digits round-mode)
  (define change-flag #f)
  ;; 丸め処理
  (case round-mode
    ;; ゼロへの丸め(truncate)のとき(ここでの処理は不要)
    ((truncate))
    ;; その他の丸めのとき
    ((floor ceiling round round2)
     ;; 数値文字列の丸め処理サブ
     (receive (change-flag1 int-st1 frac-st1)
         (%round-num-str-sub sign-st int-st frac-st digits round-mode)
       (when change-flag1
         (set! change-flag change-flag1)
         (set! int-st      int-st1)
         (set! frac-st     frac-st1))))
    )
  ;; 桁の切り捨て/追加処理
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
        (set! int-st  (string-append (substring int-st 0 (- int-len (- digits)))
                                     (make-string (- digits) #\0)))
        (set! int-st  "0")))
    (set! frac-st ""))
   (else
    (set! frac-st ""))
   )
  ;; 戻り値を多値で返す(先頭は変化フラグ)
  (values change-flag int-st frac-st))


;; 数値文字列の丸め処理サブ(内部処理用)
(define (%round-num-str-sub sign-st int-st frac-st digits round-mode)
  ;; 余分なゼロを追加(後の処理を簡単にするため)
  (cond
   ((> digits 0)
    (let1 frac-len (string-length frac-st)
      (if (< frac-len digits)
        (set! frac-st (string-append frac-st (make-string (- digits frac-len) #\0))))))
   ((< digits 0)
    (let1 int-len  (string-length int-st)
      (if (< int-len  (+ (- digits) 1))
        (set! int-st  (string-append (make-string (- (+ (- digits) 1) int-len) #\0) int-st)))))
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
    ;;   ・整数に変換して加算値を加算し、再度文字列に戻す
    (unless (= add-value 0)
      (let* ((temp-num      (+ (x->integer (substring temp-num-st1 0 (+ int-len digits)))
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
         ))
      )
    ;; 戻り値を多値で返す(先頭は変化フラグ)
    (if (= add-value 0)
      (values #f #f #f)
      (values #t int-st frac-st))))


;; 整数部の先頭のゼロを削除(内部処理用)
(define (%remove-leading-zero int-st)
  (let1 int-len (string-length int-st)
    (if (> int-len 0)
      (if-let1 non-zero-index (string-skip int-st #\0)
        (substring int-st non-zero-index int-len)
        "0")
      int-st)))


;; 数値文字列のパッド文字挿入処理(内部処理用)
(define (%insert-pad-char num-st width pad-char sign-align-left split-ok sign-st)
  (let1 num-len (string-length num-st)
    (if (< num-len width)
      (if (and sign-align-left split-ok)
        (string-append sign-st (make-string (- width num-len) pad-char)
                       (substring num-st (string-length sign-st) num-len))
        (string-append (make-string (- width num-len) pad-char) num-st))
      num-st)))


