# eenum

![image](image.png)

## 概要
- Gauche で、数値の指数表記を展開した文字列を取得するためのモジュールです。

- 例えば、1e10 → "10000000000" のように展開できます。


## インストール方法
- eenum.scm を Gauche でロード可能なフォルダにコピーします。  
  (例えば (gauche-site-library-directory) で表示されるフォルダ等)


## 使い方
- 以下を実行します。
  ```
    (use eenum)
  ```
  以後、(eenum 1e10) 等で数値の指数表記を展開できます。

- eenum 手続きの引数  
  eenum 手続きには以下の引数を指定できます。  
  `eenum num [width] [digits] [round-mode] [pad-char] [plus-sign] [sign-align-left]`
  - 第1引数の num には、数値または数値の文字列を指定します。

  - 第2引数の width には、全体の文字数を指定します。  
    結果がこの文字数未満であれば、後述の pad-char を挿入して右寄せにして出力します。  
    結果がこの文字数以上の場合には、そのまま出力します。  
    第2引数に #f を指定した場合には、文字数によらず、結果をそのまま出力します。  
    第2引数は省略可能です。省略時は #f を指定したことになります。

  - 第3引数の digits には、小数点以下の桁数を指定します。  
    結果の小数部がこの桁数より多い場合には、丸め処理を行います。  
    結果の小数部がこの桁数より少ない場合には、0を追加します。  
    もし、第3引数に負の数を指定した場合には、整数部の丸め処理を行います。  
    また、第3引数に #f を指定した場合には、結果をそのまま出力します(丸め処理を行いません)。  
    第3引数は省略可能です。省略時は #f を指定したことになります。

  - 第4引数の round-mode には、丸めモードを指定します。  
    以下のモードを指定できます。
    ```
    'truncate  ゼロへの丸め       (ゼロ方向への切り捨て)
    'floor     負の無限大への丸め (越えない最大の整数への丸め)
    'ceiling   正の無限大への丸め (下回らない最小の整数への丸め)
    'round     最近接偶数への丸め (最も近い整数へ丸める。ちょうど0.5のときは偶数に丸める)
    'round2    四捨五入
    ```
    第4引数に #f を指定した場合には、'truncate を指定したことになります。  
    第4引数は省略可能です。省略時は #f (すなわち 'truncate) を指定したことになります。

  - 第5引数の pad-char には、右寄せ時に挿入する文字を指定します。  
    第5引数に #f を指定した場合には、#\space を指定したことになります。  
    第5引数は省略可能です。省略時は #f (すなわち #\space) を指定したことになります。

  - 第6引数の plus-sign には、正符号(+)を出力するかどうかを指定します。  
    `#t` を指定すると、正符号を出力します。  
    `#f` を指定すると、正符号を出力しません。  
    第6引数は省略可能です。省略時は #f を指定したことになります。

  - 第7引数の sign-align-left には、符号を左寄せで出力するかどうかを指定します。  
    `#t` を指定すると、符号を左寄せで出力します。  
      (すなわち、符号を pad-char よりも左に出力します)  
    `#f` を指定すると、符号を左寄せにはしません。  
      (すなわち、符号を pad-char よりも右に出力します)  
    第7引数は省略可能です。省略時は #f を指定したことになります。


## 注意事項
1. 文字列操作で指数表記の展開を行うため、その分の時間がかかります。

2. 複素数には未対応です。

3. 数値の接頭辞(#b #e #i #o #x)で開始する文字列には未対応です。

4. 文字列の最大長の制限が存在します。  
   例えば、Windows 環境では、(eenum "1e536870912") はエラーになります。


## 環境等
- OS
  - Windows 8.1 (64bit)
- 言語
  - Gauche v0.9.4
  - Gauche v0.9.5_pre1

## 履歴
- 2016-5-15 v1.00 (初版)
- 2016-5-15 v1.01 先頭のゼロのスキップ処理修正  
  pad-char 引数追加
- 2016-5-15 v1.02 数値が不完全なときのエラーチェック追加  
  指数の分だけシフトした後の先頭のゼロを削除
- 2016-5-16 v1.03 数値が不完全なときのエラーチェック追加  
  小数点以下の桁数指定時の処理見直し
- 2016-5-16 v1.04 丸めモードの引数追加
- 2016-5-16 v1.05 コメント修正のみ
- 2016-5-28 v1.06 sign-align-left 引数追加  
  引数の順番見直し(過去との互換性なし)
- 2016-6-6  v1.07 内部処理の分割見直し等


(2016-6-6)
