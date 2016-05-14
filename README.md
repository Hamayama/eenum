# eenum

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
  `eenum num [width] [digits] [plus-sign]`
  - 第1引数の num には、数値または数値の文字列を指定します。

  - 第2引数の width には、全体の文字数を指定します。  
    結果がこの文字数未満であれば、半角スペースを挿入して右寄せにして出力します。  
    結果がこの文字数より多い場合には、そのまま出力します。  
    第2引数に #f を指定すると、結果をそのまま出力します。  
    第2引数は省略可能です。省略時は #f を指定したことになります。

  - 第3引数の digits には、小数点以下の桁数を指定します。  
    結果の小数部がこの桁数より多い場合には、切り捨てます。  
    結果の小数部がこの桁数より少ない場合には、0を追加します。  
    第3引数に #f を指定すると、結果をそのまま出力します。  
    第3引数は省略可能です。省略時は #f を指定したことになります。

  - 第4引数の plus-sign には、正符号(+)を出力するかどうかを指定します。  
    `#t` を指定すると、正符号を出力します。  
    `#f` を指定すると、正符号を出力しません。  
    第4引数は省略可能です。省略時は #f を指定したことになります。


## 注意事項
1. 複素数には未対応です。

2. 小数点以下の桁の丸めには未対応です(切り捨てのみ)。

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


(2016-5-15)
