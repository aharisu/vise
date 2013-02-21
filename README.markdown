# vise
LISPの様な構文を持ちVimScriptへ変換され動作するプログラミング言語。

### 依存するVimプラグイン
REPLに必須:  
[vimproc](https://github.com/Shougo/vimproc)  
あったらいいもの:  
[vim-quickrun](https://github.com/thinca/vim-quickrun)


### 依存する外部プログラム
[Gauche](http://practical-scheme.net/gauche/index-j.html) 0.9.3以上

## インストール
githubのリポジトリから全てのファイルをダウンロードして、Vimでロード可能な場所に配置してください。  
Vundleがインストールされているのであれば

    Bundle 'aharisu/vise'
と.vimrcに書いて、Vim上で

    :BundleInstall
を実行すると必要なファイルをすべてインストールすることができます。

このリポジトリは、Gaucheで書かれたviseコンパイラとVimプラグインであるviseREPLの両方を含んでいます。

- viseコンパイラ  
  viseコンパイラを使用する場合は、Gaucheが必要になるので0.9.3以上のバージョンをインストールしてください。  

- viseREPL  
  viseREPLを使用する場合は、vimprocが必要になるのでこちらも別途インストールしてください。  
  viseREPLはviseスクリプトとして書かれているのでVimScriptに変換するため一度makeを実行する必要があります。  
  Makefileはリポジトリのルートにあるので、ルートディレクトリで一度makeコマンドを実行してください。


## スクリプト例
viseでは末尾再帰最適化を行います。

    (defun fact (n)
      (let loop ((acc 1)
                 (n n))
        (if (== n 0)
          acc
          (loop (* acc n) (- n 1)))))
    (echo (fact 10))
    (echo (fact 100))

もちろんlambdaも使用できます。

    (defun Window-resize-mode (msg)
      (echo "window resize mode")
      (let loop ((ch (getchar)))
        (let1 wincmd (lambda (cmd)
                       (execute (string-append "2wincmd " cmd))
                       (execute "redraw")
                       (loop (getchar)))
          (cond
            ((== ch 104) ; 104 == 'h'
             (wincmd "<"))
            ((== ch 106) ; 106 == 'j'
             (wincmd "-"))
            ((== ch 107) ; 107 == 'k'
             (wincmd "+"))
            ((== ch 108) ; 108 == 'l'
             (wincmd ">"))))))

制限はありますがマクロも使用することができます。

    (defmacro inc!
      (match
        [(_ sym) `(+= ,sym 1)]
        [(_ sym delta) `(+= ,sym ,delta)]))
    
    (defmacro s+ str-list
      `(string-append ,@str-list))
    
    ;viseの制限により部分適用を行うときは残りの引数の数を指定しなければなりません。
    (defmacro pa$n (n proc . args)
      (let1 arg (map (lambda (n) (gensym "pa_")) (iota n))
        `(lambda ,arg (,proc ,@args ,@arg))))
    
    (defmacro pa$0 (proc . args)
      `(pa$n 0 ,proc ,@args))
    などなど

## コンパイル方法
viseファイルを開き以下を実行して下さい。

    :Genvise

Vimプラグインのvim-quickrunがインストールされている場合は設定を加えることで、
より簡単にコンパイル結果を確かめることができます。

    let g:quickrun_config = {
    \  'vise' : {
    \    'command': 'gosh',
    \    'exec': '%c ' . shellescape(globpath(&rtp, 'autoload/genvise.scm')) . ' %s',
    \    'outputter/buffer/filetype': 'vim',
    \  },
    \}

## ドキュメント
viseの構文などより詳細な情報はdoc/vise.jaxを参照してください。

