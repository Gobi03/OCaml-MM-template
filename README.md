# OCaml MM template

1ファイル提出形式のコンテストで、複数ファイルに跨るコードを書くためのパッケージです。

# 使用方法
## ビルド
```shell
$ omake
```

ファイルを足した場合は、[OMakefile](./src/OMakefile) の `FILES[]` にファイル名を追加。

## 提出ファイルの生成
```shell
$ omake submit
```
を実行すると、`submit.ml` が生成されて、ついでにクリップボードにその中身が貼っつけられる。
