# OCaml MM template

1ファイル提出形式のコンテストで、複数ファイルに跨るコードを書くためのパッケージです。

# 使用方法
最初に以下を実行する。

```shell
$ bash initialize.sh
```

## ビルド
```shell
src/ $ omake
```

ファイルを足した場合は、[OMakefile](./src/OMakefile) の `FILES[]` にファイル名を追加。

## 提出ファイルの生成
[src/make-submit-file.py](src/make-submit-file.py) の `code_files` に依存順にファイル名をファイル修飾子無しでセットし、

```shell
src/ $ make submit
```
を実行すると、`submit.ml` が生成される。
