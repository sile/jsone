jsone (0.2.1)
=============

Erlangで実装されたJSONのエンコード/デコードライブラリ。

特徴
----
- RFC4627準拠
- 日本語を含む文字列に対応
  - ただし文字列はUTF-8のみをサポート
- Pure Erlang
- デコード処理部を実験的にCPS的に実装
  - パース途中にサブバイナリが生成されることを極力抑制して最適化
  - NIFを使わない実装の中ではおそらく最速

ビルド方法
----------
ビルドツールには[rebar](https://github.com/basho/rebar)を使用している。

ビルド手順:
```sh
# ビルド
$ git clone git://github.com/sile/jsone.git
$ make init

# テスト & dialyzer 実行
$ make

# ロードパスに追加してErlangシェルを起動
$ make start
1> jsone:decode(<<"[1,2,3]">>).
[1,2,3]
```

API
---
[EDOCドキュメント](doc/jsone.md)

使用例
-----
```erlang
%% デコード
> jsone:decode(<<"[1,2,3]">>).
{[1,2,3],<<>>}                  % 返り値は {デコード結果, 未使用バイナリ} 形式

> json:decode(<<"{\"1\":2}">>).
{{[{<<"1">>,2}]},<<>>}   % オブジェクトは {object, [Key, Value]} 形式にデコードされる

%% エンコード
> jsone:encode([1,2,3]).
<<"[1,2,3]">>

```

参考
----
- [JSON](http://www.json.org/)
- [RFC4627](http://www.ietf.org/rfc/rfc4627.txt)
