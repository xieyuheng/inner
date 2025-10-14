直接实现 cfg 而不是先实现一般的 digraph。
因为 cfg 中虽然可能有多个 edge，但是 edge 是没有名字的。
也许可以用 `flow` 来代表 cfg

SSA 到 x86 的 codegen 可以简单地用 rewrite rule 实现。
由于我们有固定的数据类型 sexp，
所以使用 rewrite rule 很方便。
