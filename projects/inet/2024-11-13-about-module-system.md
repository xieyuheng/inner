---
title: about module system
date: 2024-11-13
---

关于模块系统，目前有简单的定义：

```c
struct mod_t {
    list_t *spec_list;
    list_t *rule_list;
};
```

在 cicada-lang/lambda 中有：

```typescript
export type Mod = {
  url: URL
  loadedMods: Map<string, { mod: Mod; text: string }>
  definitions: Map<string, Definition>
  stmts: Array<Stmt>
  isFinished?: boolean
}
```

在 cicada-lang/inet-js 带有依赖注入 `loader` 的 `Mod` 中：

```typescript
export type Mod = {
  loader: Loader
  env: Env
  checking: Checking
  url: URL
  text: string
  stmts: Array<Stmt>
  definitions: Map<string, Definition>
  builtins: Map<string, Definition>
  ruleEntries: Map<string, RuleEntry>
  requiredMods: Map<string, Mod>
}
```

# multi-pass and propagator model

依赖注入 `loader` 是为了处理 `import`。

之前的经验是，不应该用依赖注入来处理 `import`，应该分多次 pass。
即，先 load 所有的 `mod` 并解析好 `stmts`，
如果需要的话，可以分多次 pass 来处理 `stmts`，
比如先用 `define` 处理 `Define` 和 `Import` 一类 stmt，
再 `execute` 处理 `Compute` 一类 stmt：

```typescript
for (const stmt of mod.stmts)
  define(mod, stmt)
for (const definition of modOwnDefinitions(mod).values())
  assertAllNamesDefined(mod, definition)
for (const definition of modOwnDefinitions(mod).values())
  occurCheck(mod, definition)
for (const stmt of mod.stmts)
  execute(mod, stmt)
```

mod 本身也可以尝试用 propagator model
中的 merge partial information 来理解：

- 先有 `url: URL`
- 再 `text: string`
- 进一步 `stmts: Array<Stmt>`
- 最终获得 `definitions: Map<string, Definition>`

# one mod one file

很重要的一点是，保持 one mod one file 的思想模型，
不要为了测试的时候需要构造不对应 file 的 mod 而让 mod 变复杂，
因为最常用的情形就是 one mod one file，
应该为最常用的情形优化。
