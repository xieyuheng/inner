---
title: Why we should NOT use article when naming variables
date: 2024-06-12
---

假设我们不用 PascalCase 来命名类型，
直接用正常的 camelCase（或 snake_case）来命名类型：

- `int`
- `user`
- `item`
- `exp`
- `propagator`

那么由于在带有 dependent type 的语言中，
type 就是 value，所以变量名时，有两种方案：

「一」每次都给出有别于类型名的有意义的变量名，比如：

- `n: int`
- `guest: user`
- `target: exp`
- 等等

但是，每次都能相出有意义的名字是很难的。

「二」带上冠词：

- `anInt`
- `aUser`
- `anItem`
- `anExp`
- `aPropagator`

尽管作为 variable name 时，读起来是看似合理的，
但是用作 object 的 field name 时，根本不符合英文语法了。
因此用冠词也是不可取的。

既然以不用 PascalCase 为前提所面临的两种情况都有问题，
那么，我们一般需要用用 PascalCase 来命名类型：

- `Int`
- `User`
- `Item`
- `Exp`
- `Propagator`

而用 camelCase 来命名 variable（和某些 object 的 field）：

- `int`
- `user`
- `item`
- `exp`
- `propagator`
