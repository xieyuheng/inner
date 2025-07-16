---
title: on understanding types data abstraction and polymorphism
authors: [luca cardelli, peter wegner]
year: 1985
---

# My Motive

[2025-07-16] 想要同时支持 polymorphism 和 subtyping，
可能就不可避免地要处理 type variable 被 subtype 限制的情况，
比如 typescript 中带有 `extends` 限制的类型参数：

```typescript
interface Lengthwise {
  length: number;
}

function loggingIdentity<Type extends Lengthwise>(arg: Type): Type {
  console.log(arg.length)
  return arg
}
```

就是这篇论文中的 bounded quantification。
