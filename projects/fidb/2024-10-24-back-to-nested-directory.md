---
title: back to nested directory
author: 谢宇恒
date: 2024-10-24
---

回到之前的设计。

- users：
  - users/[user-id]/index.json
  - users/[user-id]/mimors/[mimor-id]/index.json
- mimors：
  - mimors/[mimor-id]/index.json
  - mimors/[mimor-id]/users/[user-id]/index.json

schema 可以在创建 FiDb 的时候给出：

```typescript
const db = new FiDb({
  directory,
  schemas: {
    "users/*": UserSchema,
    "users/*/mimors/*": UserMimorSchema,
    "mimors/*": MimorSchema,
    "mimors/*/users/*": MimorUserSchema,
  }
})
```

也可以用一个 schemas.ts 或者 schemas.js 模块，
来保存上面的 `schemas`。

这样就不能用 `@id` 而是要用回 `@path`。
