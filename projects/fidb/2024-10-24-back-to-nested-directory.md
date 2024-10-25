---
title: back to nested directory
author: 谢宇恒
date: 2024-10-24
---

之前错误：

- 直接用 directory 的包含关系来管理权限。
  正确的方式是在 sub-directory 中保存连接，
  而不是所连接到的数据本身。

  - 注意，错误不在于用了 nested directory。

- 想要直接用配置文件形成 HTTP API，
  其实与 API 相关的业务逻辑是很复杂的，
  没法完全用配置文件描述，应该用代码描述。

  - 在把业务逻辑翻译成代码时，
    如果有必要，可以向声明式的方向优化，
    不能强行要求用一套配置文件来完成所有业务。

回到之前的设计：

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
