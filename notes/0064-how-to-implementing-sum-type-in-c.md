---
title: how to implementing sum type in C?
date: 2025-12-18
---

在 c 中实现 sum type 的方式：

- tagged union
- tagged struct
- tagged struct + header
- tagged struct + header + vtable

# tagged union

属性：

- 类型相对安全。
- 用户不可扩展。

```c
typedef struct exp_t exp_t;

typedef enum {
    VAR_EXP,
    APPLY_EXP,
    LAMBDA_EXP,
} exp_kind_t;

struct exp_t {
    exp_kind_t kind;
    union {
        struct { char *name; } var_exp;
        struct { exp_t *target; exp_t *arg; } apply_exp;
        struct { char *parameter; exp_t *body; } lambda_exp;
    };
};
```

# tagged struct

属性：

- 类型不安全，需要 cast。
- 用户可扩展。

```c
typedef struct exp_t exp_t;

typedef enum {
    VAR_EXP,
    APPLY_EXP,
    LAMBDA_EXP,
} exp_kind_t;

struct exp_t { exp_kind_t kind; };
struct var_exp_t { exp_kind_t kind; char *name; };
struct apply_exp_t { exp_kind_t kind; exp_t *target; exp_t *arg; };
struct lambda_exp_t { exp_kind_t kind; char *parameter; exp_t *body; };
```

# tagged struct + header

属性：

- header 可以带有为所有 sum type 中所有 variant 所共用的属性。

也可以是 tagged union + header。
只需要把 kind 换成 header。

```c
typedef struct exp_t exp_t;

typedef enum {
    VAR_EXP,
    APPLY_EXP,
    LAMBDA_EXP,
} exp_kind_t;

struct exp_header_t {
    exp_kind_t kind;
    struct exp_meta_t meta;
};

struct exp_t { struct exp_header_t header; };
struct var_exp_t { struct exp_header_t header; char *name; };
struct apply_exp_t { struct exp_header_t header; exp_t *target; exp_t *arg; };
struct lambda_exp_t { struct exp_header_t header; char *parameter; exp_t *body; };
```

# tagged struct + header + vtable

属性：

- 所有就 class 而言不变的数据（函数指针），
  都放在 vtable 中，可以节约内存。

也许不应该用 vtable 这个不清晰的名字，
因为其中实际可以保存的数据是，
与 instance variable 相对的 class variable。

```c
typedef struct exp_t exp_t;
typedef struct exp_vtable_t exp_vtable_t;

typedef enum {
    VAR_EXP,
    APPLY_EXP,
    LAMBDA_EXP,
} exp_kind_t;

struct exp_vtable_t {
    void (*print)(const exp_t *);
    void (*free)(exp_t *);
};

struct exp_header_t {
    exp_kind_t kind;
    struct exp_meta_t meta;
    struct exp_vtable_t vtable;
};

struct exp_t { struct exp_header_t header; };
struct var_exp_t { struct exp_header_t header; char *name; };
struct apply_exp_t { struct exp_header_t header; exp_t *target; exp_t *arg; };
struct lambda_exp_t { struct exp_header_t header; char *parameter; exp_t *body; };
```
