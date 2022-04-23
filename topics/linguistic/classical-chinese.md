---
title: Classical Chinese
---

# Type

Noun -- name, person, place, thing

- 日 本 畢 楚

Verb -- action, state

- 變 產 望

The type of words is only clear in context.

# Combine

We have primitive Noun Verb data.

We can also combine them in the following ways.

| 例子     | 规则                |
|----------|---------------------|
| 清廉     | V V -> V            |
| 來客     | V N -> V            |
| 父母     | N N -> N            |
| 心酸     | N V -> V            |
| 心口如一 | N N V N -> N V -> V |

```
grammar {
  N -> N N;
  V -> V V | V N | N V;
}
```

# Weighting

| 例子 | 规则       |
|------|------------|
| 清談 | (V) V -> V |
| 來客 | (V) N -> N |
| 唐音 | (N) N -> N |
| 自知 | (N) V -> V |

```
grammar {
  N -> N N | (N) N | (V) N;
  V -> V V | V N | N V | (V) V | (N) V|;
}
```

# Disambiguation

用虛詞来消除歧义。

| 例子   | 类型  |
|--------|-------|
| 人心   | N N   |
| 人之心 | (N) N |

| 例子   | 类型  |
|--------|-------|
| 時祭   | N N   |
| 以時祭 | (N) V |

| 例子     | 类型    | 类型  |
|----------|---------|-------|
| 日出於東 | N V V N | N V   |
| 不日成之 | V V V N | (V) V |

# 重复以成副词

现代汉语形成副词的方式是通过重复，
比如「一点一点地」「小部分小部分地」。

# References

- [The Programming Language Called Classical Chinese](https://www.youtube.com/watch?v=vBhg2p8aAQ0) - David Branner
  - 2015
  - Strange Loop Conference
