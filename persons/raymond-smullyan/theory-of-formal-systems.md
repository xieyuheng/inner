---
title: Theory of formal systems
author: Raymond Smullyan
year: 1961
---

# CHAPTER I: FORMAL MATHEMATICAL SYSTEMS

## A. Elementary Formal Systems

```cicada
@datatype S(String) {
  a: S("a")
  b: S("b")
  ab: S("ab")
  ba: S("ba")
  xa(x: String, S(String.concat(x, "a"))): S(String.concat(x, "ab"))
  xb(x: String, S(String.concat(x, "b"))): S(String.concat(x, "ba"))
}
```
