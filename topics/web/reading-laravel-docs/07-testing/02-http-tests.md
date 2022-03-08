---
title: HTTP Tests
---

# Introduction

Extends `TestCase` and prefix method with `test_` to define a test.

# Making Requests

Main methods:

```
TestCase.get
TestCase.post
TestCase.put
TestCase.patch
```

Which returns `Illuminate\Testing\TestResponse`
instead of `Illuminate\Http\Response`.

**One test, one request.**

# Look Back

The `TestCase` is an exmaple of well designed HTTP library.
