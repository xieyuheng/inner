---
title: Testing: Getting Started
---

# Introduction

Config PHPUnit by `phpunit.xml`.

Two kinds of tests:

- `tests/Unit`

  Does **NOT** depends on Laravel application instance
  (No database, no HTTP).

  Focus on a very small,
  isolated portion of your code
  (probably a single method).

- `tests/Feature`

  Test HTTP endpoints.

  Most of the tests should be feature test.

# Environment

The environment used during testing
is different from normal environment.
