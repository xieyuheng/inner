# Small object style

2020-11-23

- use record type and higher order function -- instead of class

- dependency injection, inject the role player:

``` typescript
<Class>.<fn>(obj: <Class>..., player: {
  role1: (...) => ...,
  role2: (...) => ...,
  ...
})
```
