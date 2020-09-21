# note about "build from present" pattern

2020-09-21

A design pattern which can be used to avoid parsing.

API:
- `Target.build(present: any): Target`
- `Target.present(target: Target): any`

Where the `present` can be json, yaml, sexp, xml ...
