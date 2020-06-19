author: 謝宇恆 / XIE Yuheng

primitive:
  end

primitive:
  literal

primitive:
  false
  true
  not
  and
  or

primitive:
  add
  sub
  neg
  mul
  div
  mod

primitive:
  cr
  simple-write
  integer-write

primitive:
  bye

primitive:
  set-argument-stack-snapshot
  get-argument-stack-snapshot

primitive:
  allocate-local-memory
  allocate-conjugate-local-memory
  reset-local-memory
  reset-scope

primitive:
  antecedent-local-variable
  succedent-local-variable

primitive:
  bn

primitive:
  get-snapshot-different-number
  n-match

variable: little-test-number
  integer: 16

variable: step4
  integer: 4

variable: step-neg10
  integer: -10

variable: step10
  integer: 10

function: little-test-test
  match:
    +
    | little-test-number allocate-local-memory simple-write
      little-test-number allocate-conjugate-local-memory simple-write

function: swap
  match:
    + :a :b
    | :b :a

function: little-test
  match:
    +
    | little-test-number allocate-local-memory simple-write
      little-test-test
      little-test-number allocate-local-memory simple-write
      cr

      reset-local-memory
      little-test-number allocate-local-memory simple-write
      little-test-number allocate-local-memory simple-write
      cr

      little-test-number allocate-conjugate-local-memory simple-write
      little-test-number allocate-conjugate-local-memory simple-write
      cr

      get-argument-stack-snapshot simple-write
      little-test-number set-argument-stack-snapshot simple-write
      get-argument-stack-snapshot simple-write
      cr

      false step4 bn
      get-argument-stack-snapshot simple-write
      get-argument-stack-snapshot simple-write

      step10 neg simple-write
      step10 neg integer-write
      cr

      step-neg10 simple-write
      step-neg10 integer-write
      cr

      123 simple-write
      cr

      -123 simple-write
      -123 integer-write
      cr

      :kkk: simple-write
      cr

      :k1: antecedent-local-variable
      :k1: succedent-local-variable simple-write
      cr

      666
        1 2 3
        1 2 3
        3 n-match simple-write
      simple-write simple-write simple-write
      simple-write
      cr

      666
        1 2 3
        1 2 4
        3 n-match simple-write
      simple-write simple-write simple-write
      simple-write
      cr

      666
        1 2 3
        1 :a1: antecedent-local-variable 3
        3 n-match simple-write
        simple-write simple-write simple-write
      simple-write
      :a1: succedent-local-variable simple-write
      cr

      666
        1 2 2
        1 :a2: antecedent-local-variable :a2: antecedent-local-variable
        3 n-match simple-write
        simple-write simple-write simple-write
      simple-write
      cr

      666
        1 2 3
        1 :a3: antecedent-local-variable :a3: antecedent-local-variable
        3 n-match simple-write
        simple-write simple-write simple-write
      simple-write
      cr

      set-argument-stack-snapshot
      1 2 3
      get-snapshot-different-number
      simple-write
      simple-write
      simple-write
      simple-write
      cr

      1 2 swap simple-write simple-write
      cr

      bye

entry: little-test
