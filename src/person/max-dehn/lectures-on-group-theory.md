---
title: Lectures on group theory
---

# Lectures on group theory

------
- Auther: Max Dehn
- English translator: John Stillwell
------

# 1 generation and geometric representation of (S 3!)

- [term]
  permute -- to change the order
  permutation -- the permutation of n is n!
  symmetric group -- the group of all permutations
  permutation group -- subgroup of symmetric group

- we begin by
  generation and geometric representation of (S 3!)
  the group of permutations of three things.
  ``` js
  s1 := (1 2) = [[1 2 3] [2 1 3]]
  s2 := (1 2 3) = [[1 2 3] [2 3 1]]
  (note we have
        (= [{s1} 2 times] [identity])
        (= [{s2} 3 times] [identity])
        (= [{s1 s2} 2 times] [identity]))
  ```

- to answer whether the generators and the relations completely define the group,
  we need to consider the geometric representation of a group, or the group diagram.

- the diagram has
  one-to-one correspondence between group elements and vertices,
  and it displays all possible relations between group elements.

- x -
  通過自我映射而定義一個羣
  求這個羣的生成子和關係表示
  這裏是求解這個問題的一個例子
  Dehn 所使用的方法如下
  已經找出了想要觀察的生成子和關係
  生成它們的 cayley 圖
  然後數其中點的個數
  點的個數與想要生成的羣的元素個數相等 即可
  [但是這只對有限羣適用]

# 2 the group diagram of (S 4!)

- we begin by
  generation and geometric representation of (S 3!)
  the group of permutations of three things.
  ``` js
  s1 := (1 2) = [[1 2 3 4] [2 1 3 4]]
  s2 := (1 2 3 4) = [[1 2 3 4] [2 3 4 1]]
  (note we have
        (= [{s1} 2 times] [identity])
        (= [{s2} 4 times] [identity])
        (= [{s1 s2} 3 times] [identity]))
  ```

- again we prove this is indeed a group representation of (S 3!) by counting.

- the cayley-graph is to a certain extent a formula or computational scheme,
  and provides us with an example from the border zone between formula and figure.

# 3 group diagrams for the alternating groups (A 12) and (A 60)

- alternating groups as subgroup of symmetric groups
  with index 2

- for (A 12) we can proceed in different ways :
  ``` js
  s1 := (1 2 3)
  s2 := (2 3 4)
  (note we have
        (= [{s1} 3 times] [identity])
        (= [{s2} 3 times] [identity])
        (= [{s1 s2} 2 times] [identity]))
  ;; or
  s1 := (1 3) (2 4)
  s2 := (1 2 3)
  (note we have
        (= [{s1} 2 times] [identity])
        (= [{s2} 3 times] [identity])
        (= [{s1 s2} 3 times] [identity]))
  ;; cutting vertices of tetrahedron.
  ```

- for (A 60)
  the covering-space can be get from cutting vertices of dodecahedron.
  ``` js
  s1 := (1 2 3)
  s2 := (2 4 1 5 3)
  (note we have
        (= [{s1} 3 times] [identity])
        (= [{s2} 5 times] [identity])
        (= [{s1 s2} 2 times] [identity]))
  ```

# 4 the icosahedral and dodecahedral group

- both are (A 60)

# 5 extension of the icosahedral group by reflections

# 6 the group diagram of (S 5!)

# 7 generation of the group (S 120) bу two operations

# 8 excursus into non-euclidean geometry

# 9 the group diagram of an infinite group

# 10 investigation of a certain class of groups

# 11 group diagrams of the second kind

# 12 sufficient relations for the generators

# 13 groups with more than two generators
