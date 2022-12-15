粘贴不是真的粘贴，
只是说明可以以这种角度
去看已经引入的几何体。

我们还是用边界的 Coordinate 来理解 i: I，
不是公理，可以计算，可能在于
path type -- p 可以作用到 i 上而得到边界点。
可以用「可以从 Coordinate」中取出边界点，
来实现同样的效果。

重点在于为了粘贴，如何修改 Coordinate。

利用 face lattice 可以描述 polytope 中的任何元素（sub-polytope）。
以每个 (n-1)-face 为 polytope 的 face lattice 的生成元，
lattice 的 order 代表 polytope sub-shape 之间的 include
（a <= b <=> a conj b = a，即 b 限制到 a 上不改变 a，
即 b 已经是 a 的一部分了，越是在 `<=` 的意义上小，代表约束越多），
conjunction 两个 (n-1)-face 可以得到 (n-2)-face，
等等 face 称为 irreducible element，
其他元素可以由 conjunction 的 disjunction 描述，
并且 disjunction normal form 可以提供一个判定等价的算法。

cubical type theory 的主要 idea 是，
- 在 context 中积累关于等价的 constraints。
- generalized transport (or coercion rules) for each type former.
- 利用 face lattice 可以描述 polytope 中的任何元素，从而描述 glueing。
