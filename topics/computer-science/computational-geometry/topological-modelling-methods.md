---
title: 拓扑建模方法
subtitle: Topological modelling methods
---

# WingedEdge

这种表示起源于描述 Polyhedra，
由于每个 Edge 只能有两个 Face，
所以只能描述 Manifold。

```typescript
type Vertex = {
  incidentEdges: Array<Edge>
}

type Edge = {
  startVertex: Vertex, endVertex: Vertex

  leftFace: Face
  counterclockwisePrevEdge: Edge
  counterclockwiseNextEdge: Edge

  rightFace: Face
  clockwisePrevEdge: Edge
  clockwiseNextEdge: Edge
}

type Face = {
  boundingEdges: Array<Edge>
}
```

To decide left, right, clockwise and counterclockwise,
we should view the oriented edge from outside of the solid.

Face 的 `boundingEdges` 中的边，按照某个选定的定向法则（比如右手螺旋法则），组成一个圈。
代表就一个 Edge 而言，拿到 Face 之后可以计算出 PrevEdge 和 NextEdge，
所以在 Edge 里保存这些信息是多余的，只是为了常数时间的读取。

## 支持 Non Manifold 的变体

```typescript
type Vertex = {
  incidentEdges: Array<Edge>
  incidentFaces: Array<Face>
}

type Edge = {
  startVertex: Vertex, endVertex: Vertex
  incidentFaces: Array<Face>
}

type Face = {
  boundingVertexes: Array<Vertex>
  boundingEdges: Array<Edge>
}
```

The `incidentFaces` are cyclically ordered according to
how they intersect a plane normal to the edge.

注意这里没有 PrevEdge 和 NextEdge 之类的冗余信息，
当然也可以加上这些信息。

# HalfEdge

也是起源于描述 Polyhedra。

- 原始论文就叫 "Finding the Intersection of Two Convex Polyhedra"

  - Muller, D. E.; Preparata, F. P. (1978)

```typescript
type HalfEdge = {
  prev: HalfEdge
  next: HalfEdge
  twin: HalfEdge
  face: Face
}

type Face = {
  halfEdges: List<HalfEdge>
}
```

每个 Face 按照约定的 clockwise 或 counter-clockwise 被 HalfEdge 包围。

给定一个 `halfEdge: HalfEdge`，
的 `halfEdge.prev` 与 `halfEdge.next`
是它在 `halfEdge.face` 上的前一个与后一个 HalfEdge。

下面三个 Face 相同：

- `halfEdge.face`
- `halfEdge.prev.face`
- `halfEdge.next.face`

一个 `halfEdge.twin` 是 `halfEdge` 的另一半，
下面这两个 Face 不同：

- `halfEdge.face`
- `halfEdge.twin.face`

HalfEdge 的优点是用较少的数据描述了邻接关系。
不需要的时候，甚至可以不保存 Vertex 信息。

HalfEdge 也可以被扩展为可以表达 Non Manifold 的变体，
只要考虑 (1/n)-Edge 就可以了。

# SplitEdge

HalfEdge 的 Dual：

- http://groups.csail.mit.edu/graphics/classes/6.838/S98/meetings/m4/VIII.SplitEdge_and_Corner.html

  - 这个文档中还提到了 Corner Data Structure。
