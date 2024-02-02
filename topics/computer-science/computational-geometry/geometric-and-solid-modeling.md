---
title: Geometric and Solid Modeling
author: Christoph M. Hoffmann
year: 1989
---

# 1 Introduction

# 2 Basic Concepts

## 2.3 Boundary Representations

This description has two parts,

- a topological description of the connectivity and orientation of
  vertices, edges, and faces,

- and a geometric description for embedding these surface elements in
  space.

Historically, the representation evolved from a description of
polyhedra.

### 2.3.2 Winged-Edge Representation

这种表示起源于描述 Polyhedra，
由于每个 Edge 只能有两个 Face，
所以只能描述 Manifold。

```typescript
type Vertex = {
  incidentEdges: Array<Edge>
}

// To decide left, right, clockwise and counterclockwise,
// we should view the oriented edge from outside of the solid.

type Edge = {
  startVertex: Vertex, endVertex: Vertex

  leftFace: Face
  counterclockwisePrevEdge: Edge
  counterclockwiseNextEdge: Edge

  rightFace: Face
  clockwisePrevEdge: Edge
  clockwiseNextEdge: Edge
}

// 这里 `boundingEdges` 中的边，按照某个选定的定向法则（比如右手螺旋法则），组成一个圈。
// 代表就一个 Edge 而言，拿到 Face 之后可以计算出 PrevEdge 和 NextEdge，
// 这些信息是多余的，只是为了常数时间的读取。

type Face = {
  boundingEdges: Array<Edge>
}
```

# 3 Boolean Operations on Boundary Representation

## 3.2 Representation Conventions

是 Winged-Edge 的变体，但是支持 Non Manifold。

```typescript
type Vertex = {
  incidentEdges: Array<Edge>
  incidentFaces: Array<Face>
}

// The `incidentFaces` are cyclically ordered according to
// how they intersect a plane normal to the edge.

// 注意这里没有 PrevEdge 和 NextEdge 之类的冗余信息。

type Edge = {
  startVertex: Vertex, endVertex: Vertex
  incidentFaces: Array<Face>
}

type Face = {
  boundingVertexes: Array<Vertex>
  boundingEdges: Array<Edge>
}
```

# 4 Robust and Error-Free Geometric Operations

# 5 Representation of Curved Edges and Faces

# 6 Surface Intersections

# 7 Gröbner Bases Techniques
