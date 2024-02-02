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

```typescript
type Vertex = {
  incidentEdges: Array<Edge>
}

// To decide left, right, clockwise and counterclockwise,
// we should view the oriented edge from outside of the solid.

type Edge = {
  startVertex: Vertex, endVertex: Vertex
  leftFace: Face, rightFace: Face
  clockwisePrevEdge: Edge
  clockwiseNextEdge: Edge
  counterclockwisePrevEdge: Edge
  counterclockwiseNextEdge: Edge
}

type Face = {
  boundingEdges: Array<Edge>
}
```

# 3 Boolean Operations on Boundary Representation

# 4 Robust and Error-Free Geometric Operations

# 5 Representation of Curved Edges and Faces

# 6 Surface Intersections

# 7 Gröbner Bases Techniques
