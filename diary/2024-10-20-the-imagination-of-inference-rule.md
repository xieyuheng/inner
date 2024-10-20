---
title: the imagination of inference rule
date: 2024-10-20
---

对 inference rule 和 proof theory 的想象，
或者用 Paul 的话来说 -- proof theory 的 mathematical reality，
其实就是 proof net，
即在已经证明的命题的空间中，
调用一个 inference rule 作用到符合条件的参数上，
就可以生成新的命题。

注意，在 dependent type 中，
以 inductive datatype 的 type constructor 为 judgment（judgment constructor），
以 data constructor 为 inference rule，
所形成的都是不带有计算的 data。

从 inet 的角度看，就是 node，
但是 inet 的 node 之间可以反应，
即可以带有计算。
这与纯粹的 inductive datatype 是不同的。
