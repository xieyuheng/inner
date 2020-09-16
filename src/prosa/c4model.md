# c4model

2020-09-16

今天看了：https://c4model.com

它将构架分为了四个层次的 diagram 来描述，
分别是：
- context -- software system in context
- container -- separately runnable/deployable unit
- component -- major structural building blocks (of container) and their interactions
- code -- class or datatype and function

## 只用 class 来做抽象

其中描述 code 这个层次的 diagram 时，
作者用到了 java 中的 interface 与 impl-class，
类似 typescript 中的 interface 与 class implement，
或者 typescript 中的 abstract class 与 class extends。

这种关系其实类似 class 与 instance。
语言中有很多形成这种关系的方式，
我们尝试只用 class 与 instance，
或者更极端地，我们用 higher function 与 function。

## 「属于」与「子类型」关系

其中描述 Container 与 Component 元素的时候，
没一个元素有 name 还有 technology。

比如：
- Web Application [Container: Nodejs]
- API Application [Container: Nodejs]
- Single-Page Application [Container: Sveltejs]
- Database [Container: PostgreSQL]

如何用类型与元素之间的「属于」关系，
以及类型与类型之间的「子类型」关系，
来更精确地描述上面的信息呢？

或者是：
- Web Application : Nodejs <: Container
- API Application : Container <: Nodejs
- Single-Page Application : Container <: Sveltejs
- Database : Container <: PostgreSQL

又或者是：
- Web Application : Nodejs : Container
- API Application : Container : Nodejs
- Single-Page Application : Container : Sveltejs
- Database : Container : PostgreSQL

「属于」关系可否带有传递性，
而被视为是特殊的「子类型」关系呢？
