---
title: A Little Java
subtitle: Notes about "A Little Java, A Few Patterns"
newline: preserve
---

> The world we live in is so often too complicated
> for the rules we make to keep it orderly.
>
> -- Holmes to Walker, "Elementary | Tremors"

# Notes

## Ralph Johnson 如是说

首先i lave object-oriented and Smalltalk.
学习写程序不只是学习一些程序语言的语法与语义
还要学习如何设计程序
根其他的设计一样人们对于所设计的东西和设计的方式都有不同的理解
这样就形成了联系于不同语言的各种编程范式
而Dan展示了函数试编程和面向对象的结合

## Dan 如是说

oo式的语言支持程序的重用
要想使用好oo必须有设计良好的语言
同时还要遵循一些严格的编程准则
java根oo有关的元素是:
classs objects(instances of classs) fields methods inheritance interfaces
这些元素所组成的核心部分有着简单的语义模型
可以很好的帮助人们来表达自己

- k :: oo正是利用上面的语义来完成自己的使命的
       很容易用alist-processing来实现上面的语义
       但是想要把这种语义嵌在scheme里
       就还需要设计与sexp有关的语法

- x :: 但是没有class的所谓动态oo也可以完成这些使命
       这里核心语义是:
       objects fields methods clone interfaces
       动态性带来了统一处理的困难
       因为类似的对象的具体样子是可以随意改变的

- k :: 既然如此 那就至少说明能完成这些使命的语义不是唯一的
       那么我们可不可以自己设计一种语义来完成这些使命呢??

- x :: 当然可以但是重点还是弄清楚这些``使命''具体是什么
       我们可以通过总结上面两种语义所能完成的效果来回答这个问题
       我们也可以在具体的编程实践中
       通过考察语言的语义所带来的额外复杂性
       和语言的语法所带来的认知上的障碍
       来寻找正确的设计语言的方式

代码重用的关键是设计模式

函数编程核心是
程序是函数
要知道数学意义上的函数
是由它接收输入时给出输出的行为完全决定的
这就要求所写的程序在不同的时间和环境下
接收同样的输入时给出同样的输出

- k :: 说上面的东西是核心
       就是说这些核心与oo并不冲突

motto:

> think first, experiment later.

# runtime

## templet

``` java
public String toString() {
    return "new " + getClass().getName() + "()"; }

public String toString() {
    return "new " + getClass().getName() + "(" + x + ")"; }

public String toString() {
    return "new " + getClass().getName() + "(" + x ", " + y + ")"; }


class HelloWorld {
    public static void main(String args[]) {
        System.out.println("Hello, world");
    }
}


class Main {
    public static void main(String args[]) {
        <DataType_or_Interface> y = new ><;
        System.out.println( ... ) ;
    }
}


// 例子
class Main {
    public static void main(String args[]) {
        PointD y = new ManhattanPt(2,8);
        System.out.println( y.distanceToO ) ;
    }
}

class Main {
    public static void main(String args[]) {
        PiemanI y = new PiemanM();
        System.out.println
            (

             y.addTop(new Anchovy()) + "\n" +
             y.addTop(new Anchovy()) + "\n" +
             y.substTop(new Tuna(),new Anchovy())

             ) ;
    }
}
```

# 1 modern toy

## simple play

``` java
abstract class SeasoningD {}
class Salt extends SeasoningD {}
class Pepper extends SeasoningD {}

new Salt();
new Pepper();

// abstract class : introduces a datatype
// class : introduces a variant
// extends : connects a variant to a datatype

// 求值new Salt()两次并不返回同样的值
// 但是或略这种不同

class Thyme extends SeasoningD {}
class Sage extends SeasoningD {}



abstract class PointD {}
// 上面定义的PointD可以看成是度量空间的集合
// 而下面是两个不同的度量空间
// 即给Z^2空间赋予了不同的度量
// 可以发现这个语言的数学结构语义

class CartesianPt extends PointD {

    int x;
    int y;

    CartesianPt(int _x,int _y) {
        x = _x;
        y = _y;}
    //----------------------------

}
// 上面的下划线是为了增加可读性
// 而对constructor所作的强调

class ManhattanPt extends PointD {

    int x;
    int y;

    ManhattanPt(int _x,int _y) {
        x = _x;
        y = _y;}
    //----------------------------

}

// a constructor is used with new
// to create new instances of a class



abstract class NumD {}
class Zero extends NumD {}
class OneMoreThan extends NumD {

    NumD predecessor;

    OneMoreThan(NumD _p) {
        predecessor = _p;}
    //----------------------------
}

new OneMoreThan(
   new OneMoreThan(
      new Zero()));

// 不是显式扩展别的类的类
// 隐式的扩展Object类
// 也就是说几乎所有的东西都是Object
```

**the first bit of advice**

when specifying a collection of data,
use abstract for datatypes
and extended classes for variants.

## continues the simple play

``` java
abstract class LayerD {}

class Base extends LayerD {
    Object o;
    Base(Object _o) {
        o = _o;}
    //----------------------------
}

class Slice extends LayerD {
    LayerD l;
    Slice(LayerD _l) {
        l = _l;}
    //----------------------------
}


new Base(
   new Zero());
new Base(
   new Salt());


// 上面两个不同类型的东西都可以作为Base的实例
// 但是其实这两个都是``东西''因此也都算是东西这个类型的

// 要知道用new做出来的东西都是Object
// 因此那些基数据结构不是Object
// 下面这两个东西是类型不正确的
new Base(
   5);
new Base(
   false);
// 但是array和string是Object
// 可以发现这些需要在内存中结构化的数据是Object
// 必须得这样写:
new Base(
   new Integer(5));
new Base(
   new Boolean(false));
// 用图来看上面就是把int连到Integer,把boolean连到Boolean
// 这是语言设计上笨拙的地方吗???
```

# 2 methods to our madness

## points

``` java
// PointD可以看成是度量空间的集合
// 而CartesianPt与ManhattanPt是两个不同的度量空间
// 即给Z^2空间赋予了不同的度量
// 可以发现这个语言的数学结构语义

abstract class PointD {
  // 下面定义的抽象方法规定了 所有扩展这个抽象类的类 都有义务定义一个匹配的方法
  abstract int distanceToO();
}


class CartesianPt extends PointD {

  int x;
  int y;

  CartesianPt(int _x,int _y) {
    x = _x;
    y = _y;}
  //----------------------------

  int distanceToO() {
    return (int)Math.sqrt(x*x+y*y);}
}

class ManhattanPt extends PointD {

  int x;
  int y;

  ManhattanPt(int _x,int _y) {
    x = _x;
    y = _y;}
  //----------------------------

  int distanceToO() {
    return x + y;}
}


class run_2_methods_to_our_madness {
    public static void main(String args[]) {
      System.out.println
        (new ManhattanPt(3,4).distanceToO() + "\n" +
         new CartesianPt(3,4).distanceToO()
         ) ;
    }
}
```

## shish

``` java
abstract class ShishD {
  abstract boolean onlyOnions();
  abstract boolean isVegetarian();
}

class Skewer extends ShishD {
  boolean onlyOnions() {
    return true;}
  boolean isVegetarian() {
    return true;}
}

class Onion extends ShishD {
  ShishD s;
  Onion(ShishD _s) {
    s = _s;}
  //----------------------------
  boolean onlyOnions() {
    return s.onlyOnions();}
  // 注意了!!! 递归调用在上面的return后面呢
  // 方法的作用完全根函数的作用反着来...
  // 注意函数以对象为主要参数 这个参数是前置的
  // 然后函数的括号中的才是次要的参数
  // 每个对象包含数据 每个对象也是数据本身 每个对象的名字也是数据

  // 对每个构造子的处理分布在每个构造子中
  // 或者说 一个函数是分好多版本来定义的
  // 每次为了获知需要使用哪个版本的函数 都要明确是什么对象(类的实例)在说话

  // 并且注意这里实例作为函数所能接收的参数
  // 受限制于构造子所初始化的数据

  // 并且构造子的递归特性
  // 在它所能接受的参数对这个类的抽象类自我引用时体现出来
  boolean isVegetarian() {
    return s.isVegetarian();}
}

class Lamb extends ShishD {
  ShishD s;
  Lamb(ShishD _s) {
    s = _s;}
  //----------------------------
  boolean onlyOnions() {
    return false;}
  boolean isVegetarian() {
    return false;}
}

class Tomato extends ShishD {
  ShishD s;
  Tomato(ShishD _s) {
    s = _s;}
  //----------------------------
  boolean onlyOnions() {
    return false;}
  boolean isVegetarian() {
    return s.isVegetarian();}
}

// new Onion(
//   new Lamb(
//     new Onion(
//       new Skewer())));

// 注意 用户想要自定义数据类型 当然要用对象来定义
// 但是 想要体现数据类型是归纳定义出来的就不容易
//      比如与ml不同 根本看不出来上面的数据类型是递归的
//      也看不到所定义的数据类型的列表的本质

// 并且 java这样的语法蒙蔽了``对象就是alist''这个本质
//   ``类就是alist的模板''
//   ``抽象的类就是模板的模板''
//     当发展到这种``模板的模板''的时候其实设计就已经错了
// 所以其实动态的对象是正确的 ``任何东西都可以作为模板以用来构造新的东西''
//   只不过一定要提供对某些重要的需要被集体处理的key以保护机制
// 但是又要明白 只实现alist对于形成面向对象的语义是不够的
// ><>< 具体的还需要实现什么 ???
// ><>< 面向对象的语义究竟是如何完成它的使命的 ???

// 对于 递归的数据类型
// 抽象的类是这种类型
// 而其 具体的类是构造子
// >< 上面的理解可能有问题 毕竟这里不是ml

// new Onion(
//   new Onion(
//     new Skewer()))
//   .onlyOnions();

// 原来形成列表语义的是一系列构造子的作用 其实根ml中一样


class run_2_methods_to_our_madness {
  public static void main(String args[]) {
    System.out.println
      (new Onion(
        new Onion(
          new Skewer()))
       .onlyOnions()
       + "\n" +
       new Onion(
         new Onion(
           new Lamb(
             new Skewer())))
       .onlyOnions()
       + "\n" +
       new Onion(
         new Lamb(
           new Onion(
             new Skewer())))
       + "\n" +
       new Onion(
         new Lamb(
           new Onion(
             new Skewer())))
       + "\n" +
       new Skewer()
       ) ;
  } }
```

**the second bit of advice**

when writing a function over a datatype,
place a method in each of the variants
that make up the datatype.
if a field of a variant belongs to the datatype,
the method may call the corresponding
method of the field in computing the function.

## kebab

``` java
abstract class KebabD {
  abstract boolean isVeggie();
  abstract Object whatHolder();
}

class Holder extends KebabD {
  Object o;
  // 每次必须用副作用来初始化数据 !!!
  // 这是必须的
  Holder(Object _o) {
    o = _o;}
  //----------------------------
  boolean isVeggie() {
    return true;}
  Object whatHolder() {
    return o;}
}

class Shallot extends KebabD {
  KebabD k;
  Shallot(KebabD _k) {
    k = _k;}
  //----------------------------
  boolean isVeggie() {
    return k.isVeggie();}
  Object whatHolder() {
    return k.whatHolder();}

}

class Shrimp extends KebabD {
  KebabD k;
  Shrimp(KebabD _k) {
    k = _k;}
  //----------------------------
  boolean isVeggie() {
    return false;}
  Object whatHolder() {
    return k.whatHolder();}
}

class Radish extends KebabD {
  KebabD k;
  Radish(KebabD _k) {
    k = _k;}
  //----------------------------
  boolean isVeggie() {
    return k.isVeggie();}
  Object whatHolder() {
    return k.whatHolder();}
}

// 发现了OO的好处之一
// 想要扩张函数很容易 :
// 只要给抽象的类添加具体的类就行了
// 但是添加新的函数的时候就麻烦了:
// 必须把处理方式逐个添加到每种类型的变体中
// 所以 :
// 如果你总是需要增添新的函数 来处理相对标准的数据类型
// 那么你就别用OO而去用函数式编程
// 如果你总是使用一组标准的处理方式 但是总是需要扩展数据类型
// 那么你就使用OO而别用函数式编程
//   要知道在scheme中自己定义数据类型是很烦人的事
//   因为其实只有列表可以使用
//   但是你必须给这些列表设计出不同的语义
class Pepper extends KebabD {
  KebabD k;
  Pepper(KebabD _k) {
    k = _k;}
  //----------------------------
  boolean isVeggie() {
    return k.isVeggie();}
  Object whatHolder() {
    return k.whatHolder();}
}


abstract class RodD {}
class Dagger extends RodD {}
class Sabre extends RodD {}
class Sword extends RodD {}

abstract class PlateD {}
class Gold extends PlateD {}
class Silver extends PlateD {}
class Brass extends PlateD {}
class Copper extends PlateD {}
class Wood extends PlateD {}


class run_2_methods_to_our_madness {
  public static void main(String args[]) {
    System.out.println
      (new Shallot(
        new Pepper(
          new Holder(
            new Dagger())))
       + "\n" +
       new Shallot(
         new Radish(
           new Holder(
             new Dagger())))
       .isVeggie()
       + "\n" +
       new Shallot(
         new Radish(
           new Holder(
             new Dagger())))
       + "\n" +
       new Shallot(
         new Pepper(
           new Holder(
             new Integer(52))))
       .isVeggie()
       + "\n" +
       new Shallot(
         new Radish(
           new Holder(
             new Boolean(false))))
       .isVeggie()
       + "\n" +
       new Shallot(
         new Radish(
           new Holder(
             new Holder(1))))
       .isVeggie()
       + "\n" +
       new Shallot(
         new Radish(
           new Holder(
             new Integer(52))))
       .whatHolder()
       + "\n" +
       new Shallot(
         new Radish(
           new Holder(
             new Holder(1))))
       .whatHolder()
       ) ;
  } }
```

## points again

``` java
abstract class PointD {
  int x;
  int y;
  PointD(int _x, int _y) {
    x = _x;
    y = _y;}
  //----------------------------

  // 下面的这个函数不应该在这里被抽象地声明
  // 因为一个空间只是单纯的点集而已 不一定有度量
  // 其次 两个度量空间度量可能是完全不同的 因此可能是无法比较的
  // 所以可以看出 对这些类的不同设计如何反映了不同人对问题的不同理解
  abstract int distanceToO();
  boolean closerToO(PointD p) {
    return distanceToO() <= p.distanceToO(); }
}

class CartesianPt extends PointD {
  CartesianPt(int _x,int _y) {
    super(_x,_y);}
  //----------------------------
  int distanceToO() {
    return (int)Math.sqrt(x*x+y*y);}
}


// OO的语法就是
// 把lisp的 (<FUNCTION> <arg1> <arg2> <arg3> ...)
// 换成为   (<arg1> <FUNCTION> <arg2> <arg3> ...)
// 这样的语法更容易被人接受的原因是
// 英语是以 主 + 谓 + 宾, 为简单句结构的

class ManhattanPt extends PointD {
  ManhattanPt(int _x,int _y) {
    super(_x,_y);}
  //----------------------------
  int distanceToO() {
    return x + y;}
}



class run_2_methods_to_our_madness {
    public static void main(String args[]) {
      System.out.println
        (new ManhattanPt(3,4)
         + "\n" +
         new ManhattanPt(3,4).distanceToO()
         + "\n" +
         new CartesianPt(3,4).distanceToO()
         + "\n" +
         new ManhattanPt(3,4).closerToO(new ManhattanPt(1,5))
         + "\n" +
         new CartesianPt(2,5).closerToO(new CartesianPt(3,4))
         + "\n" +
         new ManhattanPt(3,4).closerToO(new CartesianPt(3,4))
         ) ;
    } }
```

# 3 what is new

**the third bits of advice**

when writing a function that returns
values of a datatype,
use new to create these values.

## pizza

``` java
abstract class PizzaD {
  abstract PizzaD removeAnchovy();
  abstract PizzaD topAnchovyWithCheese();
  abstract PizzaD substituteAnchovyByCheese();
}

class Crust extends PizzaD {
  PizzaD removeAnchovy() {
    return new Crust();}
  PizzaD topAnchovyWithCheese() {
    return new Crust();}
  PizzaD substituteAnchovyByCheese(){
    return new Crust();}
}

class Cheese extends PizzaD {
  PizzaD p;
  Cheese (PizzaD _p) {
    p = _p;}
  //----------------------------
  PizzaD removeAnchovy() {
    return new Cheese(p.removeAnchovy());}
  PizzaD topAnchovyWithCheese() {
    return new Cheese(p.topAnchovyWithCheese());}
  PizzaD substituteAnchovyByCheese(){
    return new Cheese(p.substituteAnchovyByCheese());}
}

class Olive extends PizzaD {
  PizzaD p;
  Olive (PizzaD _p) {
    p = _p;}
  //----------------------------
  PizzaD removeAnchovy() {
    return new Olive(p.removeAnchovy());}
  PizzaD topAnchovyWithCheese() {
    return new Olive(p.topAnchovyWithCheese());}
  PizzaD substituteAnchovyByCheese(){
    return new Olive(p.substituteAnchovyByCheese());}
}

class Anchovy extends PizzaD {
  PizzaD p;
  Anchovy (PizzaD _p) {
    p = _p;}
  //----------------------------
  PizzaD removeAnchovy() {
    return p.removeAnchovy();}
  PizzaD topAnchovyWithCheese() {
    return new Cheese(new Anchovy(p.topAnchovyWithCheese()));}
  // PizzaD substituteAnchovyByCheese(){
  //   return p.topAnchovyWithCheese().removeAnchovy();}
  PizzaD substituteAnchovyByCheese(){
    return new Cheese(p.substituteAnchovyByCheese());}
}

class Sausage extends PizzaD {
  PizzaD p;
  Sausage (PizzaD _p) {
    p = _p;}
  //----------------------------
  PizzaD removeAnchovy() {
    return new Sausage(p.removeAnchovy());}
  PizzaD topAnchovyWithCheese() {
    return new Sausage(p.topAnchovyWithCheese());}
  PizzaD substituteAnchovyByCheese(){
    return new Sausage(p.substituteAnchovyByCheese());}
}

// 此时我很容易添加一个新的数据类型(一个具体的类)
// 而不影响程序其他的部分
// >< 这种效果是OO的语义主要想要达到的吗???

// 并且 如果能动态的向类添加方法和数据
// 0. 在程序运行的过程中我就能改变程序本身
// 1. 我可以把方法写到一起了
// 2. 同时也保留了把方法分开写的可能
// 这肯定是有力的特性
// 但是其实就是对alist的副作用而已

class run_3_what_is_new {
  public static void main(String args[]) {
    System.out.println
      (new Anchovy(
         new Anchovy(
           new Anchovy(
             new Cheese(
               new Olive(
                 new Crust())))))
       .removeAnchovy()
       + "\n" +
       new Anchovy(
         new Crust())
       .topAnchovyWithCheese()
       ) ;
  } }
```

# 4 come to our carousel

## the so called visitor

``` java
class OnlyOnionsV {
  boolean forSkewer() {
    return true;}
  boolean forOnion(ShishD s) {
    return s.onlyOnions();}
  boolean forLamb(ShishD s) {
    return false;}
  boolean forTomato(ShishD s) {
    return false;}
}
// 注意与不再visitor中的函数相比这里的函数多了一个参数


abstract class ShishD {
  // protocols:
  OnlyOnionsV ooFn = new OnlyOnionsV ();
  IsVegetarianV ivFn = new IsVegetarianV();
  abstract boolean onlyOnions();
  abstract boolean isVegetarian();
}


class Skewer extends ShishD {
  boolean onlyOnions() {
    return ooFn.forSkewer();}
  boolean isVegetarian() {
    return ivFn.forSkewer();}
}

class Onion extends ShishD {
  ShishD s;
  Onion(ShishD _s) {
    s = _s;}
  //----------------------------
  boolean onlyOnions() {
    return ooFn.forOnion(s);}
  boolean isVegetarian() {
    return ivFn.forOnion(s);}
  // 注意是如何间接地在类中定义方法的
}

class Lamb extends ShishD {
  ShishD s;
  Lamb(ShishD _s) {
    s = _s;}
  //----------------------------
  boolean onlyOnions() {
    return ooFn.forLamb(s);}
  boolean isVegetarian() {
    return ivFn.forLamb(s);}
}

class Tomato extends ShishD {
  ShishD s;
  Tomato(ShishD _s) {
    s = _s;}
  //----------------------------
  boolean onlyOnions() {
    return ooFn.forTomato(s);}
  boolean isVegetarian() {
    return ivFn.forTomato(s);}
}


class IsVegetarianV {
  boolean forSkewer() {
    return true;}
  boolean forOnion(ShishD s) {
    return s.isVegetarian();}
  boolean forLamb(ShishD s) {
    return false;}
  boolean forTomato(ShishD s) {
    return s.isVegetarian();}
}



class run_4_come_to_our_carousel {
    public static void main(String args[]) {
      System.out.println
        (new Onion(
          new Onion(
            new Skewer()))
         .onlyOnions()
         + "\n" +
         new Onion(
           new Onion(
             new Lamb(
               new Skewer())))
         .onlyOnions()
         + "\n" +
         new Onion(
           new Onion(
             new Skewer()))
         .isVegetarian()
         + "\n" +
         new Onion(
           new Onion(
             new Lamb(
               new Skewer())))
         .isVegetarian()
         ) ;
    } }
```

**the fourth bit of advice**

when writing several functions
for the same self-referential datatype,
use visitor protocols so that
all methods for a function can be found in a single class.

## 改写前面 pizza 的例子作为练习

尽管非常无聊 但是还是熟悉一下这种写法吧

``` java
abstract class PizzaD {
  RemoveAnchovyV remFn
    = new RemoveAnchovyV();
  TopAnchovyWithCheeseV topFn
    = new TopAnchovyWithCheeseV();
  SubstituteAnchovyByCheeseV substFn
    = new SubstituteAnchovyByCheeseV();
  abstract PizzaD removeAnchovy();
  abstract PizzaD topAnchovyWithCheese();
  abstract PizzaD substituteAnchovyByCheese();
}

class RemoveAnchovyV {
  PizzaD forCrust() {
    return new Crust();}
  PizzaD forCheese(PizzaD p) {
    return new Cheese(p.removeAnchovy());}
  PizzaD forOlive(PizzaD p) {
    return new Olive(p.removeAnchovy());}
  PizzaD forAnchvy(PizzaD p) {
    return p.removeAnchovy();}
  PizzaD forSausage(PizzaD p) {
    return new Sausage(p.removeAnchovy());}
}

class TopAnchovyWithCheeseV {
  PizzaD forCrust() {
    return new Crust();}
  PizzaD forCheese(PizzaD p) {
    return new Cheese(p.topAnchovyWithCheese());}
  PizzaD forOlive(PizzaD p) {
    return new Olive(p.topAnchovyWithCheese());}
  PizzaD forAnchvy(PizzaD p) {
    return new Cheese(new Anchovy(p.topAnchovyWithCheese()));}
  PizzaD forSausage(PizzaD p) {
    return new Sausage(p.topAnchovyWithCheese());}
}

class SubstituteAnchovyByCheeseV {
  PizzaD forCrust() {
    return new Crust();}
  PizzaD forCheese(PizzaD p) {
    return new Cheese(p.substituteAnchovyByCheese());}
  PizzaD forOlive(PizzaD p) {
    return new Olive(p.substituteAnchovyByCheese());}
  PizzaD forAnchvy(PizzaD p) {
    return new Cheese(p.substituteAnchovyByCheese());}
  PizzaD forSausage(PizzaD p) {
    return new Sausage(p.substituteAnchovyByCheese());}
}

class Crust extends PizzaD {
  PizzaD removeAnchovy() {
    return remFn.forCrust();}
  PizzaD topAnchovyWithCheese() {
    return topFn.forCrust();}
  PizzaD substituteAnchovyByCheese(){
    return substFn.forCrust();}
}

class Cheese extends PizzaD {
  PizzaD p;
  Cheese (PizzaD _p) {
    p = _p;}
  //----------------------------
  PizzaD removeAnchovy() {
    return remFn.forCheese(p);}
  PizzaD topAnchovyWithCheese() {
    return topFn.forCheese(p);}
  PizzaD substituteAnchovyByCheese(){
    return substFn.forCheese(p);}
}

class Olive extends PizzaD {
  PizzaD p;
  Olive (PizzaD _p) {
    p = _p;}
  //----------------------------
  PizzaD removeAnchovy() {
    return remFn.forOlive(p);}
  PizzaD topAnchovyWithCheese() {
    return topFn.forOlive(p);}
  PizzaD substituteAnchovyByCheese(){
    return substFn.forOlive(p);}
}

class Anchovy extends PizzaD {
  PizzaD p;
  Anchovy (PizzaD _p) {
    p = _p;}
  //----------------------------
  PizzaD removeAnchovy() {
    return remFn.forAnchvy(p);}
  PizzaD topAnchovyWithCheese() {
    return topFn.forAnchvy(p);}
  PizzaD substituteAnchovyByCheese(){
    return substFn.forAnchvy(p);}
}

class Sausage extends PizzaD {
  PizzaD p;
  Sausage (PizzaD _p) {
    p = _p;}
  //----------------------------
  PizzaD removeAnchovy() {
    return remFn.forSausage(p);}
  PizzaD topAnchovyWithCheese() {
    return topFn.forSausage(p);}
  PizzaD substituteAnchovyByCheese(){
    return substFn.forSausage(p);}
}

class run_4_come_to_our_carousel {
  public static void main(String args[]) {
    System.out.println
      (new Anchovy(
        new Anchovy(
          new Anchovy(
            new Cheese(
              new Olive(
                new Crust())))))
       .removeAnchovy()
       + "\n" +
       new Anchovy(
         new Crust())
       .topAnchovyWithCheese()
       ) ;
  } }
```

## about visitor

- k :: visitor就是用一个class把需要的方法都集成起来
       这是对上节所提出的问题的解决方案之一
       也许它比单纯的用动态对象给出的解决方案要复杂一点
       这种间接的解决方案
       具有间接性的同时 还改变了使用函数的方式
       (其实并不改变在外面使用函数的方式)
       之所以有这样的复杂性
       是因为提出这种解决方案的人把自己限制在了java所提供的OO语义之中
       也就是说这中复杂性是语言的语义所带来额额外复杂性

- x :: 我想这种解决方案确实是不令人满意的
       比如Dan在书中就抱怨使用visitor的过程很乏味
       这也许就是这种解决方案的不简洁而导致的
       其他的语言(比如ruby)如何呢??
       为了设计好我们的OO一个也许我们还应该借鉴更多别的语言的经验

- k :: 我发现对语义的限制总是不好的

- x :: 不能这么说
       考虑一下不怎么限制语义的语言 比如C和汇编
       相比于它们我更愿意使用限制语言的高级语言

- k :: 我想问题也许在于 这些低级语言并不是没有限制语义
       而是根本就没有什么语义
       它们的语义很单一 不是吗?
       只不过是控制机器而已

- x :: 也就是说尽管低级语言 能够直接控制内存
       因而在表面上看来人们在使用它们时 人们就获得着控制机器的最大的自由
       但是我们不能说它具有丰富的语义

- k :: 没错

- x :: 我想这些低级语言存在的问题是
       它们没有抽象能力
       或者抽象能力很低
       或者进行抽象的方式不简洁
       但是让我们先放下这个问题 退一步问一问什么是「语义」?
       显然当我们连这个东西的意思都没有弄明白的时候就去讨论它
       可能永远都会是含糊不清而没有结果的

- k :: 好的 这看来是个很难的问题
       让我们先记住这个问题还有这个问题之后我们需要返回的问题
       让我们先休息一下
       我想回来之后我们讨论的第一个切入点就是要弄清楚
       在上面的讨论中当我们使用「语义」这个词的时候我们指的是什么

- x :: 好的 我们先继续作一些书中的练习

------

- x :: 我想我可以对语义这个名词形成很多种不同的理解方式
       但是我们可以以人为中心
       比如对一个形式语言来说
       对这个形式语言中的表达式
       人所能形成的理解方式就是语义
       人对各种不同的表达式能够形成各种不同的理解
       那么这些表达式就有不同的语义
       人对同一个表达式能够形成许多不同的理解
       那么这个表达式就有多种语义
       这个表达式就是有歧义的
       比如(lat? (1 2 3))
       一个人可以把它理解为一个嵌套的列表
       也可以把它理解为一个函数的作用

- k :: 我想我可以质疑你这里``以人为中心''这个基础
       但是我们总是需要一些基础才能讨论下去
       所以请你继续

- x :: 好的
       然后我们可以考察一下我们为什么会创造出``语义''这个术语
       我想是因为我们发现
       使用这个术语我们就能对我们所观察到的
       某些 事件 或 行为 或 现象 形成更结构化的理解方式

- k :: 也许我们使用某个术语的目的总是为了形成更好的理解与交流
       但是是对什么东西形成更好的理解呢??

- x :: 当然是对``理解与交流''本身形成更好的理解了
       所以也许我可以说``语义 的 语义 就是 语义''

- k :: 天哪 x
       这样就是说我们不能说出``语义''是什么意思吗
       我们不能说出语义的语义是什么只能说它是语义
       或者随便说它是任何东西吗?

- x :: 朋友
       也许我们已经漫步到了语言的边界了呢
       也许像我们的另一个名叫k的朋友说的一样
       要想获得真正的智慧只能依靠顿悟呢
       但是我不知道

- k :: 好吧 也许是该回去的时候了
       让我们回到之前对程序语言的讨论吧

- x :: 回到对某些我们不喜欢的机器语言的讨论
       我想你说它的语义很单一
       是因为虽然我们能够利用它们来以最精细的方式控制硬件
       但是对于它所能表达的全部东西 我们却只能形成单调的理解方式
       这就让它变得非常枯燥

- k :: 但是等等
       既然我们承认利用它们我们能够以最精细的方式操作机器
       为什么我们又说它所能表达的东西很单调呢?
       我明白了!
       是因为比如说我用汇编语言写了一个编译器
       但是我不能混淆我们对那个编译器所实现的语言中的表达式的理解方式
       与我们对实现这个编译器所用的汇编语言本身之中的表达式的理解方式

- x :: 其实这两种理解方式之间是有联系的
       毕竟那个编译器是用这个汇编语言来写的
       但是这种联系太复杂与琐碎
       让我去理解它就像让我去理解
       人们现在所找到的最大的质数那么多个沙粒堆积在一起的样子一样
       我是不愿意去想像的
       所以我干脆说它们之间没有联系

- k :: 我想我们可以总结一下这些讨论对我们设计语言有什么启发

- x :: 好的
       对我来说启发就是
       我现在明白我们应该让语义尽量丰富
       尽量不要去限制语言的语义
       这也就是不去限制编程者的想象力与对程序的理解方式

- k :: 我们还走到了语言的边界
       发现那里写着``语义的语义就是语义''
       不是吗?

- x :: 哈哈
       没错

# 5 objects are people too
# 6 boring protocols
# 7 oh my
# 8 like father like son
# 9 be a good visitor
# 10 the state of things to come
