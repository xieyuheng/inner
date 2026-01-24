---
title: Structure and Interpretation of Computer Programs -- Video Lecture
authors: [Harold Abelson, Gerald Jay Sussman, Julie Sussman]
date: timeless
---

# 1A

层次结构是为了控制复杂性
把复杂性控制在人类所能理解的范围内

其实
程序设计中的一大部分知识
是关于如何控制这种复杂性的
[关于表达的 而不是算法的时空消耗]

一个基本的控制表达的复杂性的方法是
设计更特殊化的新编程语言

有趣的是 在纯数学中
总想要把函数看成是点集
而在计算机科学中 发展了一套形式语言去描述过程
因此函数总被看成是对数据的操作
显然 后者更具体 它的语义更丰富
[例如 在 lambda-cal 中函数被理解为 代入转写]

如何描述对与一个数学结构族而言的抽象函数[generic operation]
使得它可以使用于结构族中各个具体的结构
[例如以线性空间族 与线性空间]
- [2026-01-24] abstract class 或 interface 的概念，
  对应于抽象的数学结构。

to show a language is to show
1. primitive elements
2. means of combination
3. means of abstraction

in lisp
1. primitive data & primitive procedures
2. using ( ) cond cons if
3. using define

构建和理解复杂系统的普遍方法：
识别原子单元，指定组合规则，并引入抽象以管理复杂性。
这不仅是语言的特征，也是数学、计算机科学和认知科学中的一般原则。

# 1B

the key to understanding complicated things
is to know what not to look at
and what not to compute
and what not to think

using Substitution Rule to explain evaluation (temporarily)

去学习这些术语
之后我们就可以用这些术语进行讨论了
one fo the things that every sorserer will tell you
is if you have the name of a spirit, you have power over it

培养直觉
一个特殊形状的 procedure or expression
如何给出一个特殊形状的 process

关于 迭代 递归 尾部递归 算法复杂度

# 2A

用一级一级的抽象来使程序变得容易理解
这就是所说的
用层级结构来控制[就表达而言的]复杂性

这种复杂性关乎于
一个人如何表达一个东西[比如对算法的表达]
而使得所作出的表达
易于被另一个人[或者将来的自己]所理解

比如 这样的层次结构还使得
写程序的人可以从 总体的性质 入手
从一个大的观念入手
而不必过早的去考虑 大的过程中的细节过程 是如何实现的
[即所谓的 wishful thinking]
可以说当使用这样的函数式编程范式时
就像是 lisp 语言在教你如何解决问题
即 去构造层次

# 2B

about compound data
just like compound procedure

构建数据结构 同对过程的抽象一样 也是一种构建层次结构的手段
形成层次的理由是相同的:
1. 表达清晰 易于理解
2. 解构之后的各个部分更易于修改
3. 使人们可以从大观念入手来构造程序 并且保持灵活性
在程序极端复杂庞大的情形 这些不仅仅是优势 而处理复杂问题的必要条件
并且某种程度上 在使用这样的范式时
你会更进一步学习到关于所处理的问题的知识
因此在解决问题的过程中获得解决问题的能力

而劣势在于:(反对意见相当单薄而易于反驳)

1. 降低了机器那方面的效率
   提高了人类的效率，并且这个劣势可以用好的编译器来弥补

2. 解构所带来的分离性

   - 这样的分离性必须是绝对的
     即使得低一层次的改变(同时低一层必须保证自己完成了高一层次所部署下来的任务)
     完全不影响高一层次的工作

   可能使不同的工作这之间不能触及彼此的工作

   - See, in general, as systems designers,
     you're forced with the necessity to make decisions
     about how you're going to do things,
     and in general, the way you'd like to retain flexibility is to
     never make up your mind about anything until you're forced to do it.

3. deferring decisions 有可能变成 outright procrastination
   推迟抉择 有可能变成 彻底的拖延

the whole name of this scheme is that
we'd like the programming language to express the concepts
that we have in our heads

当一个存储单元存储另一个存储单元的地址
这样所形成的关系满足图论中的有向图的定义
所以就用箭头来表示，所以就有指针这个术语

术语 Closure 在数学结构中很常见，指一种完备性
例如，通过 cons 得到的元素对本身也可以作为 cons 的对象来再次形成元素组
即 cons 操作(运算)在 lisp 的所有数据集合中的封闭性
+ 数学中的经验是:
  有些时候去判断所定义出的一个过程
  或者形成新数据结构的组合方式
  是否具有某种程度的封闭性是本质重要的
  因为这种就不封闭性的完备化可以作为一个非常普使的思路来理解很多东西
+ 这才是闭包这个术语的意义
  "把环境包起来"只是表象
  即为了使得λ-term能够在某些运算下完备

完备化函子的实例：

- λ闭包：从 语法项 到 语义值 的完备化
- 数学闭包：从 子结构 到 完备结构 的完备化

# 3A

list in lisp is essentially just
a conventional way for representing a sequence

meta-linguistic abstraction

如之前已经申明的，给出一个语言就在于:
1. primitives
2. meams of combination
3. means of abstraction

又一次这些东西让人想起数学结构，
只不过与静态的数学结构相比，
比如第3条指明如何用 基本的元素 与 基本的操作
来定义复杂的元素与操作。

联想一直困扰我的数学结构的层次问题!
我有一个 一般的规则
来从 起初只有基本后继关系的自然数结构(或者其他任何具体的结构)
衍生出包含 自然数结构 的更复杂结构，这个规则是:
1. 去需找所有可能的运算(多元的或一元的)，
   此时一种对于运算的有意义的分类是它们的对称性，
   即它们所能满足什么样的运算律
2. 同态概念(等等类似的概念)是基本的
   而且是在所给出的基本结构之外的
3. 从 自然数 的 后继关系 能够 构造出 加法
   在于 后继关系的自同态的集合
   (以结构中的 某些已有运算 为基础的
    满足某些运算律(在自然数的例子中 运算律由 同态 指出)的
    结构的基础集合上的 函数的集合)
   能够在某种意义上与 自然数集等同;
   从 自然数 加法运算 能够构造出 乘法运算
   在于 加法运算的自同态的集合
   能够在某种意义上与自然数集等同;
   从乘法运算能够构造出加法运算
   在于乘法运算的自同态的一部分
   能够与自然数集等同。
4. 如果某些(n元)运算的集合能够在某种意义下与结构的基本集本身等同，
   那么就可以形成一种(n+1元)运算，
   如果这些运算的集合满足某些运算律
   那么这样形成的新运算就是有意以的或者有趣的。
5. 以上都是在某个具体的数学结构的运算方面(更广泛的关系方面)作扩展定义，
   同样重要的是在结构的基本集方面(结构的元素方面)作补充定义，
   这在于 对所引入的 新运算 以及它们的逆运算 作完备化，
   即补充定义新的元素使得运算完备。
6. 从某种意义上来说 实数以及几乎全部数学
   都 以这种方式 内蕴于 自然数集。

(至少从形式上看来)这与所谓的 meta-linguistic abstraction 有很大的共通之处!
现在问题来了，我能在某种程度上，利用 lisp 实现一个有趣的东西来介绍这些概念吗?
这样的实现必须新颖并且意义丰富。

这里所介绍的 meta-linguistic abstraction 是 Henderson-Escher example。

here because the operations are closed,
we could built up complexity so fast.
(这是 embedded 所要求的性质吗?)

a language embedded in lisp.
lisp 作为强大的工具来处理和扩展 这个嵌入 lisp 的语言。
课程里想要展示的就是如何把一个语言嵌入 lisp 中，
这比在 lisp 中实现一个语言要好得多，
因为它使得你不失 lisp 的全部原始力量(比如定义高阶过程的能力)。
也就是说，用形成了一个多层次结构的嵌入 lisp 语言链 来完成任务(解决问题)，
比用 把任务分解成任务树再来分部解决要好的多(两者都是用来控制复杂性的方法)。
比如 这样能获得 健壮性
(insensitive to small changes:
a small change in the problem
should lead to only a small change in the solution.
There ought to be a continuity.
The space of solutions ought to be continuous
in this space of problems.)
对某个层次的语言中元素的改变可以被 高一层次捕获 而不影响整体。
同时构建语言链，使得你用大量的词汇来描述一些细节性质，
这使你对问题的理解更透彻，并且这带来了解决问题的灵活性。

# >< 3B

# 4A

关于模式匹配 与 在指定规则下的替换。
当想要描述一个替换规则时所使用的
尤其是在符号计算中经常需要使用(用于对符号表达式的化简)

方法就是
1 匹配(有固定的规则 并且 用到 通配符)
2 替换

# 4B

Generic operator means what it sort of precisely does
depends on the kind of data that it's looking at.

以复数的运算为例。
(注意:与我的问题相联系的是，
每当结构扩张，都需要补充定义运算
以实现这种运算符重载。)

typed data comes now!
dispatch on type.

首先，利用添加标签来实现，给不同类型的数据顺便贴上标签。
之后，为了使新的数据结构容易被增加进来，
把那个查表的管理员踢了，而直接用那张表。

data-directed programming.
这时会作出标签链的。
层次结构又出现了!

decentralized control

练习1:
去发现Galois中可以用于符号计算 或一般计算的 题目，
然后用 lisp 来实现，注意 要用4B中的方法。

练习2:
计算数论中的计算题目。

练习3:一阶语言。

练习4:公理集合论。

# 5A Assignment, State, and Side-effects

- 问题 1 -
  一个人
  对 描述性(普遍性)知识
  与 过程性(计算性)知识 的理解是统一的
  那么机器如何做到这一点?
  - 比如 机器可以在计算一个表达式之前 先审视这个表达式
    用形式规则沿某一方向 找出一些等价的表达式
    即它们的计算结果将是相同的
    但是这些形式规则是人告诉机器的
    并不是机器通过它所有能力执行的那个计算本身来获得的
    而计算本身理应包含这些形式规则
    人既知道自然数有加法 又知道加法有交换律
    而如何让机器把 就同一个具体的数学结构的
    数值计算与符号计算相结合?
  - 可计算性是什么意思?
    它限制机器使得它不能获得这种能力吗?

- 问题 2 -
  机器可不可以看着一个具体的数学结构
  然后用 提高运算 级别的方法去扩展这个具体的数学结构?
  机器如何理解数学结构?
  - 考虑 lisp 作为形式语言本身而形成的数学结构试试!
    此时结构的基本集合为所有的 S-表达式
    具有潜在的无穷性
    而且 lisp 本身并没有储存所有的结构的基本集合中的元素
    之后还有一些对这些 S-表达式 的基本操作
    可是关于这些操作的一般性知识是在形式语言之外证明的
    - 如何理解 lisp 可以在 lisp 之内实现?
    还有 lambda 与 cond 它们使得形式语言能用来表达过程

- 练习 1 -
  去用列表实现自然数结构

- 事实1 -
  描述性知识描述一些具有普遍性的定理 例如 加法交换律
  而计算时 我们发现 以两种方式计算两个具体的数的加法
  它们的结果是相等的

- 事实 2 -
  运算律 可以很好的用形式化的置换规则描述
  - 甚至我们可以构造一个 更一般的
    可以任意指明某种目的 对表达式的化简方向
    - 这可以作为一个练习 -- 练习 2

- 观点 1 -
  以后继关系为基本关系的自然数集 和其中的加法交换律
  都可以作为统计性知识 (在实际的计算实践中) 而习得
  而形式的逻辑规则 是在我们考虑这些(普遍性)知识之间的关系时
  作为统计性知识被习得
  逻辑指明命题之间的序关系
  加法交换律可以作为结论由自然数集的基本后继关系而推出

回到课程本身 ~

set! comes now!

用这个 词 之后 表达式的求值结果就与时间有关了!
side-effect!
这样就 出离 函数式编程范式了
函数的行为不再一致了
- 不再与时间无关
- 不再像一个数学函数了

明白什么时候自己的代码在函数式编程范式之内
而什么时候在函数式编程范式之外是很重要的

then comes the environment model here
- 为了引入对自由变元的求值
since the sbubstitution model fail
- 它只适用于约束变元的情形

- 老师的观点 1 -
  object 这个术语在于
  人们的为了思维的经济性
  而把在细致地描述某个集合的性质时所观察到的
  集合的 (就所描述的性质而言) 基本上相互独立的两个子集分离开
  把它们作为两个整体称为两个对象
  使得在之后的讨论中不必再深入细节

- 老师的观点 2 -
  这样的分离有时并不恰当
  比如在量子力学中
  有时实际上被我们为了经济性而分离了的
  所谓两个对象之间的联系比表面上的更多
  有时我们甚至为了思维的经济性而拒绝承认这一点
  而我们认为量子力学很难就在与我们这样的思维习惯
  因为我们正是被训练得去这样思维的
  这使我们不得要领 (比如爱因斯坦对量子力学的观点)
  思维的经济性 很值得思考的一点

- 老师的观点 3 -
  about actions and identity
  物体 (identity) 的相等与不等
  是就某些可以所用于他们的作用 (actions) 而言的
  (类比 克莱因 埃尔朗根纲领)
  但是有趣的
  例如
  考虑一个自然数轴上的映射
  它把第三个点移动到第四个点
  或者由指向第三个点变成指向第四个点
  但是不论如何总有一个客体好像是前后不变的 -- 点或者箭头
  它们只不过是被移动而已
  如果它把数字 3 变成 4
  3 只不过是变成了 4 的 3
  就像把粉笔掰断了之后得到的是掰断了的之前的那个粉笔

  虽然 Assignment statement 让我们觉得
  那里好像有一个物体的存在被声明了
  但是当我们越深入细节
  这一点就可能看起来越不真实

  object 是如此
  function 是如此
  relation 和 type 也是如此

# 5B Computational Objects

以数字电路为例子
来在 scheme 中实现 OO

inverter (not-gate)
and-gate
or-gate

可以把下面的西线想像成小球
然后那些门上的线连接到小球上

这样每个做出来的电路就是一个以某些小球为接口的东西

``` scheme
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)
```

一个语言中的复合物看起来要像基本物一样
以同样的方式使用和处理 等等
尽管复合物与基本物之本质不同

# 6A Streams-Part 1

引入 assignment 之后
一切变得复杂多了
很多概念都进入讨论了 比如 状态 时间 和 id

It's a technically harder way of looking at things
because we have to think more mechanistically
about our programming language
We can't just think about it as mathematics
It's philosophically harder, because suddenly
there are all these funny issues
about what does it mean that something changes
or that two things are the same
And also, it's programming harder,
because as Gerry showed last time
there are all these bugs
having to do with bad sequencing and aliasing
that just don't exist in a language where
we don't worry about objects

但是
之所以要引入这些概念是因为
We wanted to build systems that
fall apart into chunks that seem natural

又但是
See, maybe the real reason that
we pay such a price to write programs
that mirror our view of reality is that
we have the wrong view of reality
See, maybe time is just an illusion,
and nothing ever changes

又但是
我们毕竟得到了一种来把模块分得更细的能力
只要不随意的把这种能力用到没有必要的地方就行了

here comes stream processing: (as conventional interfaces)
another way to decompose systems that's
more like the signal processing engineer's view of the world
than it is like thinking about objects
that communicate sending messages

- 当你有兴趣学的东西
  和老师有兴趣讲的东西完全一致时
  奇迹就发生了

# [todo] 6B Streams-Part 2

# [todo] 7A

把程序视为机器
将要展示的是 universal machine
(考虑图灵 和 他的 通用图灵机)

# 9A 一个可以作为编译器的中间语言的低级语言

1. 寄存器机的特点就是
   函数的输入值与输出 都明依赖于以显地方式声明寄存器而完成

2. 与forth这种栈机器相比
   可以说sicp寄存器机是针对对寄存器的操作来优化自己的语法的
   而forth是针对对栈的操作来优化自己的语法的

3. 另外
   不同语言对函数语义的实现方式不一样
   也就是对函数的参数传递的实现方式不一样
   而在scheme这种更高级的语言中 根本就感觉不到对函数调用的约定
   调用一个函数的时候 就是需要在被调用位置用到函数的返回值的时候
   所以对参数传递方式的约定被隐藏了
   而在一个函数返回的值可以被留在栈里之后在用
   而不是需要被立即使用
   在scheme中是通过局部变量来实现这种效果的

4. 关于smalltalk中的协议和信息传递:
   在寄存器机里也有对函数参数的约定等等
   但是有什么区别呢???
   wordy-lisp如何呢???

5. 这节反复说明 机器很笨
   + 类似于图灵的计算员隐喻 但是略有区别
   但是正是机器的这种笨的
   但是能够被重复并且被通过积累而增加性能的设计
   使得现代电子计算机这种机器非常成功

   ``` scheme
   (define gcd
     (lambda (a b)
       (if (zero? b)
         a
         (gcd b (remainder a b)))))

   (define remainder
     (lambda (n d)
       (if (< n d)
         n
         (remainder (- n d) d))))

   ;; (gcd 3 6)
   ;; (gcd 3 7)
   ```

6. 极简主义的金玉良言:
   one of the important things for designing a computer,
   which i think most designers don't do,
   is you study the problem you want to solve
   and then use what you learn from studying the problem you want to solve
   to put in the mechanisms needed to solve it in the computer you're building,
   no more no less.

   - [2026-01-24] 在这个建议的指导下，
     研究想要设计的 lisp -- x-lisp 的特性，
     然后专门设计中间语言与 vm 来实现这些特性。

7. Now it may be that the problem you're trying to solve is everybody's problem,
   in which case you have to build in a universal interpreter of some language.
   But you shouldn't put any more in
   than required to build the universal interpreter of some language.

8. 也就是说，
   如果你对你所想要解决的问题有充分而深入的研究，
   并且透彻理解了那个问题，
   那么，在实现一个解决那个问题的方案时，
   给出一个极简主义的设计就是水到渠成的了。

每个函数就像一个机器，大机器里可能有小机器
而这一节的语言是一种机器描述语言
每个机器由两部分组成:

1. 电路(data path)
   一个data path对应于汇编语言中的一个指令
   + 但是显然这是两种计算模型之间的类比
     这里的每个小机器都是特殊的计算机
     而 比如说 x86的机器是一个通用的计算机
     汇编命令是这个计算机用来模拟特殊的小计算机的方式
   + 注意，通用计算机所模拟的每个小机器都可以直接作为硬件被造出来

2. 控制器(controller)
   控制器对应于流程图
   它把小机器以某种方式链接起来变成大机器
   一些汇编指令的按顺序排列就是controller
   按顺序排列之外也可利用mark language形成流程图中的圈
   而时间可以看成是在流程图中运动的一个点

参数在两个机器是之间的传递，
在于它们都读写某个共同的存储空间：寄存器，或者栈。

机器被理解为这样的东西(一个有向图)：

1. 寄存器
   一种可以存放值的节点

2. 计算元件
   一个原子计算元件 或者是 一个被抽象起来的同类机器(归纳定义产生于这里)
   一种节点
   有一些入边链接到某些寄存器，可以从这些寄存器里fetch(并不删除旧的值)出值来
   有一些出边链接到某些寄存器，可以把计算的结果保存到这些寄存器中
   就像一些电流被过滤成了另一些电流
   这个节点上有一个开关来控制计算的进行

3. 单向信息流导线(可以被理解为 特殊的计算元件)
   一种特殊的有向边
   两边都连到寄存器
   导线上有开关
   当按下开关时会把一个寄存器中的值复制到另一个中

4. 指示灯
   一种节点
   与某个寄存器相连
   指示灯可以作为谓词对这个寄存器中的值形成一个判断
   也就是对寄存器中的值我们能够形成我们所能想像到的任何谓词
   控制器可以读指示灯

5. 控制器
   来控制按那些开关的先后顺序

machine == data path + controller

``` scheme
(define-machine gcd
  (register <a> <b> <t>)
  (controller ;; 就像汇编语言 或者流程图
   ;; 程序运行过程中的某一时刻 可以看成是流程图中的一个点
   ;; 而流程图中的一些操作可以看成是与机器中的开关的按钮相对应
   MAIN (assign <a> (read))
        (assign <b> (read))
   LOOP (branch (zero? (fetch <b>)) DONE)
        (assign <t> (remainder (fetch <a>) (fetch <b>)))
        ;; fetch指出了那些寄存器节点链接到remainder的入边
        (assign <a> (fetch <b>))
        (assign <b> (fetch <t>))
        (goto LOOP)
   DONE (print (fetch <a>))
        (goto MAIN)))

;; 在上面assign与fetch就代表了带有开关的有向边
;; + 这里计算元件也被分解了
;;   因为其实不需要那么多的开关 所以可以更精简一点


;; 参数在两个机器是之间的传递在于它们都读写某个共同的存储空间:寄存器(或者栈)
;; 注意这里机器被理解为函数的方式
;; 注意约定参数传递的方式

(define-machine gcd
  (register <a> <b> <t>)
  (controller
   ;; 1. 是controller在给出按钮 并进行控制
   ;;    一个mod可以被controller分配多个按钮而运用多次
   ;;    controller描述了机器如何被搭建 同时也描述了机器如何被控制
   ;; 2. 谓词是返回bool值的机器 它返回的值能够被branch处理
   ;;    branch专门就是用来处理bool值的装置
   MAIN (<a> <-- (read))
        (<b> <-- (read))
   LOOP (branch <-- zero? <-- <b>
                DONE)
        (:remainder <t> <-- mod <-- :dividend <a> :divisor <b>)
        ;; fetch指出了那些寄存器节点链接到remainder的入边
        (<a> <-- <b>)
        (<b> <-- <t>)
        (goto LOOP)
   DONE (print <-- <a>)
        (goto MAIN)))
```

上面是iterative(尾递归的)的函数所对应的机器
下面就是看递归函数对应与什么样的机器
在这里就需要用栈来模拟无穷多个小机器的嵌套了
语义上 栈中保存的是外面的大机器的状态
当里面的小机器工作完了之后
利用栈中所保存的信息可以恢复大机器额工作
``` scheme
(define factorial
  (lambda (n)
    (if (= n 1)
      n
      (* n (factorial (- n 1))))))
```
这不是尾递归的函数了
因为为了计算返回值我们不只需要调用factorial本身
还需要把这个调用的返回值拿来和n乘
以得到最后的返回值
即 对*的调用需要等待对factorial的调用的返回值
而在尾递归的情况下不用等待

这是就需要无穷的嵌套了
但是无穷的嵌套在物理的对机器的实现中并不存在
我们把这个问题的有穷部分和无穷部分分开来解决
有穷部分就跟之前一样
而无穷部分用栈这个非常简单的数据结构来解决
栈并不是无穷的 只是非常大而已

这时候机器作为一个有向图的样子也变了
但是为了以更简洁的方式理解这个图
我不去考虑栈的实现方式
而像在joy中一样 把操作栈的primitives理解成以栈为参数的一元函数
``` scheme
(define-machine factorial
  (register <return> <arg> <continue>)
  (controller
        (assign <continue> DONE)
   LOOP (branch (= 1 (fetch <arg>)) BASE)
        (save <continue>)
        ;; 下面把<continue>指定为factorial的递归调用返回后所必须经过的处理
        (assign <continue> AFTER)
        (save <arg>)
        (assign <arg> (sub1 (fetch <arg>)))
        (goto LOOP)
   BASE (assign <return> (fetch <arg>))
        (goto (fetch <continue>))
  AFTER (restore <arg>)
        (assign <return> (* (fetch <arg>) (fetch <return>)))
        (restore <continue>)
        (goto (fetch <continue>))
   DONE
        ))

;; 大写的word是地址的值

(define-machine factorial
  (register <arg> <result> <next>)
  (stack <<ReturnStack>>)
  (controller
        (<next> <-- DONE)
   LOOP ;; 这段计算是为了把递归的扇子展开
        (branch <-- :bool one? <-- <arg>
                    :address BASE)
        (<<ReturnStack>> <-- <next>)
        (<next> <-- AFTER)
        (<<ReturnStack>> <-- <arg>)
        (<arg> <-- sub1 <-- <arg>)
        (goto <-- :address LOOP)
   ;; 下面两段计算是为了把展开的递归的扇子合起来
   BASE
        (<result> <-- <arg>)
        (goto <-- :address <next>)
   AFTER
        (<<ReturnStack>> --> <arg>)
        (<result> <-- * <-- <arg> <result>)
        (<<ReturnStack>> --> <next>)
        (goto <-- :address <next>)
   DONE
        ))
```
足够大的栈给你一个幻觉
认为递归过程可以是无穷的

在练习一个例子 以熟悉栈的用法
戒律:
1. 不要在栈里保存以后用不到的值
2. 之所以有一个有用的值需要被保存
   是因为保存这个值的寄存器马上就有别的用处
3. 取出来一个值就赶快用这个值
4. 覆盖一个寄存器的时候一定要确定里面的值已经不需要了
5. ><>< 是不是可以借鉴CPS???
``` scheme
(define fib
  (lambda (n)
    (if (<= n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2))))))
(fib 20)
==> ...

(define fib
  (lambda (n p)
    (if (zero? n)
      (car p)
      (fib (sub1 n)
           (cons (cdr p)
                 (+ (car p)
                    (cdr p)))))))
(fib 20 (cons 1 1))
==> ...


(define-machine fib
  (register <result> <arg> <continue>)
  (controller
        (assign <continue> DONE)
   LOOP (branch (< 2 (fetch <arg>)) BASE)
    #0= (save <continue>)
        (assign <continue> AFTER-fib:n-1)
    #1= (save <arg>)
        (assign <arg> (- (fetch <arg>) 1))
        (goto LOOP)
   BASE (assign <result> (fetch <arg>))
        (goto (fetch <continue>))
AFTER-fib:n-1
    #1# (restore <arg>)
        (assign <arg> (- (fetch <arg>) 2))
        ;; (restore <continue>)
        ;; (save <continue>)
        ;; peephole optimization:
        ;; 当对一个寄存器的restore save assign三连，而中间无其他操作时
        ;; 就可以作这样的优化
        (assign <continue> AFTER-fib:n-2)
    #2= (save <result>)
        (goto LOOP)
AFTER-fib:n-2;; 有几个递归调用就有几个AFTER
        (assign <arg> (fetch <result>));; fib:n-2
    #2# (restore <result>)
        (assign <result> (+ (fetch <result>) (fetch <arg>)));; 只有在最后一次递归调用的之后才能算出一个返回值
    #0# (restore <continue>)
        (goto (fetch <continue>))
   DONE
        ))

(define-machine fib
  (register <arg> <result> <next>)
  (stack <<ReturnStack>>)
  (controller
        (<next> <-- DONE)
   LOOP
        (branch <-- :bool < <-- :a 2 :b <arg>
                    :address BASE)
        (<<ReturnStack>> <-- <next>)
        (<next> <-- AFTER-fib:n-1)
        (<<ReturnStack>> <-- <arg>)
        (<arg> <-- sub1 <-- <arg>)
        (goto <-- :address LOOP)
   BASE
        (<result> <-- <arg>)
        (goto <-- :address <next>)
   AFTER-fib:n-1
        (<<ReturnStack>> --> <arg>)
        (<arg> <-- sub2 <-- <arg>)
        (<next> <-- AFTER-fib:n-2)
        (<<ReturnStack>> <-- <result>)
        (goto <-- :address LOOP)
   AFTER-fib:n-2
        (<arg> <-- <result>)
        (<<ReturnStack>> --> <result>)
        (<result> <-- + <-- <result> <arg>)
        (<<ReturnStack>> --> <next>)
        (goto <-- :address <next>)
   DONE
        ))
```

# 9B 用低级语言实现的解释器

在这节中可以发现
当仔细分析用低级语言实现的解释器时指令的顺序
那么就自然而然得到尾递归优化
并不是什么神奇的预处理机制在作尾递归优化

So we built all of these languages, they're all based on LISP.
A lot of people ask what particular problems is LISP good for solving for?
The answer is LISP is not good for solving any particular problems.
What LISP is good for is constructing within it
the right language to solve the problems you want to solve,
and that's how you should think about it.

我想强调 scheme 的上面的这个性质
并且改进它 以使它更适合完成这类任务

对于初学者来说，用元 lisp 解释器写一个 lisp 解释器会带来惊奇
而写个 lisp 到某个机器的汇编的编译器就能消除这种惊奇
一种愉快的理解被代替为另一种愉快的理解

这里是在用上节课所介绍的低级语言来写 lisp 的解释器

注意，展开者把值(保存后面的计算的指令的地址)入栈
合起来者把值(保存后面的计算的指令的地址)出栈

尾递归优化其实不是针对尾递归的
而是针对所有尾部调用的
``` scheme
(define-machine eval
  (register
   ;; contract that eval-dispatch fulfills
   <sexp>        ;; eval的第一个参数
   <env>         ;; eval的第二个参数
   <continue>    ;; 保存下一步将要去的地址
   <return>      ;; eval的返回值
   ;; 当返回值时其他的寄存器中的值就可以都不要了

   ;; contract that apply-dispatch fulfills
   <fun>         ;; apply的第一个参数
   <arg-lis>     ;; apply的第二个参数
   ;; 要求栈的顶端保存着下一步要去的地址
   ;; apply的返回值也保存在<return>寄存器中
   ;; 之后pop stack
   ;; 之后其他的寄存器中的值就可以都不要了

   <temp>
   )
  (controller

        ))
```

# 10A 编译器优化

解释器是一个可以计算某个语言的所有的表达式的机器
而编译器是一个把一个语言的表达式转化到另一个语言的机器
当目标语言是汇编时 编译器就像是制造机器的机器

+ 只要统一用define定义的函数的参数所用的寄存器
  编译器和解释器所定义的函数就能相互调用
  这就需要把解释器中的(至少是)define用编译器的目标语言来实现
+ 非全局优化的渐进编译器也能解决相互调用的问题
  因为此时解释器只不过是一个编译器的包装

关于编译器的优化:
最极端的生成低效率的代码的方式是
先写一个单纯地把一个(用低级语言实现的)解释器的解释过程存储起来的编译器
然后在用分析函数来过滤这个生成的目标代码中没必要出现的部分

关于中间语言:
1. 应该以这样的方式来实现中间语言
   使得中间语言的每一个指令必须都相互独立
2. 使用scheme中的中间语言就可以把对目标代码的处理维持在scheme中
   而尽量晚生成真正的会编码或机器码

函数的复合体现在汇编级的低级语言中
就是把一段一段相互独立的指令接起来
但是在把指令段接起来的同时要利用栈来保护某些寄存器中的值
即 如果后面的代码段需要某个寄存器 前面的代码段更改了这个寄存器
那么就需要用一对进栈与出栈来为这次连接保存这个寄存器中的值
所以对于编译器来说代码段作为数据结构的组成部分是:
1. 代码段本身
2. 代码段需要的寄存器(一个小机器读取的寄存器)
   代码段修改的寄存器(一个小机器写入的寄存器)
3. 注意连接两个代码段而形成一个大的代码段时
   数据结构中的这些值的变化

# 10B pair的实现 与 垃圾回收 与 尾声

首先pair的实现是非本质的问题
比如低效地
我们可以用哥德尔配数法来编码pair
这将是极端低效的
低效到这种实现只是在理论分析中有用

直观的从几何上看pair是非常简单的
但是并没有电子设备能直接实现这种几何直观
我们能利用的电子设备只是线性的内存而已

所以用来实现pair的机制是内存中的一个数组
每一项包含car与cdr两部分
+ 实际上这个数组的每一项保存更丰富的信息
  比如垃圾回收机制就用到了每一项中的mark信息
+ 如果让数组的每一项都保存自己这个位置的地址(或者数组的索引)
  尽管效率很低
  但是这样我就能实现一种更好的pair了
  即 从每一个pair我能显式地得到它的地址
  从而自由并且安全并且方便地在别的地方引用这个pair
+ 这样就也阻止了用户去直接处理地址
  同时又提供给我方便的引用机制
+ 但是这就给垃圾回收带来了困难
  因为比如说如果被这样明显引用的pair不允许被回收的话
  那么就需要free的帮助来明显的回收它们

这样的实现方式就需要分配内存空间
笨办法是用一个表格来记录哪些空间是自由的
另一个办法是使用一个free-list
预先初始化所有pair数组使得:
1. 有一个指针指向第一个自由的pair项
2. 每一个自由的pair项的cdr位置保存这另一个自由的pair项的地址
   + 发现 每个自由的pair在被声明使用并被覆盖之前
     它的car和cdr位置可以用来保存其他信息
     利用这一点尝试实现内存分配机制
     >< 但是带有loop的list是个问题
     当我失去对p = (1 2 b)的引用的时候
     我可能还需要对p中的其他部分的引用
     free-list:
     '(() () () () () () () ())
     或者
     '(1 2 3 8 4 2 3 4)
     因为free-list的中每一个cons的car并不重要 是cdr让它们链接起来的

     注意列表中的元素必须有类型
     因为否则列表中保存一个地址的时候 我就没法区分它是地址还是数字了
     也就是说如果想要实现类lisp的list这种数据结构 我就必须要设计类型系统
     而这只是简单的给不同的数据类型设计编码而已 而不是写一个类型推导器

     内存的分配:
     (随着构造子的出现而自动分配)
     然后每遇到一个构造子cons的时候
     free-list的第一个cons就会被拿来使用
     而free-list向后移动
     '(1 2 3 8 4 2 3 4) ==> '(2 3 8 4 2 3 4)
     出现cons的地方就是需要分配新的内存的地方

     内存的回收:
     需要计算有向图的(有向)联通性
     而且是先计算有用的 然后就知道没用的
     利用一个mark实现这一切
     但是如果我不先完全地计算好哪些是有用的
     我就没法知道那些是没用的
     marking & sweeping
     如果marking作为递归函数是利用栈来实现的
     那么当有很多的cons被用到的时候 就很可能让栈溢出
     >< 我知道数据结构上的丰富性可以式新的性状和更快的算法成为可能
     如何丰富list的数据结构才能实现一个更好的gc呢???

     >< 为什么我告诉自己我不能用那个swap算法来实现gc??
     因为我想给list实现更丰富的性状
     但是这真的形成冲突吗???
     我想实现的新性状是
     1. 当(cons sexp-a sexp-b)被求值的时候
        sexp-a和sexp-b中要能够引用这个cons的地址本身
        但是 如果cons嵌套了怎么办??
        嵌套也是可以解决的只要用对地址的明显的命名来使用它
        比如可能的语法是:
        ``` scheme
        (cons {kkk}
               sexp-a sexp-b)

        (cons :address kkk
              :car sexp-a
              :cdr sexp-b)
        ```
        然后在sexp-a中对kkk的引用就是对这个cons的地址的引用
     2. 我需要能够以明显的方式处理每个cons的地址
        上面的这种机制就足够了吗???
     3. 我可不可以原生地直接实现对wordy-list的支持呢???
        可能不行 因为没有基本的列表数据结构 我就没法用列表来实现字符串
        而字符串是需要被作为wordy-list中的那些symbol的
     4. 如果我用sicp中的方式来实现gc与列表结构
        那么字符串怎么办呢???
        一个字符串将有8 bytes而不是1 byte
        这甚至都足够用来编码字符的颜色和字体了
        >< 但是如果每个字符都需要用64 bit来编码
        用户空间能承受的了吗???

关于垃圾回收:
1. 原理是每一个计算机的"意识"就是它的寄存器中的值而已
   + 或者说只有几个固定的变量是一个机器能够意识到的
     比如 让机器意识到用户内存空间的
     可以是一个指向用户空间中的某个位置的指针
   + 而对于我的forth系统来说
     字典中保存的东西决定了那些内存空间是在机器的意识之内
     而那些内存空间是在机器的意识之外的
   计算机访问内存的方式是在寄存器中保存内存中pair数据结构(或其他数据结构)的地址
   然后pair数据结构之间的指针决定着那些内存是可以访问到的
   其他的内存就是自由的
2. 在pair数组的项中添加mark信息就能用一个遍历二叉树的算法来考察使用情况
3. 标记好了信息之后
   就可以在再跑一遍整个pair数组(很费时间)
   然后把自由的项联系起来以形成一个free-list
4. 另一个算法是把pair数组分成两部分
   在需要的时候利用swap把一半弄到硬盘中然后压缩然后再传回来

尾声 关于不可计算性
