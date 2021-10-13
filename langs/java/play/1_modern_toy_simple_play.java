
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
