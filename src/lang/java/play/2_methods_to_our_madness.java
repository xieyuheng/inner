
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
