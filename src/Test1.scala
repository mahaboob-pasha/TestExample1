
class Test1(var a:Int,var b:Int, val ll:List[Int]) {
  println("Test1 instantiated")
  
  def this(a:Int) = this(a,a,List(a))
  
  override def toString:String = {
    "("+a+","+b+")"
  }
 
  def apply(inc:Int) : Test1 = {
   println("Test1::apply, ideally this is static method in companion object\n")
    a += inc
println("Test1::apply")
    b += inc
    
    this
  }
  
  def apply(a:Int, b:Int, l:List[Int]):Test1 = Test1(this.a+a,this.b+b, l)
}


object Test1 {
  
  def apply(a:Int,b:Int, ll:List[Int] = List.empty): Test1 = {
    println("static Test1::apply")
   new Test1(a,b,ll) 
  }
  
  def unapply(t:Test1) : Option[(Int,Int, List[Int])] = {
    println("static Test1::unapply")
    Some((t.a,t.b, t.ll))
  }
  
  
  def myFoldLeft[A,B](l:List[A], acc:B, fn:(B,A)=>B):B = l match {
    case head::tail => myFoldLeft(tail, fn(acc,head), fn)
    case _ => acc
  }
  
  
  
  
  def main(args: Array[String]) : Unit = {
   val ll1 = List(1,2,3,4)
   println(myFoldLeft(ll1,0,{(acc:Int,e:Int) => if (e%2==0) acc+e else acc}))
   
   val t1 = Test1(1,2,List(1)) //static apply method
   val t2 = new Test1(100)
   val t3 = t2(1,1,List())
   println(t2)
   println(t3)
   t1(-2)
   t1 match {
     case Test1(0,0,List()) => println("All EMPTY")
     case Test1(_,0,List()) => println("b is 0 and list is empty")
     case Test1(0,_,List()) => println("a is 0 and list is empty")
     case Test1(0,0,List(_,_*)) => println("a & b are 0 and list is not empty")
     case Test1(_,0,List(_,_*)) => println("b is 0 and list is not empty")
     case Test1(0,_,List(_,_*)) => println("a is 0 and list is not empty")
     case Test1(a,b,List()) => println("a & b not empty, but list is")
     case Test1(a,b,List(_)) => println("a & b not empty,list size is 1")
     case Test1(a,b,List(v1,v2)) => println("a & b not empty,list size is 2")
     case Test1(a,b,List(_*)) => println("a & b not empty,list size is n")
     case _ =>  println("I have no idea what is this")
   }
   
  } 
 
}
