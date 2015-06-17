import scala.xml._
import scala.collection.mutable.HashMap

object PrintOptions {
  
  
  trait Getter {
    def getV[K,V](key:K)  = {
     var map = this.asInstanceOf[ scala.collection.mutable.Map[K,V]]
     val v = map.get(key) match {
       case Some(s) => s
       case None => null
     }
     v
   } 
    
  }
  
  def main(args: Array[String]): Unit = {

  // delayed(time())
   val a1=12;
   val a2=13;
  // addNumbers(b=a1,a=a2)
   
   println(applyInt(layout,"10.01"))
   
   //println(applyStr(str2Int, "asdf"))
   
   val sum = sum2N:(Int,Int) => Int
   println(apply2Pars(sum,1,2));
   
   
    val pattern = "Scala".r
    val str = "Scala is Scalable and cool"
    println(pattern findFirstIn str)
      
    val p1 = new Point(1,2)
    val p2 = new Point(1,2)
    val p3 = new Point(1,3)
    
    p1.display(p1.toString())
    p3.display(p1.toString())
    
    println(p1.isEqual(p2))
    println(p1.isEqual(p3))
    
    p3.display("as");
    
    println(matchTest(1))
    println(matchTest("two"))
    println(matchTest(10))
    println(matchTest("sdef"))
    println(matchTest(p1))
    println(matchTest("123"))
    
    var l1 = List(1,2,3);
    var l2 = l1.toList
    println(l1.hashCode())
    println(l2.hashCode())
    
    var l3 = 4 :: l1
    println(l3.hashCode())
    l3 ::= 0
    
    println(l3.hashCode())
    l3.foreach { println(_) }
    
    var str11 = "hello"
    println(str11)
    str11 = "hi"
    println(str11)
    
    xmlExample

    var colors  = new HashMap[String,String]() with Getter
    colors += ("red" -> "RED")
    colors += ("blue" -> "BLUE")
    
    
    println(colors.getV("red"))
    println(colors.getV("reds"))
  }
  
 
  
  def xmlExample = {
    val xml = <Person>
<Info>
<Name>
<FName id="123">MahaboobInfo</FName>
<LName>Pasha</LName>
</Name>
</Info>
<Name>
<FName>Mahaboob</FName>
<LName>Pasha</LName>
</Name>
<Age>33</Age>
<Gender>Male</Gender>
<Address>
<A1>a1</A1>
<A1>a2</A1>
<A1>a3</A1>
</Address>
</Person>
      
  val fname = xml \\ "Info"\\ "Name" \\ "FName"
  println(fname)
  println(fname \\ "@id");
  println(fname.text)
  
  val a1 = xml \\ "Address" \\ "A1"
  println(a1)
  val a1Itr = a1.iterator
  while(a1Itr.hasNext) {
    println(a1Itr.next)
  }
  }
  
  
  trait Equal {
    def isEqual[A](x:A): Boolean
    def isNotEqual[A](x:A): Boolean = !isEqual(x)
  }
  
  trait Display {
    def display(x:Any) : Unit = {
      println("Display::display:: "+x.toString())
    }
  }
  
   trait Display2 {
    def display2(x:Any) : Unit = {
      println("Display2::display")
    }
  }
   
   def matchTest(x:Any) :Any = x match {
     case 1 => "One"
     case "two" => 2
     case y:Int => "Scala.Int=\""+y.toString()+"\""
     case y:String => {
        try {
          return y.toInt
        } catch {
          case ex:NumberFormatException => {
            return "\""+y.toString()+"\""
          }
        }
     }
     case _ => "???"
   }
  
  //case to add default toString, hashCode and equals methods
  case class Point(xc:Int, yc:Int) extends Equal with Display with Display2 {
    var x:Int = xc
    var y:Int = yc
    
    def isEqual[A](x:A): Boolean  = {
      if (x == null) {
       return false
      }
      if (!x.isInstanceOf[Point]) {
        return false
      }
      val o2 = x.asInstanceOf[Point]
      if (o2.hashCode() == hashCode()) return true
      val b1 = o2.x == this.x
      val b2 = o2.y == this.y
      return b1 && b2
    }
    
  }
  
  def apply2Pars(f: (Int,Int) => Int, v1: Int, v2:Int) = f(v1,v2)
  
  def sum2(a:Int, b:Int): Int = {
    return a+b
  }
  def sum2N(a:Int, b:Int): Int = {
    return a+b+1
  }
  
  def applyStr(f: String => Int, v: String) = f(v)
  
  def str2Int[A](x: A):Int = {
    println(x.toString()+" length is ")
    x.toString().length()
  }
  
  def applyInt[A](f: A => String, v: A) = f(v)

  def layout[A](x: A) = "layout func [" + x.toString() + "]"
   
  def time() = {
      println("Getting time in nano seconds")
      System.nanoTime
   }
   def delayed( t: => Long ) = {
      println("In delayed method")
      println("Param1: " + t)
      println("Param2: " + t)
      
   }
 
   def addNumbers(a:Int, b:Int): Int = {
     println("a="+a)
     println("b="+b)
     return a+b
   }
}
