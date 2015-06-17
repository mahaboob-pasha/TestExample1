

object varianceEx {
  
  class Dog(val name:String) {
    override def toString:String = this.getClass.getName+"-"+name
  }
  
  class Puppy(override val name:String) extends Dog(name) {
    override def toString:String = this.getClass.getName+"-"+name
  }
  
  class TigerPuppy(override val name:String) extends Puppy(name) {
    override def toString:String = this.getClass.getName+"-"+name
  }
  
  def getDogPECS[A <: Dog](box:Box[A]) : A = box.get
  
  def putPuppyPECS[A >: Puppy](box:Box[A], d:A):Unit = {
    box.put(d)
  }
  
  def putDogPECS[A](box:Box[A], d:A):Unit = {
    box.put(d)
  }
  
  class Box[A](var dog:A) {
    def get:A = dog
    def put(d:A):Unit = {dog = d}
    
    def show:Unit = println(get)
  } 
  
  
  def testPECS() : Unit = {
    val dogRocky = new Dog("Rocky")
    val dogTiger = new Dog("Tiger")
    
    val pupRover = new Puppy("Rover")
    val pupBalboa = new Puppy("Balboa")
    val tPupT1 = new TigerPuppy("tPupT1")
    
    val dogBox = new Box[Dog](dogRocky)
    val pupBox = new Box[Puppy](pupRover)
    
    dogBox.put(dogRocky)
    pupBox.put(pupRover)
    
    //dogBox.show
    //pupBox.show
   
    println(getDogPECS(dogBox))
    putDogPECS(dogBox,pupBalboa)
    println(getDogPECS(dogBox))
    
    //How come I'm able to put tigerPuppy inside Dog Box
    putDogPECS(dogBox,tPupT1)
    println(getDogPECS(dogBox))
    
    //putPuppy(pupBox, dogRocky) //Error which is fine here
    
    putPuppyPECS(pupBox, pupRover)
    
    putPuppyPECS(pupBox, tPupT1)
  }
  
  
  trait PutBox[-A] { 
    def put(v:A): Unit = ???
  }
  
  trait GetBox[+A] {
    def get:A = ???
  }
  
  trait GetPutBox[A] extends PutBox[A] with GetBox[A]
  
  def getDogVariance(box: GetBox[Dog]): Dog = box.get
  
  def putPuppyVariance[A <: Puppy](box:PutBox[A], d:A):Unit = box.put(d)
   
  //def getDog(box:GetBox[Dog]) : Dog = box.get
  //def putPuppy(box:PutBox[Puppy], p: Puppy) : Unit = box.put(p)
  
  def testVariance() = {
    val dogRocky = new Dog("Rocky")
    val dogTiger = new Dog("Tiger")
    
    val pupRover = new Puppy("Rover")
    val pupBalboa = new Puppy("Balboa")
    val tPupT1 = new TigerPuppy("tPupT1")
    
    
    val dogPutBox = new PutBox[Dog]{}
    val dogGetBox = new GetBox[Dog]{}
    val pupGetBox = new GetBox[Puppy]{}
    val pupPutBox = new PutBox[Puppy]{}
    val pupGetPutBox = new GetPutBox[Dog]{}
    
    putPuppyVariance(dogPutBox, pupRover) //put puppy into dog get box
    getDogVariance(dogGetBox)
    
    //putPuppyVariance(dogPutBox, dogTiger) //error, this will work if I use A:< Dog
    
    putPuppyVariance(pupGetPutBox, tPupT1)
    putPuppyVariance(dogPutBox, tPupT1) //works well coz using, A<: Puppy
    
    
    getDogVariance(dogGetBox)
    //getDogVariance(dogPutBox) //error
    getDogVariance(pupGetPutBox)
    getDogVariance(pupGetBox)
    
  }
  
  def main(args: Array[String]) : Unit = {
    
    //testPECS
    
    //testVariance
    
    //val tup1:(Int,Int) = (12,34)
    //println(tup1._1)
    
    
    curryingTest
  }
  
  //The also work on mechanism of closure
  def curryingTest = {
    val add = {(a:Int, b:Int) => a+b}
    println(add(1,2))
    var global = 0
    
    val add2 = {a:Int => {global +=a;b:Int => global +=b;global}}
    println(add2(1)(4))    
    
    val add2PartialiallyApplied = add2(15)
    println(add2PartialiallyApplied(1))
    println(add2PartialiallyApplied(10))
    
    
    val add2PartialiallyAppliedList = List(1,2,3) map add2 //list of partially appiled func {b:Int => ALREADY_APPLIED_LIST_ITEM_VALUE + b}
    //println(add2PartialiallyAppliedList)

    println("foreach on each func on 5")
    add2PartialiallyAppliedList map { f => println(f(5))}
    println(add2PartialiallyAppliedList(0)(10))
    println("foreach on each func on 1")
    add2PartialiallyAppliedList.foreach(p => println(p(1)))
    
    //trying to use add2 on add3_0 func, but not working \
    
    val add3_0 = {a:Int => {global +=a;add2}}
    global = 0
    var tmp = add3_0(1)(2)(3)
    println("Tmp1="+tmp)
    global = 0
    tmp = add3_0(100)(2)(3)
    println("Tmp2="+tmp)
    
    val add4_0 = {a:Int=>{global +=a;add3_0}}

    global = 0
    tmp = add4_0(1)(2)(3)(4)
    println("Tmp3="+tmp)
    
    val add5_0 = {a:Int=>{global +=a;add4_0}}
    
    global =0;
    val fadd4_0_1 = add5_0(1)
    
    global =0;
    val fadd4_0_2 = add5_0(100)
    
    tmp = fadd4_0_1(1)(1)(1)(1)
    println("Tmp4="+tmp)
    global = -10
    tmp = fadd4_0_2(1)(1)(1)(1)
    println("Tmp5="+tmp)
    
    
    val add3 = {a:Int => {b:Int => {c:Int=>a+b+c}}}
    println(add3(1)(4)(10))  
    
    val add4_2 = {a:Int => {b:Int => {val ab=a+b; {c:Int=>val abc=ab+c;{d:Int=>abc+d}}}}}
    println(add4_2(100)(20)(3)(1))
    
  }
  
}