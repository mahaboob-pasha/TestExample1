import scala.collection.mutable

object Calculator {
  type Operator = (Int,Int) => Int
  
  object Operator {

    val operators:Map[String,Operator] =  Map("+" -> {_+_},
                                              "-" -> {_-_},
                                              "*" -> {_*_},
                                              "/" -> {_/_}
                                             )
    val tokens = operators map {_.swap}
    
    def unapply(token:String):Option[Operator] = operators.get(token)
  }
  
  object Number {
    def unapply(token:String) : Option[Int] = try {Some(token.toInt)}catch {case _:NumberFormatException=>None}
  }
  
  sealed trait Expression
  case class NumberExpression(value:Int) extends Expression
  case class OperationsExpression(lhs:Expression, rhs:Expression, op:Operator) extends Expression

  //def step(List[])
  
  def parse(expr:String) : Expression = {
    val stack = new mutable.Stack[Expression]()
    
    for(token <- expr.split(" ")) token match {
      case Number(num) => stack.push(NumberExpression(num))
      case Operator(op) => val rhs=stack.pop;val lhs=stack.pop;
                        stack.push(OperationsExpression(lhs, rhs, op));
      case _ => throw new IllegalArgumentException("expr is not proper")
    }
    
    stack.pop
  }
  
  def calculate(expr:Expression) : Int = expr match {
    case NumberExpression(num) => num
    case OperationsExpression(lhs, rhs, op) =>
      op(calculate(lhs), calculate(rhs))
  }
  
  def toInfixExpr(expr:Expression) : String = expr match {
    case NumberExpression(num) => num.toString()
    case OperationsExpression(lhs, rhs, op) =>
      s"(${toInfixExpr(lhs)} ${Operator.tokens(op)} ${toInfixExpr(rhs)})"
  }
  
  def main(args:Array[String]) = {
    val exprStr = "10 2 3 + *"
    val expression = parse(exprStr)
    val infixStr = toInfixExpr(expression)
    
    val value = calculate(expression)
    
    println(infixStr+" = "+value)
    
  }
}