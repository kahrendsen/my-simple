/**
 * __       __   ______    ______  ________   ______   ________
 * |  \     /  \ /      \  /      \|        \ /      \ |        \
 * | $$\   /  $$|  $$$$$$\|  $$$$$$\\$$$$$$$$|  $$$$$$\ \$$$$$$$$
 * | $$$\ /  $$$| $$  | $$| $$  | $$  | $$   | $$__| $$    /  $$
 * | $$$$\  $$$$| $$  | $$| $$  | $$  | $$   | $$    $$   /  $$
 * | $$\$$ $$ $$| $$  | $$| $$  | $$  | $$   | $$$$$$$$  /  $$
 * | $$ \$$$| $$| $$__/ $$| $$__/ $$  | $$   | $$  | $$ /  $$___
 * | $$  \$ | $$ \$$    $$ \$$    $$  | $$   | $$  | $$|  $$    \ :)
 * \$$      \$$  \$$$$$$   \$$$$$$    \$$    \$$   \$$ \$$$$$$$$
 *
 */
import scala.collection.mutable.{ Stack, HashMap }

//This preserves type information
object Type {
  val UNKNOWN = -1
  val STRING = 0
  val FLOAT = 1
  val INT = 2
  val BOOL = 3
}

object Binding {

  var map = new HashMap[Symbol, (Int, Any)]()
  def putValue(sym: Symbol, v: (Int, Any)) {
    map.put(sym, v)
  }
  
  def getValue(sym: Symbol): Any = {
    if(map.contains(sym)){
      return map.get(sym).get._2
    }else{
      return None
    }
  }
  def getType(sym: Symbol): Int = {
    if(map.contains(sym)){
      return map.get(sym).get._1
    }else{
      return Type.UNKNOWN
    }
  }
}

class StringAssignment(sym: Symbol) {
  // Bind this and add to variable list
  def :=(value: String) = {
    println("Assigning string:")
    println("\t" + sym + " = " + value)
    Binding.putValue(sym, (Type.STRING, value))
  }
}
class IntAssignment(sym: Symbol) {
  def :=(value: Int) = {
    println("Assigning int:")
    println("\t" + sym + " = " + value)
    (sym, value)
    Binding.putValue(sym, (Type.INT, value))
  }
  
  def :=(valSym: Symbol) = {
    val value = Binding.getValue(valSym)
    value match{
      case a: Int => {
        println("Assigning int:")
        println("\t" + sym + " = " + a)
        Binding.putValue(sym, (Type.INT, a))
      }
      case b: Double => {
        println("Assigning int:")
        val valueAsInt = (b).toInt
        println("\t" + sym + " = " + valueAsInt)
        Binding.putValue(sym, (Type.INT, valueAsInt))
      }
      case _ => {
        println("Error:")
      }
    }
  }

  def f(i: Int) = i

  case class MathFunction(lhs: Function0[Int]) {
    def *(rhs: Int): Function0[Int] = (() => lhs() * rhs)
    def *(rhs: Function0[Int]): Function0[Int] = (() => lhs() * rhs())
    def /(rhs: Int): Function0[Int] = (() => lhs() / rhs)
    def /(rhs: Function0[Int]): Function0[Int] = (() => lhs() / rhs())
    //      def +(rhs:Symbol):Function0[Int] = (() => {
    //        val n: Int = Binding.map(rhs)._2//getThingFromMap(rhs)
    //        lhs() + n
    //      })
    def +(rhs: Function0[Int]): Function0[Int] = (() => lhs() + rhs())
    //def -(rhs: Symbol): Function0[Int] = (() => lhs() - binds.num(rhs))
    def -(rhs: Function0[Int]): Function0[Int] = (() => lhs() - rhs())
  }
}
class FloatAssignment(sym: Symbol) {
  def :=(value: Double) =
    {
      println("Assigning float:")
      println("\t" + sym + " = " + value)
      (sym, value)
      Binding.putValue(sym, (Type.FLOAT, value))
    }
}
class BoolAssignment(sym: Symbol) {
  def :=(value: Boolean) =
    {
      println("Assigning bool:")
      println("\t" + sym + " = " + value)
      (sym, value)
      Binding.putValue(sym, (Type.BOOL, value))
      
    }
}

class Assignment(sym: Symbol) { // This handles expressions THIS WON'T WORKKKK D:
  def :=(value: Int) = {
    val varType = Binding.getType(sym)
    if(varType == Type.UNKNOWN){
    	println("ERR : Variable not instantiated " + sym)
    }else if(varType != Type.INT){
    	println("ERR: Wrong type " + sym)
    }else{
	    println("Assigning int:")
	    println("\t" + sym + " = " + value)
	    Binding.putValue(sym, (Type.INT, value))
    }
  }
  
  def :=(f: Function0[Any]) = {
      val value = f()
      value match{
        case a: Int => {
          println("Assigning function0(int):")
          println("\t" + sym + " = " + a)
          Binding.putValue(sym, (Type.INT, a))
        };
        case b: String => {
          println("Assigning function0(String):")
          println("\t" + sym + " = " + b)
          Binding.putValue(sym, (Type.STRING, b))
        };
        case c: Boolean => {
          println("Assigning function0(Boolean):")
          println("\t" + sym + " = " + c)
          Binding.putValue(sym, (Type.BOOL, c))
        };
        case d: Double => {
          println("Assigning function0(Float):")
          println("\t" + sym + " = " + d)
          Binding.putValue(sym, (Type.FLOAT, d))
        };
        case _ => {
          println("ERR:")
        }
      }
    }
}

class mysimpleDCG {

  implicit def symbolToAssignment(name: Symbol): Assignment = new Assignment(name)

}

object declare {
  def int(assign: Symbol): IntAssignment = new IntAssignment(assign)
  def bool(assign: Symbol): BoolAssignment = new BoolAssignment(assign)
  def float(assign: Symbol): FloatAssignment = new FloatAssignment(assign)
  def string(assign: Symbol): StringAssignment = new StringAssignment(assign)
}
