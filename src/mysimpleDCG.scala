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
object Type3 extends Enumeration {
  val STRING, FLOAT, INT, BOOL, UNKNOWN = Value
}

class hungry {
  def fish(k: Symbol): Any = {
    (Binding.map(k), numerics.get(k)) match {
      case (Some(x), None) => x
      case (None, Some(y)) => y
      case (None, None) => None
      case (Some(x), Some(y)) => Some(x, y)
    }
  }
}

object Binding {

  val map = HashMap[Symbol, (Type, Any)]()
  def putThingIntoMap(sym: Symbol, v: (Type, Any)) {
    map.put(sym, v)
  }
  def getThingFromMap(sym: Symbol) {
    val valueFromMap = map.get(sym)
  }

  def getThingFromMapInt(i: Int) {
    val valueFromMap = map.get(Symbol(i + ""))
  }

  //	val intMap = HashMap[Symbol,Int]()
  //	val boolMap = HashMap[Symbol,Boolean]()
  //	val stringMap = HashMap[Symbol,String]()
  //	val floatMap = HashMap[Symbol,Float]()
  //	
  //	def getValue(sym:Symbol) {
  //	  if(intMap.contains(sym)) {
  //	    return intMap(sym)
  //	  }
  //	  else if (floatMap.contains(sym)) {
  //	    value = floatMap(sym)
  //	  }
  //	  else {
  //	    println("cry")
  //	  }
  //	}
}

class StringAssignment(sym: Symbol) {
  // Bind this and add to variable list
  def :=(value: String) = {
    println("Assigning string:")
    println("\t" + sym + " = " + value)
    Binding.putThingIntoMap(sym, (Type.STRING, value))
  }
}
class IntAssignment(sym: Symbol) {
  def :=(value: Int) = {
    println("Assigning int:")
    println("\t" + sym + " = " + value)
    (sym, value)
    Binding.putThingIntoMap(sym, (Type.INT, value))
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
    def -(rhs: Symbol): Function0[Int] = (() => lhs() - binds.num(rhs))
    def -(rhs: Function0[Int]): Function0[Int] = (() => lhs() - rhs())
  }
}
class FloatAssignment(sym: Symbol) {
  def :=(value: Double) =
    {
      println("Assigning float:")
      println("\t" + sym + " = " + value)
      (sym, value)
      Binding.putThingIntoMap(sym, (Type.FLOAT, value))
    }
}
class BoolAssignment(sym: Symbol) {
  def :=(value: Boolean) =
    {
      println("Assigning bool:")
      println("\t" + sym + " = " + value)
      (sym, value)
      Binding.putThingIntoMap(sym, (Type.BOOL, value))
    }
}

class Assignment(sym: Symbol) { // This handles expressions THIS WON'T WORKKKK D:
  def :=(value: Function0[Int]) =
    {
      println("Assigning function0(int):")
      println("\t" + sym + " = " + value())
      Binding.putThingIntoMap(sym, (Type.INT, value()))
    }
  def :=(value: Function0[Float]) =
    {
      println("Assigning function0(float):")
      println("\t" + sym + " = " + value())
      Binding.putThingIntoMap(sym, (Type.FLOAT, value()))
    }
}

class mysimpleDCG {

  implicit def symbolToAssignment(name: Symbol): Assignment = new Assignment(name)

  //	implicit def declareToClass(d:Class) = new declare()
}

object declare {
  def int(assign: Symbol): IntAssignment = new IntAssignment(assign)
  def bool(assign: Symbol): BoolAssignment = new BoolAssignment(assign)
  def float(assign: Symbol): FloatAssignment = new FloatAssignment(assign)
  def string(assign: Symbol): StringAssignment = new StringAssignment(assign)
}

