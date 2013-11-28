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


class Holder(){
    var stringValue = ""
    var intValue = 0
    var floatValue = 0.0
    var boolValue = true
}


object Binding {

  var map = new HashMap[Symbol, (Int, Holder)]()
  def putThingIntoMap(sym: Symbol, v: (Int, Holder)) {
    map.put(sym, v)
  }
   def getTypeFromMap(sym: Symbol): Int = {
      map.get(sym).get._1
   }
   def getIntFromMap(sym: Symbol): Int = {
     map.get(sym).get._2.intValue
   }
   def getStringFromMap(sym: Symbol): String = {
     map.get(sym).get._2.stringValue
   }
   def getFloatFromMap(sym: Symbol): Double = {
     map.get(sym).get._2.floatValue
   }
   def getBoolFromMap(sym: Symbol): Boolean = {
     map.get(sym).get._2.boolValue
   }
}

  //        val intMap = HashMap[Symbol,Int]()
  //        val boolMap = HashMap[Symbol,Boolean]()
  //        val stringMap = HashMap[Symbol,String]()
  //        val floatMap = HashMap[Symbol,Float]()
  //        
  //        def getValue(sym:Symbol) {
  //          if(intMap.contains(sym)) {
  //            return intMap(sym)
  //          }
  //          else if (floatMap.contains(sym)) {
  //            value = floatMap(sym)
  //          }
  //          else {
  //            println("cry")
  //          }
  //        }

class StringAssignment(sym: Symbol) {
  // Bind this and add to variable list
  def :=(value: String) = {
    println("Assigning string:")
    println("\t" + sym + " = " + value)
    var tempHolder = new Holder()
    tempHolder.stringValue = value
    Binding.putThingIntoMap(sym, (Type.STRING, tempHolder))
  }
}
class IntAssignment(sym: Symbol) {
  def :=(value: Int) = {
    println("Assigning int:")
    println("\t" + sym + " = " + value)
    (sym, value)
    var tempHolder = new Holder()
    tempHolder.intValue = value
    Binding.putThingIntoMap(sym, (Type.INT, tempHolder))
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
      var tempHolder = new Holder()
      tempHolder.floatValue = value
      Binding.putThingIntoMap(sym, (Type.FLOAT, tempHolder))
    }
}
class BoolAssignment(sym: Symbol) {
  def :=(value: Boolean) =
    {
      println("Assigning bool:")
      println("\t" + sym + " = " + value)
      (sym, value)
      var tempHolder = new Holder()
      tempHolder.boolValue = value
      Binding.putThingIntoMap(sym, (Type.BOOL, tempHolder))
      
    }
}

class Assignment(sym: Symbol) { // This handles expressions THIS WON'T WORKKKK D:
  def :=(value: Function0[Int]) =
    {
      println("Assigning function0(int):")
      println("\t" + sym + " = " + value())
      
      //Binding.putThingIntoMap(sym, (Type.INT, value()))
    }
  /*def :=(value: Function0[Float]) =
    {
      println("Assigning function0(float):")
      println("\t" + sym + " = " + value())
      Binding.putThingIntoMap(sym, (Type.FLOAT, value()))
    }*/
}

class mysimpleDCG {

  implicit def symbolToAssignment(name: Symbol): Assignment = new Assignment(name)

  //        implicit def declareToClass(d:Class) = new declare()
}

object declare {
  def int(assign: Symbol): IntAssignment = new IntAssignment(assign)
  def bool(assign: Symbol): BoolAssignment = new BoolAssignment(assign)
  def float(assign: Symbol): FloatAssignment = new FloatAssignment(assign)
  def string(assign: Symbol): StringAssignment = new StringAssignment(assign)
}
