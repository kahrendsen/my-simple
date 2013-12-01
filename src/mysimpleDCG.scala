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

import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedList

//This preserves type information
object Type {
  val UNKNOWN = -1
  val STRING = 0
  val FLOAT = 1
  val INT = 2
  val BOOL = 3
}

object ErrMsg {
  var INT = "integer"
  var STRING = "string"
  var FLOAT = "float"
  var BOOL = "bool"
  var UNKNOWN = "unknown-or-undefined"
  def errMsgType(str1:String, str2:String):String = 
  {
    return "ERROR: expected " + str1 + " got " + str2
  }
  def typeConstToStringName(typeConst:Int):String = 
  {
	typeConst match 
	{
	  case Type.UNKNOWN => return "unknown"
	  case Type.STRING => return "string"
	  case Type.FLOAT => return "float"
	  case Type.INT => return "int"
	  case Type.BOOL => return "bool"
    }
  }
}

object PRINT_TYPES {
  def apply() = Binding.printAll()
}

object Binding {
  var scopedMaps = new LinkedList[HashMap[Symbol, (Int, Any)]]()
  inception()

  def inception() = scopedMaps = scopedMaps.+:(new HashMap[Symbol, (Int, Any)]())
  def kick() = scopedMaps = scopedMaps.next

  def putValue(sym: Symbol, v: (Int, Any)) {
    val mapContaining = getDeepestMapWith(sym)
    if (mapContaining == null)
      scopedMaps(0).put(sym, v)
    else
      mapContaining.put(sym, v)
  }

  def getValue(sym: Symbol): Any = {
    val mapContaining = getDeepestMapWith(sym)
    if (mapContaining != null) {
      return mapContaining(sym)._2
    } else {
      return None
    }
  }

  def getType(sym: Symbol): Int = {
    val mapContaining = getDeepestMapWith(sym)
    if (mapContaining != null) {
      return mapContaining(sym)._1
    } else {
      return Type.UNKNOWN
    }
  }

  def getDeepestMapWith(sym: Symbol): HashMap[Symbol, (Int, Any)] = {
    for (currentMap <- scopedMaps) yield {
      if (currentMap.contains(sym))
        return currentMap
    }
    return null
  }

  def printAll() = {

    var currentLevel = scopedMaps.size
    for (currentMap <- scopedMaps) {
      println(currentLevel + " levels deep.")
      for ((key, value) <- currentMap) {
        print(key + " --> " + value._2 + ":")
        value._1 match {
          case Type.STRING => println("String")
          case Type.FLOAT => println("Float")
          case Type.INT => println("Int")
          case Type.BOOL => println("Bool")
          case _ => println("Unknown")
        }
      }
      currentLevel -= 1
    }
  }
}

class FunctionDeclaration(name: Symbol, param: Symbol) {

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
    value match {
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
  def :=(value: Any) = {
    val varType = Binding.getType(sym)
    if (varType == Type.UNKNOWN) {
      println("ERR : Variable not instantiated " + sym)
    } else {
      value match {
        case s: Symbol => bindValueToSymbol(Binding.getValue(s), varType)
        case _ => bindValueToSymbol(value, varType)
      }
    }
  }

  def bindValueToSymbol(value: Any, varType: Int) = {
    value match {
      case a: Int => {
        if (varType != Type.INT) println(ErrMsg.errMsgType(ErrMsg.INT, ErrMsg.typeConstToStringName(varType)))
        println("Assigning int:")
        println("\t" + sym + " = " + a)
        Binding.putValue(sym, (Type.INT, a))
      }
      case b: Double => {
        if (varType != Type.FLOAT) println(ErrMsg.errMsgType(ErrMsg.FLOAT, ErrMsg.typeConstToStringName(varType)))
        println("Assigning float:")
        println("\t" + sym + " = " + b)
        Binding.putValue(sym, (Type.FLOAT, b))
      }
      case c: Boolean => {
        if (varType != Type.BOOL) println(ErrMsg.errMsgType(ErrMsg.BOOL, ErrMsg.typeConstToStringName(varType)))
        println("Assigning boolean:")
        println("\t" + sym + " = " + c)
        Binding.putValue(sym, (Type.BOOL, c))
      }
      case d: String => {
        if (varType != Type.STRING) println(ErrMsg.errMsgType(ErrMsg.STRING, ErrMsg.typeConstToStringName(varType)))
        println("Assigning String:")
        println("\t" + sym + " = " + d)
        Binding.putValue(sym, (Type.STRING, d))
      }
      case _ => {
        println(ErrMsg.errMsgType(ErrMsg.UNKNOWN, ErrMsg.typeConstToStringName(varType)))
      }
    }
  }

  def :=(f: Function0[Any]) = {
    val value = f()
    value match {
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
  def func(name: Symbol, param: Symbol): FunctionDeclaration = new FunctionDeclaration(name, param)
}
