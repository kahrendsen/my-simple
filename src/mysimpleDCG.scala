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
import scala.collection.mutable.Stack

class mysimpleDCG {

  // Current line number
  var currentLine: Int = 1

  //This preserves type information
  object Type {
    val INCOMP = -3
    val UNDEF = -2
    val UNKNOWN = -1
    val STRING = 0
    val FLOAT = 1
    val INT = 2
    val BOOL = 3

    def toString(typeConst: Int): String =
      {
        typeConst match {
          case Type.UNDEF => return "UNDEFINED"
          case Type.UNKNOWN => return "UNKNOWN"
          case Type.STRING => return "STRING"
          case Type.FLOAT => return "FLOAT"
          case Type.INT => return "INT"
          case Type.BOOL => return "BOOL"
          case Type.INCOMP => return "INCOMPATIBLE"
        }
      }
  }

  def PRINT_TYPES = Binding.printAll()

  object Binding {
    var scopedMaps = new LinkedList[HashMap[Symbol, Int]]()
    inception()
    privatePut(ds, Type.UNKNOWN)

    // we use this when we go in a scope
    def inception() = scopedMaps = scopedMaps.+:(new HashMap[Symbol, Int]())
    // we use this to get out of a scope
    def kick() = scopedMaps = scopedMaps.next

    def privatePut(sym: Symbol, v: Int) = {
      scopedMaps(0).put(sym, v)
    }

    // use this to declare a variable for the first time in our scope
    // returns an error if the variable already exists in the topmost scope
    def declareValue(sym: Symbol, v: Int) = {
      if (scopedMaps(0).contains(sym))
        ERROR.alreadyDef(sym)
      else {
        scopedMaps(0).put(sym, v)
        println("" + sym + ":" + Type.toString(v))
      }
    }

    // map a symbol to a type
    def put(sym: Symbol, v: Int) = {
      val mapContaining = getDeepestMapWith(sym)
      if (mapContaining != null) {
        mapContaining.put(sym, v)
        //println("" + sym + ":" + Type.toString(v) + " ass: " + sym.hashCode())
      } else
        ERROR.undef(sym)
    }

    // get the deepest mapping of symbol to type
    def get(sym: Symbol): Int = {
      val mapContaining = getDeepestMapWith(sym)
      if (mapContaining != null) {
        return mapContaining(sym)
      } else {
        return Type.UNDEF
      }
    }

    def getDeepestMapWith(sym: Symbol): HashMap[Symbol, Int] = {
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
          if (key != ds) {
            print("" + key + " --> :")
            value match {
              case Type.STRING => println("String")
              case Type.FLOAT => println("Float")
              case Type.INT => println("Int")
              case Type.BOOL => println("Bool")
              case _ => println("Unknown")
            }
          }
        }
        currentLevel -= 1
      }
      println("\n")
    }
  }

  // functionStack is how we keep track of which function we are currently in
  var functionStack = new Stack[(Symbol, Symbol)]()
  // functionParam maps function names to their parameter type
  // this is used for type checking when we attempt to call it later
  var functionParam = new HashMap[Symbol, Int]

  // temporarily assign unknown to the function and its parameter
  class Function(name: Symbol, param: Symbol) {
    Binding.privatePut(name, Type.UNKNOWN)
    Binding.inception()
    functionStack.push((name, param))
    MyStack.push(MyStack.FUNC)
    Binding.privatePut(param, Type.UNKNOWN)
    currentLine += 1
  }

  // pop off the function stack, go out a scope, and assign the parameter type to the function
  def ENDFUNCTION() = {
    currentLine += 1
    if (functionStack.length < 1) {
      println("ERROR: No more functions to end")
    }
    if (MyStack.pop != MyStack.FUNC)
      println("ERROR: Unfinished loops/ifs")
    val tuple = functionStack.pop
    val name = tuple._1
    val param = tuple._2
    functionParam.put(name, Binding.get(param))
    Binding.kick()
  }

  object RETURN {
    currentLine += 1
    def apply(value: Any) = {
      val tuple = functionStack.pop
      val name = tuple._1
      functionStack.push(tuple)
      value match {
        case a: Int => putFunctionType(name, Type.INT)
        case b: Double => putFunctionType(name, Type.FLOAT)
        case c: String => putFunctionType(name, Type.STRING)
        case d: Boolean => putFunctionType(name, Type.BOOL)
        case s: Symbol => {
          val typeOfS = Binding.get(s)
          if(typeOfS == Type.UNDEF)
            ERROR.undef(s)
          putFunctionType(name, typeOfS)
        }
      }
    }

    def putFunctionType(name: Symbol, fType: Int) = {
      if (Binding.get(name) == Type.UNKNOWN) {
        Binding.put(name, fType)
      } else if (Binding.get(name) != fType) {
        ERROR.wrongType(name, fType)
      }
    }
  }

  //TO DO: Be sure to check the type of param with the one stored in FunctionParam
  def CALLFUNCTION(name: Symbol, param: Any): Symbol = {
    if (Binding.getDeepestMapWith(name) == null)
      println("ERROR on line " + currentLine + ": Attempting to call undeclared function " + name)
    val paramType = typeSide(param)
    val expectedParamType = functionParam(name)
    if(expectedParamType == Type.UNKNOWN){
      functionParam(name) = paramType
    }else if(expectedParamType != paramType){
      println("ERROR on line " + currentLine + ": function " + name + " expected type " + Type.toString(expectedParamType) + ", got " + Type.toString(paramType)) 
    }
    return name
  }

  object MyStack {
    val IF = 0
    val WHILE = 1
    val FUNC = 2
    val scopingStack = new Stack[Int]()

    def push(value: Int) = scopingStack.push(value)
    def pop(): Int = scopingStack.pop()
    def peek(): Int = {
      val last = scopingStack.pop()
      scopingStack.push(last)
      last
    }
    def isEmpty(): Boolean = scopingStack.length == 0
  }

  def WHILE(cond: Any) = {
    Conditional(cond, MyStack.WHILE)
    currentLine += 1
  }

  def IF(cond: Any) = {
    Conditional(cond, MyStack.IF)
    currentLine += 1
  }
  
  def Conditional(cond: Any, t: Int) = {
    MyStack.push(t)
    cond match {
      case a: Int => ERROR.wrongConditional(Type.INT)
      case b: Double => ERROR.wrongConditional(Type.FLOAT)
      case c: String => ERROR.wrongConditional(Type.STRING)
      case d: Boolean => {

      }
      case s: Symbol => {
        val condType = Binding.get(s)
        if (condType != Type.BOOL)
          if (t == MyStack.IF)
            ERROR.wrongConditional(condType)
          else
            ERROR.wrongWhile(condType)
      }
      case f: Function0[Int] => {
        val condType: Int = f()
        if (condType != Type.BOOL)
          ERROR.wrongConditional(condType)
      }
      case _ => {
        if (t == MyStack.IF)
          ERROR.wrongConditional(Type.UNKNOWN)
        else
          ERROR.wrongWhile(Type.UNKNOWN)
      }
    }
    Binding.inception
  }

  def ENDWHILE() = {
    if (MyStack.isEmpty() || MyStack.pop() != MyStack.WHILE)
      println("ERROR on line " + currentLine + ": Attempting to close unopened 'WHILE'")
    Binding.kick
    currentLine += 1
  }

  def ELSE() = {
    Binding.kick
    if (MyStack.isEmpty() || MyStack.peek() != MyStack.IF)
      println("ERROR on line " + currentLine + ": Attempting to ELSE unopened 'IF'")
    Binding.inception
    currentLine += 1
  }

  def ENDIF() = {
    if (MyStack.isEmpty() || MyStack.pop() != MyStack.IF)
      println("ERROR on line " + currentLine + ": Attempting to close unopened 'IF'")
    Binding.kick
    currentLine += 1
  }

  def ENDALL() = {
    currentLine = 1
    while (!MyStack.isEmpty) {
      MyStack.pop match {
        case MyStack.IF => println("ERROR: Unclosed IF")
        case MyStack.WHILE => println("ERROR: Unclosed WHILE")
        case MyStack.FUNC => println("ERROR: Unended FUNCTION")
      }
    }
  }

  class Assignment(sym: Symbol) {
    def :=(value: Any) = {
      val varType = Binding.get(sym)
      if (varType == Type.UNDEF) {
        ERROR.undef(sym)
      } else {
        value match {
          case s: Symbol => {
            val valueType = Binding.get(s)
            if (valueType == Type.UNKNOWN) {
              Binding.put(s, varType)
            } else {
              if (valueType != varType) {
                ERROR.wrongType(sym, valueType)
              } else
                Binding.put(sym, valueType)
            }
          }
          case _ => bindToSymbol(value, varType)
        }
      }
      currentLine += 1
    }

    def bindToSymbol(value: Any, varType: Int) = {
      value match {
        case a: Int => {
          if (varType != Type.INT && varType != Type.UNKNOWN) {
            ERROR.wrongType(sym, Type.INT)
          } else {
            Binding.put(sym, Type.INT)
          }
        }
        case b: Double => {
          if (varType != Type.FLOAT && varType != Type.UNKNOWN) {
            ERROR.wrongType(sym, Type.FLOAT)
          } else {
            Binding.put(sym, Type.FLOAT)
          }
        }
        case c: Boolean => {
          if (varType != Type.BOOL && varType != Type.UNKNOWN) {
            ERROR.wrongType(sym, Type.BOOL)
          } else {
            Binding.put(sym, Type.BOOL)
          }
        }
        case d: String => {
          if (varType != Type.STRING && varType != Type.UNKNOWN) {
            ERROR.wrongType(sym, Type.STRING)
          } else {
            Binding.put(sym, Type.STRING)
          }
        }
        case f: Function0[Int] => {
          val valType = f()
          if (valType != varType) {
            ERROR.wrongType(sym, valType)
          } else {
            Binding.put(sym, valType)
          }
        }
        case _ => {
          ERROR.wrongType(sym, Type.UNKNOWN)
        }
      }
    }
  }

  class Thing(sym: Symbol) {
    def :=(value: Any) = {

      value match {
        case a: Int => {
          Binding.declareValue(sym, Type.INT)
        }
        case b: String => {
          Binding.declareValue(sym, Type.STRING)
        }
        case c: Boolean => {
          Binding.declareValue(sym, Type.BOOL)
        }
        case d: Double => {
          Binding.declareValue(sym, Type.FLOAT)
        }
        case s: Symbol => {
          val valueType = Binding.get(s)
          if (valueType == Type.UNKNOWN) {
            Binding.put(s, Type.UNKNOWN)
          } else if (valueType == Type.UNDEF) {
            ERROR.undef(s)
          } else if (valueType == Type.INCOMP) {
            ERROR.wrongType(s, valueType)
          } else {
            Binding.declareValue(sym, valueType)
          }
        }
      }
      currentLine += 1
    }
  }

  object DECLARE {
    def FUNCTION(name: Symbol, param: Symbol): Function = new Function(name, param)
    //Everything is just newVar now, we inference from that
    def NEWVAR(name: Symbol): Thing = new Thing(name)
  }

  implicit def symbolToAssignment(name: Symbol): Assignment = new Assignment(name)

  implicit def binaryRelation(any: Any): MathFunction = MathFunction(any)
  implicit def binaryRelationStringBoolHack(bool: Boolean) : MathFunction = MathFunction(bool)
  

  /*
   * We're doing strings. need to do def -(...) and def *(...) etc for all the otehr functions.
   * also need bools. floats/ints are done for +
   */
  val ds = 'dummySymbol
  case class MathFunction(lhs: Any) {

    def +(rhs: Any): Symbol = arithmetic(rhs, "+")
    def -(rhs: Any): Symbol = arithmetic(rhs, "-")
    def *(rhs: Any): Symbol = arithmetic(rhs, "*")
    def **(rhs: Any): Symbol = arithmetic(rhs, "**")
    def /(rhs: Any): Symbol = arithmetic(rhs, "/")
    
    def > (rhs: Any): Symbol = comparator(rhs, ">")
    def < (rhs: Any): Symbol = comparator(rhs, "<")
    def >= (rhs: Any): Symbol = comparator(rhs, ">=")
    def <= (rhs: Any): Symbol = comparator(rhs, "<=")
    def === (rhs: Any): Symbol = comparator(rhs, "===")
    def =/= (rhs: Any): Symbol = comparator(rhs, "=/=")
	
    def &&(rhs: Any): Symbol = boolLogic(rhs, "&&")
    def ||(rhs: Any): Symbol = boolLogic(rhs, "||")
    
    def comparator(rhs: Any, operator: String): Symbol =
    {
      val left = typeSide(lhs)
      val right = typeSide(rhs)
      
      if((lhs==Type.INT||lhs==Type.FLOAT)&&(rhs==Type.INT||rhs==Type.FLOAT))
      {
        Binding.privatePut(ds,Type.BOOL)
      }
      else
      {
        ERROR.wrongTypeInExpression(left, operator, right)
        Binding.privatePut(ds,Type.INCOMP)
      }
      return ds
    }

    def arithmetic(rhs: Any, operator: String): Symbol =
      {
        val left = typeSide(lhs)
        val right = typeSide(rhs)
        left match {
          case Type.INT => {
            right match {
              case Type.INT => Binding.privatePut(ds, Type.INT)
              case Type.FLOAT => Binding.privatePut(ds, Type.FLOAT)
              case Type.UNKNOWN => {
                Binding.privatePut(ds, Type.INT)
                rhs match{
                  case s:Symbol => Binding.put(s, Type.INT)
                }
              }
              case Type.STRING => {
                if (operator.equals("+")) {
                  Binding.privatePut(ds, Type.STRING)
                } else {
                  ERROR.wrongTypeInExpression(left, operator, right)
                  Binding.privatePut(ds, Type.INCOMP)
                }
              }
              case _ => {
                ERROR.wrongTypeInExpression(left, operator, right)
                Binding.privatePut(ds, Type.INCOMP)
              }
            }
          }
          case Type.FLOAT => {
            right match {
              case Type.INT => Binding.privatePut(ds, Type.FLOAT)
              case Type.FLOAT => Binding.privatePut(ds, Type.FLOAT)
              case Type.UNKNOWN => {
                Binding.privatePut(ds, Type.FLOAT)
                rhs match{
                  case s:Symbol => Binding.put(s, Type.FLOAT)
                }
              }
              case Type.STRING => {
                if (operator.equals("+")) {
                  Binding.privatePut(ds, Type.STRING)
                } else {
                  ERROR.wrongTypeInExpression(left, operator, right)
                  Binding.privatePut(ds, Type.INCOMP)
                }
              }
              case _ => {
                ERROR.wrongTypeInExpression(left, operator, right)
                Binding.privatePut(ds, Type.INCOMP)
              }
            }
          }
          case Type.STRING => {
            if (operator.equals("+")) {
              Binding.privatePut(ds, Type.STRING)
            } else {
              ERROR.wrongTypeInExpression(left, operator, right)
              Binding.privatePut(ds, Type.INCOMP)
            }
          }
          case Type.BOOL => {
            right match {
              case Type.STRING => {
                if (operator.equals("+")) {
                  Binding.privatePut(ds, Type.STRING)
                } else {
                  ERROR.wrongTypeInExpression(left, operator, right)
                  Binding.privatePut(ds, Type.INCOMP)
                }
              }
              case _ => {
                ERROR.wrongTypeInExpression(left, operator, right)
                Binding.privatePut(ds, Type.INCOMP)
              }
            }
          }
          case Type.UNKNOWN =>{
            var tempSym:Symbol = 'sym
            lhs match{
              case s:Symbol => tempSym = s
            }
            right match{
              case Type.INT => {
                Binding.privatePut(ds, Type.INT)
                Binding.privatePut(tempSym, Type.INT)
              }
              case Type.FLOAT => {
                Binding.privatePut(ds, Type.FLOAT)
                Binding.privatePut(tempSym, Type.FLOAT)
              }
              case Type.STRING => {
                if (operator.equals("+")) {
                  Binding.privatePut(ds, Type.STRING)
                  Binding.privatePut(tempSym, Type.STRING)
                } else {
                  ERROR.wrongTypeInExpression(left, operator, right)
                  Binding.privatePut(ds, Type.INCOMP)
                }
              }
              case _ => ERROR.wrongTypeInExpression(left, operator, right)
            }
          }
          case _ => {
            ERROR.wrongTypeInExpression(left, operator, right)
            Binding.privatePut(ds, Type.INCOMP)
          }
        }
        return ds
      }

    def boolLogic(rhs: Any, operator: String): Symbol =
      {
        val left = typeSide(lhs)
        val right = typeSide(rhs)
        left match {
          case Type.BOOL => {
            right match {
              case Type.BOOL => Binding.privatePut(ds, Type.BOOL)
              case Type.UNKNOWN => Binding.privatePut(ds, Type.BOOL)
              case _ => {
                ERROR.wrongTypeInExpression(left, operator, right)
                Binding.privatePut(ds, Type.INCOMP)
              }
            }
          }
          case _ =>
            {
              ERROR.wrongTypeInExpression(left, operator, right)
              Binding.privatePut(ds, Type.INCOMP)
            }
        }
        return ds
      }
  }
  def typeSide(side: Any): Int = {
    side match {
      case x: Int => return Type.INT
      case x: Double => return Type.FLOAT
      case x: String => return Type.STRING
      case x: Boolean => return Type.BOOL
      case x: Symbol => return Binding.get(x)
      case x: Function0[Symbol] => return Binding.get(x())
      case _ => return Type.UNKNOWN
    }
  }

  object ERROR {
    def alreadyDef(sym: Symbol) = {
      println("ERROR on line " + currentLine + ": " + sym + " is already defined with type " + Type.toString(Binding.get(sym)))
    }
    def wrongType(sym: Symbol, attempted: Int) = {
      println("ERROR on line " + currentLine + ": Attempted to assign type " + Type.toString(attempted) + " to " + sym + ":" + Type.toString(Binding.get(sym)))
    }
    def wrongTypeInExpression(lhsType: Int, operator: String, rhsType: Int) = {
      println("ERROR on line " + currentLine + ": Cannot perform " + Type.toString(lhsType) + " " + operator + " " + Type.toString(rhsType))
    }
    def incompType(sym: Symbol) = {
      println("ERROR on line " + currentLine + ": Attempted to assign to " + sym + ":" + Type.toString(Binding.get(sym)) + " Bad Types")
    }
    def undef(sym: Symbol) = {
      println("ERROR on line " + currentLine + ": attempt to access undefined " + sym)
    }
    def wrongConditional(attempted: Int) = {
      println("ERROR on line " + currentLine + ": Conditional expected type " + Type.toString(Type.BOOL) + " got type " + Type.toString(attempted))
    }
    def wrongWhile(attempted: Int) = {
      println("ERROR on line " + currentLine + ":  WHILE expected type " + Type.toString(Type.BOOL) + " got type " + Type.toString(attempted))
    }

  }
}
