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
import java.util.Stack

class mysimpleDCG{

	//This preserves type information
	object Type {
	  val UNDEF = -2
	  val UNKNOWN = -1
	  val STRING = 0
	  val FLOAT = 1
	  val INT = 2
	  val BOOL = 3
	  
	  def toString(typeConst:Int):String = 
	  {
	        typeConst match 
	        {
	          case Type.UNDEF => return "undefined"
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
	  var scopedMaps = new LinkedList[HashMap[Symbol, Int]]()
	  inception()
	  
	  // we use this when we go in a scope
	  def inception() = scopedMaps = scopedMaps.+:(new HashMap[Symbol, Int]())
	  // we use this to get out of a scope
	  def kick() = scopedMaps = scopedMaps.next
	  
	  // use this to declare a variable for the first time in our scope
	  // returns an error if the variable already exists in the topmost scope
	  def declareValue(sym: Symbol, v: Int) = {
	    if( scopedMaps(0).contains(sym))
	      ERROR.alreadyDef(sym)
	    else{
	      scopedMaps(0).put(sym, v)
	      println(sym+":"+Type.toString(v))
	    }
	  }
	  
	  // map a symbol to a type
	  def put(sym: Symbol, v: Int) = {
	    val mapContaining = getDeepestMapWith(sym)
	    if(mapContaining != null){
	      mapContaining.put(sym, v)
	      println(sym +":"+ Type.toString(v))
	    }else
	      ERROR.undef(sym)
	  }
	  
	  // get the deepest mapping of symbol to type
	  def get(sym: Symbol): Int = {
	    val mapContaining = getDeepestMapWith(sym)
	    if(mapContaining != null){
	      return mapContaining(sym)
	    }else{
	      return Type.UNDEF
	    }
	  }
	  
	  def getDeepestMapWith(sym: Symbol): HashMap[Symbol, Int] = {
	    for(currentMap <- scopedMaps) yield {
	      if(currentMap.contains(sym))
	        return currentMap
	    }
	    return null
	  }
	  
	  def printAll() = {
	    
	    var currentLevel = scopedMaps.size
	    for(currentMap <- scopedMaps){
	    	println(currentLevel + " levels deep.")
		    for((key, value) <- currentMap){
		      print(key + " --> :")
		      value match{
		        case Type.STRING => println("String")
		        case Type.FLOAT => println("Float")
		        case Type.INT => println("Int")
		        case Type.BOOL => println("Bool")
		        case _ => println("Unknown")
		      }
		    }
	    	currentLevel-=1
	    }
	  }
	}
	
	// functionStack is how we keep track of which function we are currently in
	var functionStack = new Stack[(Symbol, Symbol)]()
	// functionParam maps function names to their parameter type
	// this is used for type checking when we attempt to call it later
	var functionParam = new HashMap[Symbol, Int]
	
	// temporarily assign unknown to the function and its parameter
	class Function(name: Symbol, param: Symbol) {
	  Binding.declareValue(name, Type.UNKNOWN)
	  Binding.inception()
	  functionStack.push((name, param))
	  Binding.declareValue(param, Type.UNKNOWN)
	  
	  
	}
	
	// pop off the function stack, go out a scope, and assign the parameter type to the function
	object endfunction{
	  def apply() = {
	    if(functionStack.empty())
	      println("ERROR: No more functions to end")
	  
	    val name = functionStack.peek()._1
	    val param = functionStack.peek()._2
	    functionParam.put(name, Binding.get(param))
	    Binding.kick()
	  }
	}
	
	object RETURN {
	  def apply(value: Any) = {
	    val name = functionStack.peek()._1
	    value match{
	      case a:Int => putFunctionType(name, Type.INT)
	      case b:Double => putFunctionType(name, Type.FLOAT)
	      case c:String => putFunctionType(name, Type.STRING)
	      case d:Boolean => putFunctionType(name, Type.BOOL)
	      case s:Symbol => putFunctionType(name, Binding.get(s))
	    }
	  }
	  
	  def putFunctionType(name: Symbol, fType: Int) = {
	    if(Binding.get(name) == Type.UNKNOWN){
	      Binding.put(name, fType)
	    }else if(Binding.get(name) != fType){
	      ERROR.wrongType(name, fType)
	    }
	  }
	}
	
	class Assignment(sym: Symbol) {
	  def :=(value: Any) = {
	    val varType = Binding.get(sym)
	    if(varType == Type.UNDEF){
	    	ERROR.undef(sym)
	    }else{
	    	value match{
		    	case s: Symbol => {
		    	    val valueType = Binding.get(s)
		    	    if(valueType == Type.UNKNOWN){
		    	    	Binding.put(s, varType)
		    	    }else{
		    	    	if(valueType != varType)
		    	    		ERROR.wrongType(sym, valueType)
		    	    	else
		    	    		Binding.put(sym, valueType)
		    	    }
		    	}
		    	case _ => bindToSymbol(value, varType)
	    	}
	    }
	  }
	  
	  def bindToSymbol(value: Any, varType: Int ) = {
	    value match{
	        case a: Int => {
	          if(varType != Type.INT && varType != Type.UNKNOWN)
	        	 ERROR.wrongType(sym, Type.INT)
	          Binding.put(sym, Type.INT)
	        }
	        case b: Double => {
	          if(varType != Type.FLOAT && varType != Type.UNKNOWN)
	            ERROR.wrongType(sym, Type.FLOAT)
	          Binding.put(sym, Type.FLOAT)
	        }
	        case c: Boolean => {
	          if(varType != Type.BOOL && varType != Type.UNKNOWN)
	        	  ERROR.wrongType(sym, Type.BOOL)
	          Binding.put(sym, Type.BOOL)
	        }
	        case d: String => {
	          if(varType != Type.STRING && varType != Type.UNKNOWN)
	        	  ERROR.wrongType(sym, Type.STRING)
	          Binding.put(sym, Type.STRING)
	        }
	        case _ => {
	          println("ERR")
	        }
	      }
	  }
	  
	  def :=(f: Function0[Any]) = {
	      val value = f()
	      value match{
	        case a: Int => {
	          Binding.put(sym, Type.INT)
	        }
	        case b: String => {
	          Binding.put(sym, Type.STRING)
	        }
	        case c: Boolean => {
	          Binding.put(sym, Type.BOOL)
	        }
	        case d: Double => {
	          Binding.put(sym, Type.FLOAT)
	        }
	        case _ => {
	          println("ERR:")
	        }
	      }
	    }
	}
	
	class Thing(sym: Symbol) {
	  def :=(value: Any) = {
	    value match{
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
		    	    if(valueType == Type.UNKNOWN){
		    	    	Binding.put(s, Type.UNKNOWN)
		    	    }else if(valueType == Type.UNDEF){
		    	    	ERROR.undef(s)
		    	    }else{
		    	    	Binding.declareValue(sym, valueType)
		    	    }
	        }
	      }
	  }
	}
	
	object declare {
	  def function(name: Symbol, param: Symbol): Function = new Function(name, param)
	  //Everything is just newVar now, we inference from that
	  def newVar(name: Symbol): Thing = new Thing(name)
	}
	
	implicit def symbolToAssignment(name: Symbol): Assignment = new Assignment(name)

	object ERROR {
	  def alreadyDef(sym: Symbol) = {
	    println("ERROR:" + sym + " is already defined with type " + Type.toString(Binding.get(sym)))
	  }
	  def wrongType(sym: Symbol, attempted: Int) = {
	    println("ERROR: Attempted to assign type" + Type.toString(attempted) + " to " + sym + ":" + Type.toString(Binding.get(sym)))
	  }
	  def undef(sym: Symbol) = {
	    println("ERROR: attempt to access undefined "+ sym)
	  }
	  
	}

}
