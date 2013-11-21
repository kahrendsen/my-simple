/**
    #######        ##      ##    ##    ######     ####     ######     ##   ##
    ##    ##      ####      ##  ##    ##           ##     ##    ##    ##  ##
    #######      ##  ##       ##       ######      ##     ##          ####
    ##    ##    ########      ##            ##     ##     ##    ##    ##  ##
    #######    ##      ##     ##       ######     ####     ######     ##   ##
 */

  import scala.collection.mutable.Stack
  import scala.collection.mutable.HashMap

  class mysimpleDCG {
    abstract sealed class BasicLine
    case class PrintString(s: String) extends BasicLine

    val lines = new HashMap[Int, BasicLine]
    var parseLine = 0
    
    def PRINT(str:String) = println(str)
    def INT(sym:Symbol,i:Int) = println(""+sym+":="+i)
    
 }
     