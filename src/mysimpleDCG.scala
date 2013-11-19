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
    case class PrintString(num: Int, s: String) extends BasicLine
    
    var parseLine = 0
    var lines = new HashMap[Int, BasicLine]

  //case class LineBuilder(str: String) {
    object print
    {
    	//parseLine = parseLine + 1
        def apply(str:String) = lines(parseLine) = PrintString(parseLine, str)
        
    }
  // }
 }
     