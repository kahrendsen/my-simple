
object SimpleTest extends mysimpleDCG {
  def main(args: Array[String]) = {
    {
      DECLARE NEWVAR 'matt := 5
      DECLARE FUNCTION('happy, 'random)
        RETURN ('random)
      ENDFUNCTION
      PRINT_TYPES
      'matt := CALLFUNCTION('happy, 4)
      'matt := CALLFUNCTION('happy, 5)
      RETURN ('matt)
      RETURN ('matt)
      ENDALL
    }
  }
}
object DeclareTest extends mysimpleDCG {
  // Tests for line numbers
  def main(args: Array[String]) = {
    DECLARE NEWVAR 'matt := 10 //1
    DECLARE NEWVAR 'mootaz := 1 //2
    DECLARE NEWVAR 'test := 5 + 'matt //3
    DECLARE NEWVAR 'albert := true //4
    DECLARE NEWVAR 'mugan := 2.4 // His course eval 5
    DECLARE NEWVAR 'kristen := 10 //6
    DECLARE NEWVAR 'lin := "hi" //7
    DECLARE NEWVAR 'linnnnnn := 'albert + 'lin
    DECLARE NEWVAR 'happy := 1.0 + 4 + 2 + 1 * 3
    ENDALL
  }
}
object LoopTest extends mysimpleDCG {
  // Different scope
  def main(args: Array[String]) = {
    DECLARE NEWVAR 'foo := 0
    WHILE(2)
    'foo := 'foo + 1
    ENDWHILE
    ENDALL
  }
}

object ScopingFuncTest extends mysimpleDCG {
  def main(args: Array[String]) =
    {
      DECLARE FUNCTION ('hello, 'hi)
      WHILE(true)
      DECLARE NEWVAR 'foo := 600
      ENDWHILE
      ENDFUNCTION
      DECLARE NEWVAR 'foo := true
      ENDALL
    }
}

object LineNumbersTest extends mysimpleDCG {
  def main(args: Array[String]) = {
    DECLARE NEWVAR 'foo := 0
    'foo := 'foo + 1 + true
    ENDALL
  }
}

object StringCornerCaseTest extends mysimpleDCG {
  def main(args: Array[String]) = {
    DECLARE NEWVAR 'bar := "Hi" + true
  }
}
// Test for arithmetic operators and string concat
object ArithOpAndStrConcat extends mysimpleDCG {
  def main(args: Array[String]) = {
    DECLARE NEWVAR 'matt := 10
    DECLARE NEWVAR 'test := 5 + 'matt
    DECLARE NEWVAR 'albert := true
    DECLARE NEWVAR 'happy := true && 'albert
    //DECLARE NEWVAR 'kendall := true + "" 
    //DECLARE NEWVAR 'hello := 4 + 'kendall + 1 + 1.0 
    ENDALL
  }
}
object LogicalOpsTest extends mysimpleDCG {
  def main(args: Array[String]) = {
    //	  DECLARE NEWVAR 'blahblah := 1 === 1
    DECLARE NEWVAR 'Cobb := 1 =/= 1
    DECLARE NEWVAR 'counter := 'Cobb + 1
    ENDALL
  }
}

object BitShift extends mysimpleDCG {
  def main(args: Array[String]) = {
    DECLARE NEWVAR 'Something := 4
    DECLARE NEWVAR 'Happy := 5 >> 'Something
  }
}

