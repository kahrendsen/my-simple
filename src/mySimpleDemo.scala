
object SimpleTest extends mysimpleDCG {
  def main(args: Array[String]) = {
    {
      DECLARE NEWVAR 'temp := "hello"
      DECLARE FUNCTION('matt, 'para)
        RETURN('para)
      ENDFUNCTION
      DECLARE NEWVAR 'something := CALLFUNCTION('matt, 5)
      DECLARE NEWVAR 'something2 := CALLFUNCTION('matt, "String")
      RETURN ('something)
      PRINT_TYPES
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
  def main(args: Array[String]) = {
    DECLARE NEWVAR 'foo := 0
    WHILE(true)
      'foo := 'foo + 1
    ENDWHILE
    ENDALL
  }
}

object LoopTestError extends mysimpleDCG {
  // Different scope
  def main(args: Array[String]) = {
    DECLARE NEWVAR 'foo := 0
    WHILE(1)
    	'foo := 'foo + 1
    ENDWHILE
    ENDALL
  }
}

object FunctionParameterInferenceTest_FloatSuccess extends mysimpleDCG {
  def main(args: Array[String]) =
    {
      DECLARE FUNCTION ('sqrt, 'x)
      	DECLARE NEWVAR 'result := 'x ** 0.5
      	RETURN('result)
      ENDFUNCTION
      DECLARE NEWVAR 'y := CALLFUNCTION('sqrt, 9.0)
      ENDALL
    }
}

object FunctionParameterInferenceTest_IntError extends mysimpleDCG {
  def main(args: Array[String]) =
    {
      DECLARE FUNCTION ('sqrt, 'x)
      	DECLARE NEWVAR 'result := 'x ** 0.5
      	RETURN('result)
      ENDFUNCTION
      DECLARE NEWVAR 'y := CALLFUNCTION('sqrt, -4)
      ENDALL
    }
}

object ScopingFuncTest extends mysimpleDCG {
  def main(args: Array[String]) =
    {
      DECLARE NEWVAR 'foo := true
      DECLARE FUNCTION ('hello, 'hi)
        WHILE(true)
          DECLARE NEWVAR 'foo := 600
        ENDWHILE
      ENDFUNCTION
      'foo := true
      RETURN ("hello")
      ENDALL
    }
}

object ScopingFuncTestError extends mysimpleDCG{
  def main(args: Array[String]) =
    {
      DECLARE NEWVAR 'foo := true
      DECLARE FUNCTION ('hello, 'hi)
        WHILE(true)
          'foo := 600
        ENDWHILE
      ENDFUNCTION
      'foo := true
      RETURN ("hello")
      ENDALL
    }
}

object BinaryParenthesisTest extends mysimpleDCG{
  def main(args: Array[String]) =
    {
      DECLARE NEWVAR 'foo := 5
      DECLARE NEWVAR 'bar := 4.0
      'bar := ('foo + 'bar) ** ('foo - 'bar)
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

object ParameterTest01 extends mysimpleDCG {
  def main(args: Array[String]) = {
	DECLARE FUNCTION('hello, 'hi)
		DECLARE NEWVAR 'foo := 'hi + 2.0
		RETURN ('foo)
	ENDFUNCTION
	DECLARE NEWVAR 'x := CALLFUNCTION('hello, 3.2)
	DECLARE NEWVAR 'y := CALLFUNCTION('hello, "qwerty")
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
    DECLARE NEWVAR 'Something := true
    DECLARE NEWVAR 'Happy := 5 >> 'Something
  }
}

object If extends mysimpleDCG{
  def main(args: Array[String]) = {
    IF(true)
    ENDIF
    RETURN (5.0)
    ENDALL
  }
}

object IfError extends mysimpleDCG{
  def main(args: Array[String]) = {
    IF(true)
    RETURN (5.0)
    ENDALL
  }
}

object UnclosedError extends mysimpleDCG {
  def main(args: Array[String]) = {
    DECLARE NEWVAR 'matthew := 4.0
    IF(true)
      WHILE(true)
        'matthew := 5.0
      ENDIF
    ENDWHILE
    ENDALL
  }
}
  
 object ImmediateReturn extends mysimpleDCG {
   def main(args: Array[String]) = {
    RETURN ("HELLO WORLD!")
    ENDALL
  }
 }
object FunctionCall extends mysimpleDCG{
  def main(args: Array[String]) = {
    DECLARE FUNCTION ('hello, 'param)
      DECLARE NEWVAR 'random := 5.0 + 'param
      RETURN ('param)
    ENDFUNCTION
    RETURN (CALLFUNCTION('hello, 4.0))
    ENDALL
  }
}

