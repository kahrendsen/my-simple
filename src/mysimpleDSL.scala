class MattsDream extends mysimpleDCG {

  def printTestTitle(str:String)
  {
    println("")
    println("Test Case: " + str)
  }  
  
  @Test
  def thisShouldFail()
  {
    printTestTitle("This Should Fail")		
	DECLARE NEWVAR 'blahhh := 1
	ENDALL   
  }

  // Tests for line numbers
  @Test
  def test_lineNumbers01() {
    printTestTitle("Line Number 1")
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

  // Different scope
  @Test
  def test_lineNumbers02() {
    printTestTitle("Line Number 2")
    DECLARE NEWVAR 'foo := 0
    WHILE(true)
    'foo := 'foo + 1
    ENDWHILE
    ENDALL
  }

  // Same scope
  @Test
  def test_lineNumbers03() {
    printTestTitle("Line Number 3")
    DECLARE NEWVAR 'foo := 0
    'foo := 'foo + 1
    ENDALL
  }

  // String corner case
  @Test
  def test_stringAndBoolConcat() {
    printTestTitle("String and bool concat")
    DECLARE NEWVAR 'bar := "Hi" + true
  }

  // Test for arithmetic operators and string concat
  @Test
  def test_arithmeticOperators1() {
    printTestTitle("Arith Ops 1")
    DECLARE NEWVAR 'matt := 10
    DECLARE NEWVAR 'test := 5 + 'matt
    DECLARE NEWVAR 'albert := true
    DECLARE NEWVAR 'happy := true && 'albert
    //DECLARE NEWVAR 'kendall := true + "" 
    //DECLARE NEWVAR 'hello := 4 + 'kendall + 1 + 1.0 
    ENDALL
  }
  
  // Test for boolean operators and comparators
  @Test
  def test_logicalOperators1() {
    printTestTitle("Logical ops 1")
    //	  DECLARE NEWVAR 'blahblah := 1 === 1
    DECLARE NEWVAR 'Cobb := 1 =/= 1
    ENDALL

  }
}
