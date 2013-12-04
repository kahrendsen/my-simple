object Albert extends mysimpleDCG {

  def main(args:Array[String]) = {
	println("--------------------------------")
    println("Test Case: Line Number 01")
    test_lineNumbers01();
	
    println("--------------------------------")
    println("Test Case: Line Number 02")
    test_lineNumbers02();
    
    println("--------------------------------")
    println("Test Case: Line Number 03")
    test_lineNumbers03();
    
    println("--------------------------------")
    println("Test Case: Logical Operators 01")
    test_logicalOperators1();
    
  }
  
  // Tests for line numbers
  def test_lineNumbers01() {
       DECLARE NEWVAR 'matt := 10 //1
       DECLARE NEWVAR 'mootaz := 1 //2
       DECLARE NEWVAR 'test := 5 + 'matt //3
       DECLARE NEWVAR 'albert := true  //4
       DECLARE NEWVAR 'mugan := 2.4 // His course eval 5
       DECLARE NEWVAR 'kristen := 10 //6
       DECLARE NEWVAR 'lin := "hi"  //7
       DECLARE NEWVAR 'linnnnnn := 'albert + 'lin
       DECLARE NEWVAR 'happy := 1.0 + 4 && 2 + 1 * 3
       ENDALL
  }
  
  // Different scope
  def test_lineNumbers02(){
    DECLARE NEWVAR 'foo :=0
    WHILE(true)
    	'foo := 'foo + 1
    ENDWHILE
    ENDALL
  }
  
  // Same scope
  def test_lineNumbers03() {
    DECLARE NEWVAR 'foo := 0
    'foo := 'foo + 1
    ENDALL
  }
  
  
  // Test for arithmetic operators and string concat
  def test_arithmeticOperators1() {
       DECLARE NEWVAR 'matt := 10
       DECLARE NEWVAR 'test := 5 + 'matt
       DECLARE NEWVAR 'albert := true
       DECLARE NEWVAR 'happy := true && 'albert
       //DECLARE NEWVAR 'kendall := true + "" 
       //DECLARE NEWVAR 'hello := 4 + 'kendall + 1 + 1.0 
       ENDALL
  }
  // Test for boolean operators and comparators
  def test_logicalOperators1(){
//	  DECLARE NEWVAR 'blahblah := 1 === 1
	  DECLARE NEWVAR 'Cobb := 1 =/= 1
	  ENDALL
  }
}






















