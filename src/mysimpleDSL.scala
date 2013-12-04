object Albert extends mysimpleDCG {

  def main(args: Array[String]) = {
    DECLARE NEWVAR 'matt := 10
    DECLARE NEWVAR 'robert := ('matt + 5) + ('matt / 5.0)
    DECLARE NEWVAR 'other := true
    DECLARE FUNCTION('temp, 'hello)
      RETURN (4)
    ENDFUNCTION
    'matt := 10
    'matt := 5 
    'matt := 4.0
    'matt := true
    'matt := true
    'matt := true
    ENDALL

  }
}