Type System for mySIMPL
=========

<<<<<<< HEAD
Overview
----
This is a Scala type system implementation for mySIMPL. The type system supports primitive types including: ineger, boolean, float, and string. Type inference allows for dynamically typed variables while we enforce coercion and type rules on top of this. Type checking will produce errors both during compile and runtime. Variable scoping prevents data leaks and unauthorized variable access.

*Files*

`src/mysimpleDCG.scala` - This file contains the Scala DCG which is responsible for parsing the input mySIMPL, type inferencing, checking, and error handling.

`src/mysimpleDSL.scala` - mySIMPL code is written in this file. This also contains the main method.

`src/mySimpleDemo.scala` - Contains working demo code including JUnit test cases.

Scope
----
* Type inferencing for variables and primitive types
* Dynamic function parameter types
* Coercion, operator, and expression rules
* Type checking
* Informative syntax errors

Binary Operators
----
* Arithmetic: +, -, /,\*, \*\* (exponent)
* Comparator: >, >=, <, <=, ===, =/=, 
* Logical: &&, ||
* Bitshift: <<, >>, >>>

Example:
```
DECLARE NEWVAR ‘tax := 0.92
DECLARE NEWVAR ‘total := 5.25 + ‘tax
```

Function Type &amp; Parameter Inference
----
* We infer the function return type using same techniques for assignment and declaration type inferencing
* We also infer the function parameter type. This is done in two ways.
* 1) Look at function body and examine expressions and binary operations.
* 2) Look at parameter types of previous function calls

Example:
```
DECLARE FUNCTION('hello, 'hi)
     DECLARE NEWVAR ‘foo := ‘hi + 2
     RETURN ‘foo
ENDFUNCTION
DECLARE NEWVAR 'foo := CALLFUNCTION(‘hello, 3.2)
```

In the example above, our type system will see the arithmetic operation inside function `hello` and assume that the parameter is a numeric type.

Syntax &amp; Type Error Handling
----
* The type system will inform the user of the specific line where a type error has occurred
* A type error may occur due to operations or expressions involving incorrect types including conditional statements and loops.
* To aid the user in identifying the specific error, we also identify the type mismatch and the (intended) operation


Team Members
----
* Kendall Ahrendsen
* Lyee Chong
* Albert Haque
* Matthew Lau
=======
####Overall project idea
* mySIMPL with type checking for variables.

####Goals
- [x] Form the super awesome team
- [x] Ask Rellermeyer if our idea is super awesome
- [x] Getting SOMETHING to work
- [x] Type checking for variables
- [ ] Don't derp too much while doing presentation

####Caveats from Rellermeyer
* While loops will be hard
* Nested functions may be challenging
* Try to have separate statements for assignment and declaration

####The Super Awesome Rellermeyer Squad
* Kendall Ahrendsen - Lead Slacker
* Lyee Chong - Director of Pessimism
* Albert Haque - Head Troll
* Matthew Lau - Manager of Procrastination (and occasional genius)
<p>
* With special thanks to Rellermeyer,
<br>Who inspired this project and continuously movitates us by making it 10% of our grade.
</p>

http://www.xkcd.com/1296/
>>>>>>> 97b414bbe6943b60dff5cac0e38cf44a8e29cef4
