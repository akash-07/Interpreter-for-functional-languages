# The_Interpreter

# TABLE OF CONTENTS
- [Overview](#overview)
- [Overview of Functional Language Constructs](#overview-of-functional-language-constructs)
- [Pretty Printer](#pretty-printer)
- [Lexer](#lexer)
- [Parser](#parser)
- [Template Instantiation Machine](#template-instantiation-machine)

===========================================================================================================================================
## Overview 

This implementation takes some program written in simple Core language and executes it. The Core language forms the basis for buildling Interpreters for high level functional languages such as Haskell whose constructs form the 'front end' of the Interpreter. Our initial focus is on building the back end which consists of constructs in the Core language. 

#### The Interpreter for Haskell would be built in four steps.

- The first step includes understanding syntax and sematics of haskell and knowing how it's different constructs would be handled by  the Interpreter. `CoreProgram` and `CoreExpression` will form the first of the two data types we declare for accepting core programs and expressions as input.
- Secondly, we would be designing a pretty printer to make the output readable and elegant.
- Thirdly, the Lexer would be made which would convert the input Program String into a list of Tokens.
- Fourthly, this list of tokens would be parsed to convert it into a format accepted by the Machine or as defined in the Interpreter.
- Lastly, the parsed tokens would then be evaluated by a graph reducer based on template instantiation.


=======================================================================================================================================
## Overview Of Functional Language Constructs

- The Program is considered to be set of Supercombinator definitions.Each supercombinator is an set of one or more variables evaluating to an expression. Supercombinators can define functions taking any number of arguements. The production rules look like ->
`program -> sc1 ; sc2 ; .... scn`
`sc -> var var1 var2 ... varn = expr`
**Let**
  ```haskell
  let <...bindigs....> in <expr>
  ```
  ### Example:
  ```haskell
  > let x = 5 in x*x
  === Final value  = 25 ===
  ```
  **Function Application**
  
=======================================================================================================================================
## Pretty Printer

=======================================================================================================================================
## Lexer

=======================================================================================================================================
## Parser

=======================================================================================================================================
## Template Instantiation Machine

===========================================================================================================================================
