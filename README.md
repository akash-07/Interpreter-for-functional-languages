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

#### The Interpreter for Haskell would be built in four steps. 

This implementation takes some program written in simple Core language and executes it. The Core language forms the basis for buildling Interpreters for high level functional languages such as Haskell whose constructs form the 'front end' of the Interpreter. Our initial focus is on building the back end which consists of constructs in the Core language. 

- The first step includes understanding syntax and sematics of haskell and knowing how it's different constructs would be handled by  the Interpreter. `CoreProgram` and `CoreExpression` will form the first of the two data types we declare for accepting core programs and expressions as input.
- Secondly, we would be designing a pretty printer to make the output readable and elegant.
- Thirdly, the Lexer would be made which would convert the input Program String into a list of Tokens.
- Fourthly, this list of tokens would be parsed to convert it into a format accepted by the Machine or as defined in the Interpreter.
- Lastly, the parsed tokens would then be evaluated by a graph reducer based on template instantiation.


=======================================================================================================================================
## Overview Of Functional Language Constructs

=======================================================================================================================================
## Pretty Printer

=======================================================================================================================================
## Lexer

=======================================================================================================================================
## Parser

=======================================================================================================================================
## Template Instantiation Machine

===========================================================================================================================================
