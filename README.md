# Haskell asks, "Can I Interpret myself?"

Interpreter for functional languages using template instantiation.  

# Table Of Contents
- [Overview](#overview)
- [Overview of Functional Language Constructs](#overview-of-functional-language-constructs)
- [Pretty Printer](#pretty-printer)
- [Lexer](#lexer)
- [Parser](#parser)
- [Template Instantiation Machine](#template-instantiation-machine)
- [References](#references)

=======================================================================================================================================
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
- Constructs 
 - **Let**
  
  There are recursive and non recursive let bindings.
  
  ```haskell
  let <...bindigs....> in <expr>
  ```
  ### Example:
  ```haskell
  > let x = 5 in x*x
  === Final value  = 25 ===
  ```
 - **Function Application**
  ```haskell
  function <args>
  ```
  ###Examle:
  ```haskell
  square x = x*x
  ```
 - **Data Constructors** 
  ```haskell
  Pack{tag,arity}
  ```
  Tag represents a specific constructor and arity represents number of arguements taken by the constructor.
  
  ### Example:
  ```haskell
  Data Colour = Red | Green | Blue 
  ```
  This would be represented as:
  ```haskell
  Red = {1,0}
  Green  = {2.0}
  Blue = {3,0}
  ```
 - **Case expressions**
  ```haskell
  case <expr> of
  ```
  ### Example:
  ```haskell
  isGreen colour  = case colour of 
                          <1> -> False;
                          <2> -> True;
                          <3> -> False
  ```
 - **Comments**
  ```haskell
  # A comment begins with a # in core language and continues till the end of line. (No multiline comments)
  ```
 - **Lambda abstractions**
  ```haskell
  square = \x. x*x
  ```
  
== Note == 

Lamda abstractions and case expressions are not evaluated by this machine as they require certain different implementations.

=======================================================================================================================================
## Pretty Printer

The pretty printer makes the ouput more readable. We will be developing our own printer called ``pprint``. The implementation of this part has been skipped to move on to implementing Instantiation machine as early as possible.

=======================================================================================================================================
## Lexer

- The Lexer essentially converts an input string into a list of Tokens. Tokens are nothing but indivdual units of String which cannot be further divided. So `let` ,` =` , `25` ,etc are essentially tokens where `let` is keyword in core language, `=` represents the equal to sign and `25` is just an Integer.   
- So we define a set of keywords and operators to be considered while lexing as:
 
 ```haskell
 keywords :: [String]
 keywords = ["let","letrec","case","in","of","Pack"]
 
 binop :: [String]
 binop = ["=","<",">","<=",">=","-","+","/","*","|","&"]
 ```
 ### Example
 
 ```haskell
 square x = x*x
 main = square 2
 ```
 This will we tokenized as
 
 ```haskell
 ["square","x","=","x","*","x","main","=","square","2"]
 ```
- All the whitespace is ignored while lexing.
- Comments will be ignored while lexing which are identified as beginning with `#`.
- Variables are String beginning with an alphabet followed by alphanumeric characters or underscores.
- Thus lexing converts input program into a list of tokens which is then fed to the parser.

=======================================================================================================================================
## Parser

This forms the most important step in building an Interpreter as we need to convert the tokenized strings into a form that can be understood by our machine.

- We begin with defining Expr data type for core language expressions as:

   ```haskell
     data Expr a = EVar Name 
        | ENum Int 
        | EConstr Int Int
        | EAp (Expr a) (Expr a )
        | ELet IsRec [(a,Expr a)] (Expr a)
        | ECase (Expr a) [Alter a]
        | ELam [a] (Expr a)
        deriving Show
   ```
  This type handles all basic exprssion in the core language.

- Next we need to define a few types which we will be using for parsing like
  ```haskell
  type Name = String
  type CoreExpr = Expr Name
  type Program a = [ScDefn a]
  type CoreProgram = Program Name
  type ScDefn a = (Name,[a],Expr a)
  type CoreScDefn = ScDefn Name
  type Token = String
  type Parser a = [Token] -> [(a,[Token])]
  ```
  This basically tells you that CoreExpr is an expression of type `Name` which is an alias for String.
  Similarly we can see that a Program is a  list of supercombinator definitions and SuperCombinator definitions are represented as a  tuple containing name, list of variables and and expression.  
  
 - In all the following text 'p' stands for a parser. We have a few parser combinators like:
 
  - pAlt tries different parsers and returns a list. In other words it's like '|' in a Production rule like hg -> hello | goodbye

   ```haskell
   pAlt :: Parser a -> Parser a -> Parser a
    ```
  - pThen parses using p1 and then p2 on tokens returned by p1. It's like A -> BC, so it will check for BC.

   ```haskell
   pThen :: (a->b->c) -> Parser a-> Parser b-> Parser c
    ```
   Similarly we have pThen3 and pThen4.
  
  - Checks for one or more occurences of a particular parser like in A -> BBBBC , so here B is repeated and parsed by pOneOrMore
    
    ```haskell
    pOneOrMore :: Parser a -> Parser [a]
    ```
    Similarly we have pZeroOrMore, pOneOrMoreWithSep , pEmpty, pSat.
    
  - Converts from Parser of type a to type b using a function as input

     ```haskell
     pApply :: Parser a->(a->b)->Parser b
     ```
  
  - `pLit` checks for a literal, `pVar` checks for a variable and `pNum` checks for a Number.
   
    ``haskell
    pLit :: String -> Parser String
    pVar :: Parser String
    pNum :: Parser Int
    ``
    
 - Once these parser combinators are done, we define production rules using them as: 
   
   ```haskell
   pProgram :: Parser CoreProgram
   pProgram = pOneOrMoreWithSep pSc (pLit ";")
   ```
   
   Now what this tells is a program is set a of one or more supercombinators separated with literal ";" . So it resembles the production rule program -> sc1 ; sc2; sc3; .... scn   
 
 - We then define pDefn, pDefns, pExpr, pAexpr, pExprArith1 and others in a similar manner.
 
   Thus the parser converts tokenized strings into an ouput that can be evaluated by our machine.
 
   ### Example:
 
   ```haskell
   x = 3;
   square x = x*x; 
   
   == After parsing ==
   
   [("x",[],ENum 3),("square",["x"],EAp (EAp (EVar "*") (EVar "x")) (EVar "x"))]
   ```

=======================================================================================================================================
## Template Instantiation Machine

The functional program is executed by evaluating an expression. The expression is represented by a graph whose evaluation takes place by carrying out a sequence of reductions. 

This part is yet to be made.

=======================================================================================================================================
## References

- Special thanks to bollu and his Interpreter TIMI: https://github.com/bollu/timi
- Implementing functional Languages, a tutorial by Peyton Jones and David Lester.

=======================================================================================================================================

Currently Working On : Template Instantiation Machine.
