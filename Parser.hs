module Parser where

--Expr is Data type for Core language Expressions
import Data.Char
import Data.List
data Expr a = EVar Name 
			| ENum Int 
			| EConstr Int Int
			| EAp (Expr a) (Expr a )
			| ELet IsRec [(a,Expr a)] (Expr a)
			| ECase (Expr a) [Alter a]
			| ELam [a] (Expr a)
			deriving Show

type Name = String

-- Core Expressions are of type Expr Name where Name is String

type CoreExpr = Expr Name

-- A Boolen variable to check whether the let is recursive or not

type IsRec = Bool
recursive :: IsRec
nonRecursive :: IsRec
recursive = True
nonRecursive = False

-- bindersOf is to get the list of all the binding variables
bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name,rhs) <- defns]

-- rhsOf gets the list of expressions to which variables are bound
rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (name,rhs) <- defns]

--Alternatives are tuples consisiting of tag,list of var and an expr to evaluate
type Alter a = [(Int,[a],Expr a)]
type CoreAlt = Alter Name

--Boolean valued Fuction to check whether the expressions are atomic
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True 
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

-- Program is list of superCombinator definitions
type Program a = [ScDefn a]
type CoreProgram = Program Name

--ScDefn are a consist of Name, a list of bound variables and then an expr
type ScDefn a = (Name,[a],Expr a)
type CoreScDefn = ScDefn Name

-- I didn't understand what this is being used for!
preludeDefs :: CoreProgram
preludeDefs = [	("I",["x"],EVar"x"),
				("K",["x","y"],EVar "x"),
				("K1",["x","y"],EVar "y"),
				("S",["f","g","x"],EAp (EAp (EVar "f") (EVar "x"))	(EAp (EVar "g") (EVar "x"))),
				("compose",["f","g","x"],EAp (EVar "f") (EAp (EVar "g")(EVar "x"))),
				("twice",["f"],EAp (EAp (EVar "compose")(EVar "f")) (EVar "f")) ]


-- Lexer 

type Token = String

clex :: String -> Int -> [(Token,Int)]
--syntax :: [Token] -> CoreProgram
--parse :: String -> CoreProgram
--parse = syntax.clex

clex (c:cs) line
	| isNewLine c = clex cs (line+1)   
	| isSpace c = clex cs line
	| isDigit  c  = (num_Token,line) : clex restDigit_cs line
	| isAlpha c  = (var_tok,line) : clex restAlpha_cs line
	| isComment c = clex (dropWhile isNotNewLine cs) line
	| otherwise = ([c],line) : clex cs line where	
		var_tok = c : takeWhile isIdChar cs
		restAlpha_cs = dropWhile isIdChar cs
		num_Token = c : takeWhile isDigit cs
		restDigit_cs = dropWhile isDigit cs
		isNotNewLine c = c /= '\n'
		isComment c = c == '-'
		isNewLine c = c == '\n'
clex [] line = []

clex_getToken :: [(Token,Int)] -> [Token]
clex_getToken list = map getFirst list

getFirst :: (Token,Int)->Token
getFirst (token,line) = token

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_'

--Any Parser will take a String of tokens and return the value that is parsed and the remaining tokens to be parsed 
type Parser a = [Token] -> [(a,[Token])]

--pLit has been defined down again with the help of pSat
--pLit :: String -> Parser String
--pLit s (tok:toks) = if s == tok then [(s,toks)] else []
--pLit s  [] 		  = []

--pVar has been defined again below with the help of pSat
--pVar :: Parser String
--pVar (tok:toks) = [(tok,toks)]
--pVar [] = []

--pAlt tries different parsers and returns a list. In other words it's like '|' in a Production rule like hg -> hello | goodbye
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

--pThen parses using p1 and then p2 on tokens returned by p1. It's like A -> BC, so it will check for BC.
pThen :: (a->b->c) -> Parser a-> Parser b-> Parser c
pThen combine p1 p2 toks = [(combine v1 v2, toks2) | (v1,toks1) <- p1 toks, (v2, toks2) <- p2 toks1]

--pGreeting has been defined down again with the help of pThen3
--pGreeting :: Parser (String,String)
--pGreeting = pThen mk_pair pHelloOrGoodbye pVar where
--				mk_pair hg name = (hg,name)

--Same as pThen but checks for BCD in the production rule A -> BCD
pThen3 :: (a->b->c->d) -> Parser a->Parser b->Parser c->Parser d
pThen3 combine p1 p2 p3 toks = [(combine v1 v2 v3, toks3) | (v1,toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3 ,toks3 )<- p3 toks2]

pGreeting :: Parser (String,String)
pGreeting = pThen3 mk_greeting pHelloOrGoodbye pVar (pLit "!") where
				mk_greeting hg name exclamation = (hg,name) 

--Same as pThen but checks for BCDE in the production rule A -> BCDE
pThen4 :: (a->b->c->d->e) -> Parser a->Parser b->Parser c->Parser d->Parser e
pThen4 combine p1 p2 p3 p4 toks = [(combine v1 v2 v3 v4, toks4) | (v1,toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3 ,toks3 )<- p3 toks2, (v4 , toks4) <- p4 toks3]

--Checks for one or more occurences of a particular parser like in A -> BBBBC , so here B is repeated and parsed by pOneOrMore
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p1 = pThen (:) p1 (pZeroOrMore p1) 

--Doesn't parse anything or behaves as if it consumed everything
pEmpty :: a -> Parser a
pEmpty a = \tok -> [(a,tok)]

--Same as pOneOrMore but instead checks for zero or more occurences of a particular parser
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting

-- Converts from Parser of type a to type b using a function as input
pApply :: Parser a->(a->b)->Parser b
pApply p1 fn toks = [(fn v1,toks1)|(v1,toks1)<-p1 toks]

pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length

--Checks for separators as well as parsers
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen3 combine_pOneOrMoreWithSep p1 p2 (pZeroOrMoreWithSep p1 p2) where
									combine_pOneOrMoreWithSep p1_a p2_b p3_list_a = p1_a:p3_list_a

pZeroOrMoreWithSep :: Parser a->Parser b->Parser [a]
pZeroOrMoreWithSep p1 p2 = (pOneOrMoreWithSep p1 p2) `pAlt` (pEmpty [])

--combine_pOneOrMoreWithSep :: (a->b->[a]->[a])
--combine_pOneOrMoreWithSep a b c = a:c

--Used to implement pLit and pNum and pVar
pSat :: (String -> Bool) -> Parser String
pSat fn [] = []
pSat fn (tok:toks) = if(fn tok) then [(tok,toks)] else [] 

-- pLit defined here again
pLit :: String -> Parser String
pLit s = pSat (==s)

keywords :: [String]
keywords = ["let","letrec","case","in","of","Pack"]

--pVar defined here again
pVar :: Parser String
pVar s = pSat (checkForKeyword keywords) s

checkForKeyword :: [String] -> String -> Bool
checkForKeyword list str = str `notElem` list && (isAlpha(str!!0))

pNum :: Parser Int
pNum = (pSat checkStringIsNum) `pApply` convertStringToInt

convertStringToInt :: String -> Int
convertStringToInt str = read str::Int

checkStringIsNum :: String -> Bool
checkStringIsNum str = (all isDigit str)

syntax :: [Token] -> CoreProgram
syntax = take_first_parse.pProgram where
			take_first_parse ((prog,[]):others) = prog
			take_first_parse (parse : others) = take_first_parse others
			take_first_parse other  = error "Syntax Error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr where 
				mk_sc name list_names equalTo expr = (name,list_names,expr)
{-
pExpr :: Parser CoreExpr
pExpr = pThen4 fn1 (pLit "let") pDefns (pLit "in") pExpr `pAlt`
		pThen4 fn2 (pLit "letrec") pDefns (pLit "in") pExpr `pAlt`
		pThen4 fn3 (pLit "case") pDefns (pLit "of") pAlts `pAlt`
		pThen4 fn4 (pLit "\") (pOneOrMore pVar) (pLit "->") (pExpr) `pAlt`
		pAxpr	
-}

-- Production rule:  apexpr -> Num | Var 
pAexpr :: Parser CoreExpr
pAexpr = (pVar `pApply` var_EVar) `pAlt` (pNum `pApply` num_Enum) where
			var_EVar str = EVar str 
			num_Enum num = ENum num

-- Production rule: binop -> + | - | * | /
pBinop :: Parser String
pBinop = (pLit "+") `pAlt` (pLit "-") `pAlt` (pLit "*") `pAlt` (pLit "/")


-- Production rule: expr -> arithexpr1 | let defn in expr
pExpr :: Parser CoreExpr
pExpr = pExprArith1 `pAlt` 
        pThen4 foo2 (pLit "let") (pDefns) (pLit "in") (pExpr) where
        	foo2 p1_let p2_defns p3_in p4_expr = ELet nonRecursive p2_defns p4_expr 


-- Production rule: defn -> var = expr 
pDefn :: Parser (String, Expr Name)
pDefn = pThen3 foo3 (pVar) (pLit "=") (pExpr) where 
						foo3 name equalTo expr = (name, expr)


-- Production rule: defns -> defn1;defn2; ....;defnn 
pDefns :: Parser [(String,Expr Name)]
pDefns = pOneOrMore pDefn

-- Below given three rules are as follows:
-- expr1 -> expr2 + expr 1 | expr2 - expr 1 | expr2 
-- expr2 -> expr3 * expr 2 | expr3 / expr 2 | expr3
-- expr3 -> apexpr

pExprArith1 :: Parser CoreExpr
pExprArith1 = pThen3 foo1 (pExprArith2) (pLit "+") (pExprArith1) `pAlt`
			  pThen3 foo1 (pExprArith2) (pLit "-") (pExprArith2) `pAlt` 
			  pExprArith2 where
					foo1 expr1 binop expr2 = EAp (EAp (EVar binop) expr2) expr1

pExprArith2 :: Parser CoreExpr
pExprArith2 = pThen3 foo1 (pExprArith3) (pLit "*") (pExprArith2) `pAlt`
			  pThen3 foo1 (pExprArith3) (pLit "/") (pExprArith3) `pAlt` 
			  pExprArith3 where
					foo1 expr1 binop expr2 = EAp (EAp (EVar binop) expr2) expr1

pExprArith3 :: Parser CoreExpr
pExprArith3 = pAexpr
