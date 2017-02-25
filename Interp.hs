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

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_'


type Parser a = [Token] -> [(a,[Token])]

pLit :: String -> Parser String
pLit s (tok:toks) = if s == tok then [(s,toks)] else []
pLit s  [] 		  = []

pVar :: Parser String
pVar (tok:toks) = [(tok,toks)]
pVar [] = []

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pThen :: (a->b->c) -> Parser a-> Parser b-> Parser c
pThen combine p1 p2 toks = [(combine v1 v2, toks2) | (v1,toks1) <- p1 toks, (v2, toks2) <- p2 toks1]

pGreeting :: Parser (String,String)
--pGreeting = pThen mk_pair pHelloOrGoodbye pVar where
--				mk_pair hg name = (hg,name)

pThen3 :: (a->b->c->d) -> Parser a->Parser b->Parser c->Parser d
pThen3 combine p1 p2 p3 toks = [(combine v1 v2 v3, toks3) | (v1,toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3 ,toks3 )<- p3 toks2]

pGreeting = pThen3 mk_greeting pHelloOrGoodbye pVar (pLit "!") where
				mk_greeting hg name exclamation = (hg,name) 

pThen4 :: (a->b->c->d->e) -> Parser a->Parser b->Parser c->Parser d->Parser e
pThen4 combine p1 p2 p3 p4 toks = [(combine v1 v2 v3 v4, toks3) | (v1,toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3 ,toks3 )<- p3 toks2, (v4 , toks4) <- p4 toks3]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p1 = pThen (:) p1 (pZeroOrMore p1) 

pEmpty :: a -> Parser a
pEmpty a = \tok -> [(a,tok)]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting