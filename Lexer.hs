--lexer

clex ::  String -> Int -> [(Token,Int)]

type Token = String -- Never Empty

clex (c:cs) ln                                --ln is the line number
 | isSpace c = clex cs ln
 | isNewLine c= clex cs (ln+1)
 | isDigit c = (num_token,ln) : clex rest_cs ln
 where
   num_token = c : takeWhile isDigit cs
   rest_cs = dropWhile isDigit cs
clex (c:cs) ln | isAlpha c = (var_tok,ln) : clex rest_cs ln
 where
   var_tok = c : takeWhile isIdChar cs
   rest_cs = dropWhile isIdChar cs

clex (c:cs) ln = ([c],ln) : clex cs ln
clex [] ln = []

isIdChar,isNewLine :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')
isNewLine c = c == '\n'

twoCharOps :: [String]
twoCharOps = ["==", "!=", ">=", "<=", "->"]

--Parser
type Parser a = [Token] -> [(a, [Token])]

--pLit :: String -> Parser String
--pLit s (tok:toks) = if (s==tok) then [(s, toks)] else []
--pLit s [] = []

--pVar:: Parser String
--pVar (tok:toks) = [(tok,toks)]
--pVar [] = []

pAlt :: Parser a -> Parser a -> Parser a   --To combine Parsers
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks = [ (combine v1 v2, toks2) | (v1,toks1) <- p1 toks, (v2,toks2) <- p2 toks1]

pGreeting :: Parser (String, String)
pGreeting = pThen3 mk_greeting pHelloOrGoodbye pVar (pLit "!")
    where mk_greeting hg name exclamation = (hg, name)

pThen3 :: (a->b->c->d)-> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks = [(combine v1 v2 v3, toks3) | (v1,toks1)<- p1 toks, (v2,toks2)<- p2 toks1, (v3,toks3)<- p3 toks2]

pThen4 :: (a->b->c->d->e)-> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks = [(combine v1 v2 v3 v4, toks3) | (v1,toks1)<- p1 toks, (v2,toks2)<- p2 toks1, (v3,toks3)<- p3 toks2, (v4,toks4)<-p4 toks3]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pEmpty :: a -> Parser a
pEmpty a = \tok -> [(a,tok)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p1 = pThen (:) p1 (pZeroOrMore p1)

--[a,[Token]]  [[a],[Token]] combine a [a] = a:[a]
--combine :: a->[a]->[a]
-- elem : list  (:) elem List

pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting

pApply :: Parser a ->(a->b)->Parser b
pApply p1 fn toks = [(fn v1,toks1)|(v1,toks1)<-p1 toks]

pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen3 combine_pOneOrMoreWithSep p1 p2 (pZeroOrMoreWithSep p1 p2) where
									combine_pOneOrMoreWithSep p1_a p2_b p3_list_a = p1_a:p3_list_a
pZeroOrMoreWithSep :: Parser a->Parser b->Parser [a]
pZeroOrMoreWithSep p1 p2 = (pOneOrMoreWithSep p1 p2) `pAlt` (pEmpty [])

pSat :: (String -> Bool) -> Parser String
pSat fn [] = []
pSat fn (tok:toks) = if(fn tok) then [(tok,toks)] else []

pLit :: String -> Parser String
pLit s = pSat (==s)

keywords :: [String]
keywords = ["let","letrec","case","in","of","Pack"]

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
take_first_parse other = error "Syntax Error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr where
				mk_sc name list_names equalTo expr = (name,list_names,expr)

-- Production rules:
--apexpr -> Num | Var
--binop -> + | - | * | /
--expr -> arithexpr1 | let defn in expr
--defn -> var = expr
--defns -> defn1;defn2; ....;defnn
-- expr1 -> expr2 + expr 1 | expr2 - expr 1 | expr2
-- expr2 -> expr3 * expr 2 | expr3 / expr 2 | expr3
-- expr3 -> apexpr
--Defining each in order

pAexpr :: Parser CoreExpr
pAexpr = (pVar `pApply` var_EVar) `pAlt` (pNum `pApply` num_Enum) where
var_EVar str = EVar str
num_Enum num = ENum num

pBinop :: Parser String
pBinop = (pLit "+") `pAlt` (pLit "-") `pAlt` (pLit "*") `pAlt` (pLit "/")

pExpr :: Parser CoreExpr
pExpr = pExprArith1 `pAlt`
  pThen4 foo2 (pLit "let") (pDefns) (pLit "in") (pExpr) where
    foo2 p1_let p2_defns p3_in p4_expr = ELet nonRecursive p2_defns p4_expr

pDefn :: Parser (String,Expr Name)
pDefn = pThen3 foo3 (pVar) (pLit "=") (pExpr) where
   foo3 name equalTo expr = (name, expr)

pDefns :: Parser [(String, Expr Name)]
pDefns = pOneOrMore pDefn

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

