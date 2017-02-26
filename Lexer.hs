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
