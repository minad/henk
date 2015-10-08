module Henk.HenkParser where

import Text.Megaparsec
import Text.Megaparsec.Text.Lazy
import qualified Text.Megaparsec.Lexer as L
import Henk.HenkAS
import Henk.HenkPP()

integer :: Parser Integer
integer = lexeme L.integer

symbol :: String -> Parser String
symbol = L.symbol whiteSpace

semi :: Parser String
semi = symbol ";"

comma :: Parser String
comma = symbol ","

whiteSpace :: Parser ()
whiteSpace = L.space (() <$ spaceChar) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

braces, parens, lexeme :: Parser a -> Parser a
lexeme  = L.lexeme whiteSpace
braces = between (symbol "{") (symbol "}")
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = do
  n <- lexeme $ try $ (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> oneOf "'_?")
  if elem n [ "case", "data", "letrec", "type", "import", "in", "let", "of", "at", "Int"]
    then fail $ "reserved keyword " ++ n
    else return n

reserved :: String -> Parser String
reserved s = lexeme $ try $ string s

----------------------------------------------------------------
-- The Program
----------------------------------------------------------------
program :: Parser Program
program =  do{whiteSpace
             ;(tds,vds) <- manyAlternate tDecl vDecl
             ;eof
             ;return $ Program tds vds
             }

manyAlternate :: Parser a -> Parser b -> Parser ([a],[b])
manyAlternate pa pb = do{as<-some pa; (as',bs') <- manyAlternate pa pb; return (as++as',bs')}
                      <|>
                      do{bs<-some pb; (as',bs') <- manyAlternate pa pb; return (as',bs++bs')}
                      <|>
                      return ([],[])

----------------------------------------------------------------
-- Type Declaration
----------------------------------------------------------------
tDecl :: Parser TDecl
tDecl =  do{reserved "data"
           ;t <- bindVar
           ;symbol "="
           ;ts <- braces $ sepBy1 bindVar semi
           ;return $ TDecl t ts
           }
           <?> "type declaration"

----------------------------------------------------------------
-- Value Declaration
----------------------------------------------------------------
vDecl :: Parser VDecl
vDecl  = letnonrec <?> "value Declaration"

letnonrec :: Parser VDecl
letnonrec  = do{reserved "let"
               ;tv <- bindVar'
               ;symbol "="
               ;ex <- expr
               ;return $ VDecl tv ex
               }


----------------------------------------------------------------
-- Expression
----------------------------------------------------------------
expr :: Parser Expr
expr = choice
     [piExpr    --pi (\/) before lambda (\) to improve parser efficiency.
     ,lamExpr
     ,caseExpr
     ,funExpr
     ]
     <?> "expression"


atomExpr :: Parser Expr
atomExpr = choice
            [try varExpr
            ,litExpr
            ,sort
            ,unknown
            ,parens expr
            ]
            <?> "atomic expression"

--single expression
single_expr :: Parser Expr
single_expr =do { whiteSpace
                ; ex <- expr
                ; return ex
                }

-----------------------------------------------------------------
-- Application
-----------------------------------------------------------------
appExpr :: Parser Expr
appExpr = do{atoms <- some atomExpr;
             return $  foldl1 AppExpr atoms}
          <?> "application"

----------------------------------------------------------------
-- (Capital) Lambda Expression
----------------------------------------------------------------
lamExpr :: Parser Expr
lamExpr =  do{symbol "\\" <|> symbol "/\\"
            ;tvs <- sepBy1 bindVar comma
            ;symbol "."
            ;e <- expr
            ;return $ foldr LamExpr e tvs}
            <?> "lambda expression"

----------------------------------------------------------------
-- Pi Expression / ForAll Expression
----------------------------------------------------------------
piExpr :: Parser Expr
piExpr  = do{ (symbol "|~|") <|> try (symbol ("\\/"))
          ;tvs <- sepBy1 bindVar comma
          ;symbol "."
          ;e <- expr
          ;return $ foldr PiExpr e tvs}
          <?> "pi expression"


----------------------------------------------------------------
-- Function Expression
----------------------------------------------------------------
funExpr :: Parser Expr
funExpr = chainr1 appExpr arrow
 where
 arrow = do{symbol "->"; return $ \ex1 ex2 -> PiExpr (TVar Anonymous ex1) ex2}



----------------------------------------------------------------
-- Case Expression
----------------------------------------------------------------
caseExpr :: Parser Expr
caseExpr = do{reserved "case"
             ;ex  <- expr
             ;reserved "of"
             ;as  <- braces $ sepBy1 alt semi
             ;case_type <- option Unknown (do{reserved ":"; case_type <- expr ; return case_type})
             ;return $ CaseExpr ex as case_type
             }
             <?> "Case Expression"

alt :: Parser Alt
alt = do{tc   <- boundVar
        ;tcas <- many var
	;tcas <- return $ map (\v -> TVar v Unknown) tcas
        ;symbol "=>"
        ;res <- expr
        ;return $ Alt tc tcas [] res
        }
        <?> "case alternative"

----------------------------------------------------------------
-- Variable Expression
----------------------------------------------------------------
varExpr = do{tv <- boundVar
            ;return $ VarExpr tv
            }
            <?> "variable expression"


----------------------------------------------------------------
-- Variable
----------------------------------------------------------------
var :: Parser Var
var = do{v <- identifier
        ;return $ Var v
        }

anonymousvar :: Parser Var
anonymousvar =
      do{symbol "_"
        ;v <- option "" identifier
        ;return $ Var ('_':v)
        }

----------------------------------------------------------------
-- Binding Variable
----------------------------------------------------------------
bindVar :: Parser TVar
bindVar = do{v <- (anonymousvar <|> var)
          ;(do {e <- isOfType
               ; return $ TVar v e
               }
            <|>
            (return $ TVar v (SortExpr Star)))      --  convention for binding variables
          }
          <?> "variable"

bindVar' :: Parser TVar
bindVar' = do{v <- (anonymousvar <|> var)
             ;(do {e <- isOfType
                  ; return $ TVar v e
                  }
                  <|>
                 (return $ TVar v Unknown))      --  convention for lets
             }
             <?> "variable"

isOfType :: Parser Expr
isOfType =  do{symbol ":"
              ;aex <- expr
              ;return aex}

----------------------------------------------------------------
-- Bound Variable
----------------------------------------------------------------
boundVar :: Parser TVar
boundVar =  do{v <- var
              ;(do {e <- isOfType
                 ;return $ TVar v e
                 }
                 <|>
              (return $ TVar v Unknown))      --  convention for bound variables
              }
              <?> "variable"

----------------------------------------------------------------
-- Literal Expression
----------------------------------------------------------------
litExpr :: Parser Expr
litExpr = do {l <- lit
             ;return $ LitExpr l
             }
             <?> "literal expression"

----------------------------------------------------------------
-- Literal
----------------------------------------------------------------
lit :: Parser Lit
lit = do{i <- integer
        ;return $ LitInt i
        }
      <|>
      do{reserved "Int"
        ;return $ IntType
        }

----------------------------------------------------------------
-- Sort
----------------------------------------------------------------
sort :: Parser Expr
sort = do{s <-    try (sortNum)
              <|> star
              <|> box
     ;return $ SortExpr s}

sortNum :: Parser Sort
sortNum = do{ symbol "*"
            ; n <- integer
            ; return $ SortNum n
            }


star :: Parser Sort
star = do{ symbol "*"
         ; return Star
         }


box  :: Parser Sort
box  = do{ symbol "||"
         ; return Box
         }

----------------------------------------------------------------
-- Unknown
----------------------------------------------------------------
unknown  :: Parser Expr
unknown  = do{ symbol "?"
             ; return Unknown
             }
