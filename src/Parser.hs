module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

binops = [[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

typeSpecifier :: Parser TypeSpecifier
typeSpecifier = choice [reserved "int" >> return Int
                       , reserved "void" >> return Void]

int :: Parser Expr
int = IntValue <$> integer

expr :: Parser Expr
expr = Ex.buildExpressionParser binops factor

variable :: Parser Expr
variable = do
  typ <- typeSpecifier
  var <- identifier
  return $ VarDeclaration typ var

function :: Parser Expr
function = do
  typ <- typeSpecifier
  name <- identifier
  args <- parens $ commaSep variable
  body <- braces $ semiSep expr
  return $ FunDeclaration typ name args body

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr
factor = try int
      <|> function
      <|> call
      <|> variable
      <|> parens expr

defn :: Parser Expr
defn = try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
