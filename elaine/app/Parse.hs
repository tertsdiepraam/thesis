{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse (parseFile, parseString) where

import AST
import Data.Text hiding (empty)
import Data.Void
import Pretty (pretty)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseFile :: FilePath -> IO ()
parseFile filename = do
  contents <- readFile filename
  parseString filename (pack contents)

parseString :: String -> Text -> IO ()
parseString name s = do
  let result = parse program name s
  case result of
    Left err -> print err
    Right decs -> putStrLn $ pretty decs

-- WHITESPACE & BASICS
space' :: Parser ()
space' =
  L.space
    space1
    (L.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space'

symbol :: Text -> Parser Text
symbol = L.symbol space'

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- angles :: Parser a -> Parser a
-- angles = between (symbol "<") (symbol ">")

-- brackets :: Parser a -> Parser a
-- brackets = between (symbol "[") (symbol "]")

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

-- dot = symbol "."


-- Identifiers can contain numbers, but may not start with them
-- They may contain underscores, lowercase letters and uppercase letters
-- They may end with ticks, like in Haskell
-- They match the regex [a-zA-Z_][a-zA-Z_0-9]*'*
firstIdentChars :: [Char]
firstIdentChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']

otherIdentChars :: [Char]
otherIdentChars = firstIdentChars ++ ['0' .. '9']

ident :: Parser String
ident = lexeme $ do
  first <- oneOf firstIdentChars
  rest <- many (oneOf otherIdentChars)
  ticks <- many (char '\'')
  return $ [first] ++ rest ++ ticks

-- PROGRAM
-- A program is simply a sequence of modules, but the parser requires parsing
-- the entire source code.
program :: Parser [Module]
program = space' *> many mod' <* eof

mod' :: Parser Module
mod' = do
  name <- symbol "mod" >> ident
  decs <- braces (many declaration)
  return $ Mod name decs

-- DECLARATIONS
declaration :: Parser Declaration
declaration =
  try import'
    <|> try decFun
    <|> try decType
    <|> try decEffect
    <|> try handler
    <|> elaboration

import' :: Parser Declaration
import' = Import <$> (symbol "import" >> ident)

decFun :: Parser Declaration
decFun = symbol "fun" >> Fun <$> function

function :: Parser Function
function = do
  (name, p, ret) <- funSig
  Function name p ret <$> braces do'

handler :: Parser Declaration
handler = do
  name <- symbol "handler" >> ident
  Handler name <$> braces (many function)

elaboration :: Parser Declaration
elaboration = do
  name <- symbol "elaboration" >> ident
  Elaboration name <$> braces (many function)

algEff :: Parser EffectType
algEff = Algebraic <$> (symbol "!" >> ident)

highEff :: Parser EffectType
highEff = HigherOrder <$> (symbol "!!" >> ident)

effect :: Parser EffectType
effect = try highEff <|> algEff

decEffect :: Parser Declaration
decEffect = do
  name <- symbol "effect" >> effect
  Effect name <$> braces (many operation)

operation :: Parser Operation
operation = do
  (name, p, ret) <- funSig
  return $ Operation name p ret

-- Function signature
funSig :: Parser (Ident, [(Ident, Type)], Type)
funSig = do
  name <- ident
  p <- params
  ret <- colon >> type'
  return (name, p, ret)

decType :: Parser Declaration
decType = do
  name <- symbol "type" >> ident
  TypeDec name <$> braces (many constructor)

params :: Parser [(Ident, Type)]
params = parens (param `sepBy` comma)

constructor :: Parser Constructor
constructor = do
  name <- ident
  Constructor name <$> params

param :: Parser (Ident, Type)
param = do
  name <- ident
  typ <- colon >> type'
  return (name, typ)

type' :: Parser Type
type' = do
  name <- ident
  Type name <$> many effect

-- STATEMENTS
do' :: Parser Do
do' = try let' <|> (Pure <$> expr)

let' :: Parser Do
let' = do
  x <- ident
  e <- symbol "<-" >> expr
  d <- semicolon >> do'
  return $ Do (Let x e) d

-- EXPRESSIONS
expr :: Parser Expr
expr = try app <|> try match' <|> (Leaf <$> leaf)

leaf :: Parser Leaf
leaf = (Lit <$> lit) <|> (Var <$> ident)

app :: Parser Expr
app = do
  name <- ident
  arg <- parens (leaf `sepBy` comma)
  return $ App name arg

match' :: Parser Expr
match' = do
  l <- symbol "match" >> leaf
  Match l <$> braces (many matchArm)

matchArm :: Parser MatchArm
matchArm = do
  name <- ident
  p <- parens (ident `sepBy` comma)
  e <- symbol "=>" >> expr
  return $ MatchArm (Pattern name p) e

-- -- Literals
lit :: Parser Lit
lit = (Int <$> intLiteral) <|> (String <$> stringLiteral) <|> (Bool <$> boolLiteral)

intLiteral :: Parser Int
intLiteral = lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

boolLiteral :: Parser Bool
boolLiteral = True <$ symbol "true" <|> False <$ symbol "false"
