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

parseFile :: FilePath -> IO (Maybe Program)
parseFile filename = do
  contents <- readFile filename
  parseString filename (pack contents)

parseString :: String -> Text -> IO (Maybe Program)
parseString name s = do
  let result = parse program name s
  case result of
    Left bundle -> putStr (errorBundlePretty bundle) >> return Nothing
    Right decs -> return $ Just decs

-- WHITESPACE & BASIC SYMBOLS
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

keyword :: Text -> Parser Text
keyword kw = lexeme (string kw <* notFollowedBy (oneOf otherIdentChars))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- angles :: Parser a -> Parser a
-- angles = between (symbol "<") (symbol ">")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

-- dot = symbol "."

-- IDENTIFIERS
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
  import'
    <|> decFun
    <|> decType
    <|> decEffect
    <|> handler
    <|> elaboration

import' :: Parser Declaration
import' = Import <$> (try (keyword "import") >> ident)

decFun :: Parser Declaration
decFun = try (keyword "fn") >> DecFun <$> function

function :: Parser Function
function = Function <$> ident <*> funSig <*> braces do'

handler :: Parser Declaration
handler = do
  name <- try (keyword "handler") >> ident
  t <- colon >> handlerType
  DecHandler . Handler name t <$> braces (many function)

elaboration :: Parser Declaration
elaboration = do
  name <- try (keyword "elaboration") >> ident
  t <- colon >> handlerType
  DecElaboration . Elaboration name t <$> braces (many function)

algEff :: Parser Effect
algEff = Algebraic <$> (symbol "!" >> ident)

highEff :: Parser Effect
highEff = HigherOrder <$> (symbol "!!" >> ident)

effect :: Parser Effect
effect = try highEff <|> algEff

decEffect :: Parser Declaration
decEffect = do
  name <- try (keyword "effect") >> effect
  DecEffect name <$> braces (many operation)

operation :: Parser Operation
operation = Operation <$> ident <*> funSig

-- Function signature
funSig :: Parser FunSig
funSig = FunSig <$> typeParams <*> functionParams <*> (colon >> computationType)

decType :: Parser Declaration
decType = do
  name <- try (keyword "type") >> ident
  tps <- typeParams
  DecType name tps <$> braces (many constructor)

functionParams :: Parser [(Ident, ComputationType)]
functionParams = parens (functionParam `sepBy` comma)

constructor :: Parser Constructor
constructor = Constructor <$> ident <*> parens (computationType `sepBy` comma)

functionParam :: Parser (Ident, ComputationType)
functionParam = do
  name <- ident
  typ <- colon >> computationType
  return (name, typ)

computationType :: Parser ComputationType
computationType = do
  v <- valueType
  ComputationType v <$> effectRow

valueType :: Parser ValueType
valueType =
  try (parens valueType)
    <|> (ValueFunctionType <$> functionType)
    <|> TypeName <$> ident <*> typeParams

functionType :: Parser FunctionType
functionType = try (keyword "fn") >> FunctionType <$> typeParams <*> parens (computationType `sepBy` comma) <*> (colon >> computationType)

handlerType :: Parser HandlerType
handlerType = HandlerType <$> try computationType <*> (try (symbol "->") >> computationType)

effectRow :: Parser [Effect]
effectRow = many effect

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
  l <- keyword "match" >> leaf
  Match l <$> braces (many matchArm)

matchArm :: Parser MatchArm
matchArm = do
  name <- ident
  p <- parens (ident `sepBy` comma)
  e <- symbol "=>" >> expr
  return $ MatchArm (Pattern name p) e

typeParams :: Parser TypeParams
typeParams = TypeParams <$> option [] (try (brackets (ident `sepBy` comma)))

-- Literals
lit :: Parser Lit
lit = (Int <$> intLiteral) <|> (String <$> stringLiteral) <|> (Bool <$> boolLiteral)

intLiteral :: Parser Int
intLiteral = lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

boolLiteral :: Parser Bool
boolLiteral = True <$ keyword "true" <|> False <$ keyword "false"
