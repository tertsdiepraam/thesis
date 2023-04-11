{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Elaine.Parse (parseFile, parseString, parseProgram, parseExpr, prettyError, ParseResult) where

import Data.Either (partitionEithers)
import Data.Text (Text, pack, unpack)
import Data.Void
import Elaine.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type ParseResult a = Either (ParseErrorBundle Text Void) a

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

-- This mostly exists for testing
parseExpr :: String -> ParseResult Expr
parseExpr s = parse expr s (pack s)

-- For testing purposes
parseProgram :: String -> ParseResult [Module]
parseProgram s = parse program s (pack s)

prettyError :: ParseResult a -> Either String a
prettyError (Left a) = Left $ errorBundlePretty a
prettyError (Right x) = Right x

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

delimiters :: Text -> Text -> Parser a -> Parser a
delimiters left right = between (symbol left) (symbol right)

parens :: Parser a -> Parser a
parens = delimiters "(" ")"

braces :: Parser a -> Parser a
braces = delimiters "{" "}"

angles :: Parser a -> Parser a
angles = delimiters "<" ">"

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

equals :: Parser Text
equals = symbol "="

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
  exc <- option "" (symbol "!")
  return $ [first] ++ rest ++ ticks ++ unpack exc

-- PROGRAM
-- A program is simply a sequence of modules, but the parser requires parsing
-- the entire source code.
program :: Parser [Module]
program = space' *> many mod' <* eof

mod' :: Parser Module
mod' = do
  name <- keyword "mod" >> ident
  decs <- braces (many declaration)
  return $ Mod name decs

-- DECLARATIONS
declaration :: Parser Declaration
declaration = do
  vis <- visibility
  Declaration vis
    <$> ( decImport
            <|> decVal
            <|> decType
            <|> decEffect
            <|> decElaboration
        )

visibility :: Parser Visibility
visibility = Public <$ try (keyword "pub") <|> return Private

decImport :: Parser DeclarationType
decImport = Import <$> try (keyword "import" >> ident)

decElaboration :: Parser DeclarationType
decElaboration = do
  from <- try (keyword "elaboration") >> ident
  to <- symbol "->" >> effectRow
  DecElaboration . Elaboration from to <$> braces (many operationClause)

decVal :: Parser DeclarationType
decVal = do
  name <- try (keyword "let") >> ident
  DecLet name <$> (equals >> expr)

function :: Parser Function
function = Function <$> functionParams <*> computationType <*> expr

handler :: Parser Expr
handler = do
  arms <- try (keyword "handler") >> braces (many handlerArm)
  let (rets, functions) = partitionEithers arms
      ret = case rets of
        [r] -> r
        [] -> error "Handler must have a return arm"
        _ -> error "Handler cannot have multiple return arms"
  return $ Val $ Hdl $ Handler ret functions

handlerArm :: Parser (Either HandleReturn OperationClause)
handlerArm =
  (Left <$> handleReturn)
    <|> (Right <$> operationClause)

handleReturn :: Parser HandleReturn
handleReturn = do
  var <- try (keyword "return") >> parens ident
  HandleReturn var <$> expr

operationClause :: Parser OperationClause
operationClause = OperationClause <$> ident <*> parens (ident `sepBy` comma) <*> expr

decEffect :: Parser DeclarationType
decEffect = do
  name <- try (keyword "effect") >> ident
  DecEffect name <$> braces (many operationSignature)

operationSignature :: Parser OperationSignature
operationSignature = OperationSignature <$> ident <*> parens (valueType `sepBy` comma) <*> valueType

decType :: Parser DeclarationType
decType = do
  name <- try (keyword "type") >> ident
  DecType name <$> braces (many constructor)

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
  effs <- effectRow
  v <- valueType
  return $ ComputationType v effs

valueType :: Parser ValueType
valueType =
  try (parens $ pure UnitType)
    <|> try (parens valueType)
    <|> (ValueFunctionType <$> functionType)
    <|> TypeName <$> ident

functionType :: Parser FunctionType
functionType = try (keyword "fn") >> FunctionType <$> parens (computationType `sepBy` comma) <*> (colon >> computationType)

effectRow :: Parser EffectRow
effectRow =
  angles $ do
    effects <- ident `sepBy` comma
    extend <- (Extend <$> (symbol "|" >> ident)) <|> return Empty
    return $ foldr Cons extend effects

-- EXPRESSIONS
expr :: Parser Expr
expr =
  braces expr
    <|> (Fn <$> functionLiteral)
    <|> Val <$> value
    <|> let'
    <|> if'
    <|> match'
    <|> handle
    <|> elab
    <|> handler
    <|> try app
    <|> (Var <$> ident)

value :: Parser Value
value =
  (Int <$> intLiteral)
    <|> (String <$> stringLiteral)
    <|> (Bool <$> boolLiteral)
    <|> (Unit <$ unitLiteral)

let' :: Parser Expr
let' = do
  x <- try (keyword "let") >> ident
  e1 <- equals >> expr
  Let x e1 <$> expr

if' :: Parser Expr
if' = do
  cond <- try (keyword "if") >> expr
  e1 <- keyword "then" >> expr
  e2 <- keyword "else" >> expr
  return $ If cond e1 e2

handle :: Parser Expr
handle = try (keyword "handle") >> Handle <$> expr <*> expr

elab :: Parser Expr
elab = try (keyword "elab") >> Elab <$> expr

app :: Parser Expr
app = do
  name <- ident
  arg <- parens (expr `sepBy` comma)
  return $ App (Var name) arg

match' :: Parser Expr
match' = do
  l <- try (keyword "match") >> expr
  Match l <$> braces (many matchArm)

matchArm :: Parser MatchArm
matchArm = do
  name <- ident
  p <- parens (ident `sepBy` comma)
  e <- symbol "=>" >> expr
  return $ MatchArm (Pattern name p) e

-- Literals
intLiteral :: Parser Int
intLiteral = lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"') <* space

boolLiteral :: Parser Bool
boolLiteral = True <$ try (keyword "true") <|> False <$ try (keyword "false")

unitLiteral :: Parser ()
unitLiteral = parens $ pure ()

functionLiteral :: Parser Function
functionLiteral = try (keyword "fn") >> function
