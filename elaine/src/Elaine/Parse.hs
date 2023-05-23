module Elaine.Parse (parseProgram, parseExpr, ParseResult, Spans, Span) where

import Control.Monad.State (State, evalState, get, put, runState)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Void
import Elaine.AST
import Text.Megaparsec
  ( MonadParsec (eof, notFollowedBy, try),
    ParseErrorBundle,
    ParsecT,
    between,
    empty,
    errorBundlePretty,
    getInput,
    getOffset,
    many,
    manyTill,
    oneOf,
    option,
    optional,
    runParserT,
    sepBy,
    (<|>),
  )
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (span)

data Span = Span Int Int String
  deriving (Show)

type Spans = [(Span, String)]

type Parser = ParsecT Void Text (State Spans)

type ParseResult a = Either (ParseErrorBundle Text Void) a

span :: String -> Parser a -> Parser a
span c p = do
  start <- getOffset
  input <- fmap unpack getInput
  x <- p
  end <- getOffset
  let length' = end - start
  currentList <- get
  () <- put (currentList ++ [(Span start end (take length' input), c)])
  return x

parseProgram :: (Text, Text) -> Either String ([Declaration], Spans)
parseProgram (name, s) =
  let parsed = runParserT program (unpack name) s
      afterState = runState parsed []
   in case afterState of
        (Left a, _) -> Left (errorBundlePretty a)
        (Right p, s') -> Right (p, s')

-- This mostly exists for testing
parseExpr :: String -> ParseResult Expr
parseExpr s =
  let parsed = runParserT expr s (pack s)
   in evalState parsed empty

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
keyword kw = span "keyword" $ lexeme (string kw <* notFollowedBy (oneOf otherIdentChars))

delimiters :: Text -> Text -> Parser a -> Parser a
delimiters left right = between (symbol left) (symbol right)

parens :: Parser a -> Parser a
parens = delimiters "(" ")"

braces :: Parser a -> Parser a
braces = delimiters "{" "}"

brackets :: Parser a -> Parser a
brackets = delimiters "[" "]"

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
ident = span "identifier" $ lexeme $ do
  firstChar <- oneOf firstIdentChars
  rest <- many (oneOf otherIdentChars)
  ticks <- many (char '\'')
  exc <- option "" (symbol "!")
  return $ [firstChar] ++ rest ++ ticks ++ unpack exc

-- PROGRAM
-- A program is simply a sequence of modules, but the parser requires parsing
-- the entire source code.
program :: Parser [Declaration]
program = space' *> many declaration <* eof

mod' :: Parser DeclarationType
mod' = do
  name <- keyword "mod" >> ident
  decs <- braces (many declaration)
  return $ Module name decs

-- DECLARATIONS
declaration :: Parser Declaration
declaration = do
  vis <- visibility
  Declaration vis
    <$> ( decUse
            <|> mod'
            <|> decVal
            <|> decType
            <|> decEffect
        )

visibility :: Parser Visibility
visibility = Public <$ try (keyword "pub") <|> return Private

decUse :: Parser DeclarationType
decUse = Use <$> try (keyword "use" >> ident <* semicolon)

elaboration :: Parser Expr
elaboration = do
  from <- try (keyword "elaboration") >> ident
  to <- symbol "->" >> row
  Val . Elb . Elaboration from to <$> braces (many operationClause)

decVal :: Parser DeclarationType
decVal = do
  name <- try (keyword "let") >> ident
  t <- optional (symbol ":" >> computationType)
  DecLet name t <$> (equals >> expr <* semicolon)

function :: Parser Function
function = Function <$> functionParams <*> optional computationType <*> exprBlock

handler :: Parser Expr
handler = do
  arms <- try (keyword "handler") >> braces (many handlerArm)
  let (rets, functions) = partitionEithers arms
      ret = case rets of
        [r] -> r
        [] -> error "Handler must have a return arm"
        _ -> error "Handler cannot have multiple return arms"
  return $ Val $ Hdl $ Handler ret functions

handlerArm :: Parser (Either Function OperationClause)
handlerArm =
  (Left <$> handleReturn)
    <|> (Right <$> operationClause)

handleReturn :: Parser Function
handleReturn = try (keyword "return") >> function

operationClause :: Parser OperationClause
operationClause = OperationClause <$> ident <*> parens (ident `sepBy` comma) <*> exprBlock

decEffect :: Parser DeclarationType
decEffect = do
  name <- try (keyword "effect") >> ident
  DecEffect name <$> braces (many operationSignature)

operationSignature :: Parser OperationSignature
operationSignature = OperationSignature <$> ident <*> parens (computationType `sepBy` comma) <*> computationType

decType :: Parser DeclarationType
decType = do
  name <- try (keyword "type") >> ident
  DecType name <$> braces (many constructor)

functionParams :: Parser [(Ident, Maybe ASTComputationType)]
functionParams = parens (functionParam `sepBy` comma)

constructor :: Parser Constructor
constructor = Constructor <$> ident <*> parens (computationType `sepBy` comma)

functionParam :: Parser (Ident, Maybe ASTComputationType)
functionParam = do
  name <- ident
  typ' <- optional (colon >> computationType)
  return (name, typ')

computationType :: Parser ASTComputationType
computationType = do
  effs <- optional row
  ASTComputationType (fromMaybe (Row [] Nothing) effs) <$> valueType

valueType :: Parser ASTValueType
valueType =
  try (parens $ pure TypeUnit)
    <|> functionType
    <|> (TypeName <$> ident)
    <|> try (parens valueType)

functionType :: Parser ASTValueType
functionType = try (keyword "fn") >> TypeArrow <$> parens (computationType `sepBy` comma) <*> computationType

row :: Parser Row
row =
  angles $ do
    effects <- ident `sepBy` comma
    extend <- optional (symbol "|" >> ident)
    return $ Row effects extend

-- EXPRESSIONS
data LetOrExpr = Let' String (Maybe ASTComputationType) Expr | Expr' Expr

exprBlock :: Parser Expr
exprBlock = do
  exprs <- braces (letOrExpr `sepBy` semicolon)
  case foldr f Nothing exprs of
    Just a -> return a
    Nothing -> return $ Val Unit
  where
    f (Expr' e1) Nothing = Just e1
    f (Let' {}) Nothing = error "last expression in a block cannot be let"
    f (Expr' e1) (Just e2) = Just $ Let "_" Nothing e1 e2
    f (Let' x t e1) (Just e2) = Just $ Let x t e1 e2

letOrExpr :: Parser LetOrExpr
letOrExpr = let' <|> Expr' <$> expr

expr :: Parser Expr
expr = do
  root <-
    exprBlock
      <|> if'
      <|> Val <$> value
      <|> match'
      <|> handle
      <|> elab
      <|> handler
      <|> elaboration
      <|> (Var <$> ident)
  applications <- many (parens (expr `sepBy` comma))
  return $ foldl App root applications

value :: Parser Value
value =
  (Int <$> intLiteral)
    <|> (String <$> stringLiteral)
    <|> (Bool <$> boolLiteral)
    <|> (Unit <$ unitLiteral)
    <|> (Fn <$> functionLiteral)

let' :: Parser LetOrExpr
let' = do
  x <- try (keyword "let") >> ident
  t <- optional (symbol ":" >> computationType)
  Let' x t <$> (equals >> expr)

if' :: Parser Expr
if' = do
  cond <- try (keyword "if") >> expr
  e1 <- exprBlock
  e2 <- keyword "else" >> exprBlock
  return $ If cond e1 e2

handle :: Parser Expr
handle = try (keyword "handle") >> Handle <$> brackets expr <*> expr

elab :: Parser Expr
elab = do
  _ <- try (keyword "elab")
  elaborationExpr <- optional (brackets expr)
  case elaborationExpr of
    Just a -> Elab a <$> expr
    Nothing -> do
      -- Implicit elabs need a unique identifier for the transformation
      -- The offset is unique, so might as well use that.
      off <- getOffset
      ImplicitElab off <$> expr

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
intLiteral = do
  negative <- optional (symbol "-")
  let mul = case negative of
        Just _ -> negate
        Nothing -> id
  mul <$> lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"') <* space

boolLiteral :: Parser Bool
boolLiteral = True <$ try (keyword "true") <|> False <$ try (keyword "false")

unitLiteral :: Parser ()
unitLiteral = parens $ pure ()

functionLiteral :: Parser Function
functionLiteral = try (keyword "fn") >> function
