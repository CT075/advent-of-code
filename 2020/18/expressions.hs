import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = Const Integer
  | Add Expr Expr
  | Mult Expr Expr
  deriving (Show)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

number :: Parser Expr
number = Const <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

expr :: Parser Expr
expr = makeExprParser term operators

term :: Parser Expr
term = choice [parens expr, number]

binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

operators :: [[Operator Parser Expr]]
operators =
  [ [ binary "*" Mult,
      binary "+" Add
    ]
  ]

term2 :: Parser Expr
term2 = choice [parens expr2, number]

expr2 :: Parser Expr
expr2 = makeExprParser term2 operators2

operators2 :: [[Operator Parser Expr]]
operators2 =
  [ [binary "+" Add],
    [binary "*" Mult]
  ]

eval :: Expr -> Integer
eval (Const i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mult e1 e2) = (eval e1) * (eval e2)

unwrap (Right a) = a
unwrap (Left _) = undefined

solveOne :: IO ()
solveOne = do
  input <- readFile "input.txt"
  print $ sum $ map (eval . unwrap . parse expr "expression") $ lines input
  return ()

solveTwo :: IO ()
solveTwo = do
  input <- readFile "input.txt"
  print $ sum $ map (eval . unwrap . parse expr2 "expression") $ lines input
  return ()
