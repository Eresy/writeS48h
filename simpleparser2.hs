module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- main function needed to call readExpr and print out results
main :: IO()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))


-- define a parser that recognizes one of the symbols allowed
symbol :: Parser Char --let ghc find this out
symbol = oneOf "!$%&|*+-/:<=?>#^_~#"

-- Defining a function to call our parser and handle errors:
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
-- this
--    input  = parameter name
--    symbol = action defined above
--    "lisp" = name of the parser
-- are passed to the plf parse.
-- parse can return either the parsed value or an error:

-- Defining a parser that recognizes any number of whitespace chars
spaces :: Parser ()
-- passing the Parser action space to the Parser action skipMany1
spaces = skipMany1 space


-- we need to define a data type to hold any lisp value
data LispVal = Atom String 
               | List [LispVal]]
               | DottedList [LispVal] LispVal
               | Number Integer
               | String String
               | Bool Bool
-- algebraic data type: it defines a set of possible
-- values that a variable of type List Val can hold.
parseString :: Parser LispVal
parseString = do 
                char ""
                x <- many (noneOf "\"")
                char ""
                return $ String x
-- We're back to using the do-notation instead of the >> operator.
-- This is because we'll be retrieving the value of our parse
-- (returned by many(noneOf "\"")) and manipulating it,
-- interleaving some other parse operations in the meantime.
--
-- In general, use 
--                 >> if the actions don't return a value,
--                 >>= if you'll be immediately passing
--                     that value into the next action,
--                 and do-notation otherwise.
-- Every constructor in an algebraic data type also acts like a function 
-- that turns its arguments into a value of its type.
-- It also serves as a pattern that can be used in the left-hand side of a pattern-matching expression;
-- Every constructor in an algebraic data type also acts like a function
-- that turns its arguments into a value of its type.
-- It also serves as a pattern that can be used in the left-hand
-- side of a pattern-matching expression;
-- we then apply the built-in function return to lift our LispVal into the Parser monad
