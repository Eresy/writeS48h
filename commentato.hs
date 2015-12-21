module Main where
import System.Environment
-- a simple parser built upon the Parsec library
-- importing library function minus `spaces` function
-- that'll conflict with a function that we'll define later
import Text.ParserCombinators.Parsec hiding (spaces)

-- main function needed to call readExpr and print out results
-- main :: IO()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))


-- define a parser that recognizes one of the symbols allowed
-- in Scheme
symbol :: Parser Char --let ghc find this out
symbol = oneOf "!$%&|*+-/:<=?>#^_~#"
-- another example of monad:
-- the "extra info" being hidden is all the info about position 
-- in the input stream, backtracking record, first and follow sets,etc.
--
-- Parsec takes care of all of that for us, we just need the Parsec Library
-- function (from now on, plf) oneOf and it'll recognize a single one of any
-- of the chars in the string passed to it


-- Defining a function to call our parser and handle errors:
readExpr :: String -> String
readExpr input = case parse (space >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
-- this
--    input  = parameter name
--    symbol = action defined above
--    "lisp" = name of the parser
-- are passed to the plf parse.
-- parse can return either the parsed value or an error:
-- Parsec returns an Either data type, using Left constructor to indicate
-- an error and the Right one for a normal value, we use case...of for the pattern
-- matching.
--  (space >> symbol) explicity bind whitespace whitespace with symbol parsers.
--  in the Parser monad >> (bind) means:
--                                Attempt to match the first parser, then attempt
--                                to match the second with the remaining input,
--                                fails if either fails.
--      Observation:
--                  bind (>>) is intended as a general way to structure computations,
--                  and so needs to be general enough to accomodate all the different 
--                  types of computation ==> bind will have wildly different effects
--                  in different monads.

-- Defining a parser that recognizes any number of whitespace chars
spaces :: Parser ()
spaces = skipMany1 space
-- passing the Parser action space to the Parser action skipMany1
