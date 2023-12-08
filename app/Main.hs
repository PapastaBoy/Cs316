module Main (main) where

import System.Environment (getArgs)

import JSON
import JSONInput
import JSONOutput
import ParserCombinators (runParser)
import QueryLanguage
import Result

query :: Query
query = Elements (Field "city" `Equal` ConstString "Seattle")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> processFile filename
    ["-c", jsonStr] -> processCommandLine jsonStr
    _ -> putStrLn "Usage: program filename OR program -c 'jsonString'"

processFile :: FilePath -> IO ()
processFile filename = do
  rawText <- readFile filename
  let inputJSON = abortOnError (stringToJSON rawText)
  processJSON inputJSON

processCommandLine :: String -> IO ()
processCommandLine jsonStr = do
  let inputJSON = abortOnError (stringToJSON jsonStr)
  processJSON inputJSON

processJSON :: IO JSON -> IO ()
processJSON jsonIO = do
  inputJSON <- jsonIO
  putStrLn (renderJSON inputJSON)

  -- Execute the query
  let queryResults = execute query inputJSON

  -- Print the result of the query
  putStrLn "Query Result:"
  case queryResults of
    Ok results -> mapM_ (putStrLn . renderJSON) results
    Error errorMsg -> putStrLn $ "Error: " ++ errorMsg

