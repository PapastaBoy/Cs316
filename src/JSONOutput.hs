module JSONOutput where

import JSON
import Data.List (intersperse)

-- | Converts a 'String' into its string JSON representation by
-- placing quotes around it and escaping any quote characters that
-- appear in the string.
--
-- For example,
--
-- >  renderString "hello \"world\""
--
-- Returns
--
-- >  "\"hello \\\"world\\\"\""
renderString :: String -> String
renderString str = "\"" ++ concatMap escaper str ++ "\""
  where
    escaper '"' = "\\\""
    escaper c = [c]

-- HINT: one way of doing the escaping is the same way that we did the
-- htmlEscape function in the Week01 problems.

-- | Converts `JSON` into its string representation, as documented in
-- the JSON standard (https://www.json.org/json-en.html). The format
-- should be as is understood by the parsing functions in `JSONInput`.
--
-- For example,
--
--  > renderJSON (Object [("a",Number 1), ("b",Number 2)])
--
-- should give
--
--  > "{\"a\": 1, \"b\": 2}"
renderJSON :: JSON -> String
renderJSON json = case json of
   String s   -> renderString s
   Boolean b  -> if b then "true" else "false"
   Null       -> "null"
   Number n   -> show n
   Array xs   -> "[" ++ renderArray xs ++ "]"
   Object fs  -> "{" ++ renderObject fs ++ "}"
   where
     renderArray :: [JSON] -> String
     renderArray = concat . intersperse ", " . map renderJSON

     renderObject :: [(String, JSON)] -> String
     renderObject concat . intersperse ", " . map renderField
       where
         renderField (key, val) = renderString key ++ ": " ++ renderJSON val

-- HINT: the `intersperse` function (imported above) is a good way of
-- putting something between every element of a list. It is the
-- "printing" counterpart of the 'sepBy' parser combinator.
--
-- You should use 'renderString' to implement rendering of 'String's
-- in the JSON.
