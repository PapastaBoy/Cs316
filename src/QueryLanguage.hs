module QueryLanguage where

import JSONTransformer

data Query
  = Pipe        Query Query
  | Field       String
  | Elements
  | Select      Query
  | ConstInt    Int
  | ConstString String
  | Equal       Query Query
  deriving Show

-- | Executes a 'Query' by translating it into a `Transformer`. Each
-- of the constructors of 'query' is turned into its corresponding
-- `Transformer` defined in `JSONTransformer`.
--
-- For example:
--
-- >  execute Elements (Array [Number 1, Number 2])
--
-- returns
--
-- >  [Number 1, Number 2]
--
-- which is the behaviour of `elements` on this input.
execute :: Query -> Transformer
execute (ConstString s) = string s
execute (ConstInt i) = int i
execute (Elements) = elements
execute (Field s) = field s
execute (Pipe q1 q2) = pipe (execute q1) (execute q2)
execute (Equal q1 q2) = equal (execute q1) (execute q2)
execute (Select q1) = select (execute q1)


-- HINT: this function is very similar to the 'eval' function for
-- evaluating Boolean formulas defined in the Week03 problems.
