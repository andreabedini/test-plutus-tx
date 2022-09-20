{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

import PlutusTx
import PlutusTx.Builtins
import PlutusTx.Prelude
import Prettyprinter
import System.IO (IO, print)

integerOne :: CompiledCode Integer
{- 'compile' turns the 'TExpQ Integer' into a
  'TExpQ (CompiledCode Integer)' and the splice
  inserts it into the program. -}
integerOne =
  $$( compile
        {- The quote has type 'TExpQ Integer'.
          We always use unbounded integers in Plutus Core, so we have to pin
          down this numeric literal to an ``Integer`` rather than an ``Int``. -}
        [||(1 :: Integer)||]
    )

integerIdentity :: CompiledCode (Integer -> Integer)
integerIdentity = $$(compile [||\(x :: Integer) -> x||])

{- Functions which will be used in Plutus Tx programs should be marked
  with GHC’s 'INLINABLE' pragma. This is usually necessary for
  non-local functions to be usable in Plutus Tx blocks, as it instructs
  GHC to keep the information that the Plutus Tx compiler needs. While
  you may be able to get away with omitting it, it is good practice to
  always include it. -}
{-# INLINEABLE plusOne #-}
plusOne :: Integer -> Integer
{- 'addInteger' comes from 'PlutusTx.Builtins', and is
  mapped to the builtin integer addition function in Plutus Core. -}
plusOne x = x `addInteger` 1

{-# INLINEABLE myProgram #-}
myProgram :: Integer
myProgram =
  let -- Local functions do not need to be marked as 'INLINABLE'.
      plusOneLocal :: Integer -> Integer
      plusOneLocal x = x `addInteger` 1

      localTwo = plusOneLocal 1
      externalTwo = plusOne 1
   in localTwo `addInteger` externalTwo

functions :: CompiledCode Integer
functions = $$(compile [||myProgram||])

{- We’ve used the CK evaluator for Plutus Core to evaluate the program
  and check that the result was what we expected. -}

matchMaybe :: CompiledCode (Maybe Integer -> Integer)
matchMaybe =
  $$( compile
        [||
        \(x :: Maybe Integer) -> case x of
          Just n -> n
          Nothing -> 0
        ||]
    )

-- | Either a specific end date, or "never".
data EndDate = Fixed Integer | Never

-- | Check whether a given time is past the end date.
pastEnd :: CompiledCode (EndDate -> Integer -> Bool)
pastEnd =
  $$( compile
        [||
        \(end :: EndDate) (current :: Integer) -> case end of
          Fixed n -> n < current
          Never -> False
        ||]
    )

addOne :: CompiledCode (Integer -> Integer)
addOne = $$(compile [||\(x :: Integer) -> x `addInteger` 1||])

addOneToN :: Integer -> CompiledCode Integer
addOneToN n =
  addOne
    -- 'applyCode' applies one 'CompiledCode' to another.
    `applyCode`
    -- 'liftCode' lifts the argument 'n' into a
    -- 'CompiledCode Integer'.
    liftCode n

-- 'makeLift' generates instances of 'Lift' automatically.
makeLift ''EndDate

pastEndAt :: EndDate -> Integer -> CompiledCode Bool
pastEndAt end current =
  pastEnd
    `applyCode` liftCode end
    `applyCode` liftCode current

main :: IO ()
main = do
  print $ pretty $ getPlc integerOne
  -- (program 1.0.0
  --   (con 1)
  -- )
  print $ pretty $ getPlc integerIdentity
  -- (program 1.0.0
  --   (lam ds (con integer) ds)
  -- )
  -- FIXME print $ pretty $ unsafeEvaluateCk $ toTerm $ getPlc functions
  -- (con 4)
  print $ pretty $ getPlc addOne
  -- (program 1.0.0
  --   [
  --     (lam
  --       addInteger
  --       (fun (con integer) (fun (con integer) (con integer)))
  --       (lam ds (con integer) [ [ addInteger ds ] (con 1) ])
  --     )
  --     (lam
  --       arg
  --       (con integer)
  --       (lam arg (con integer) [ [ (builtin addInteger) arg ] arg ])
  --     )
  --   ]
  -- )
  let program1 = getPlc $ addOneToN 4
  print $ pretty program1

-- (program 1.0.0
--   [
--     [
--       (lam
--         addInteger
--         (fun (con integer) (fun (con integer) (con integer)))
--         (lam ds (con integer) [ [ addInteger ds ] (con 1) ])
--       )
--       (lam
--         arg
--         (con integer)
--         (lam arg (con integer) [ [ (builtin addInteger) arg ] arg ])
--       )
--     ]
--     (con 4)
--   ]
-- )
-- print $ pretty $ unsafeEvaluateCk $ toTerm program1
-- (con 5)
-- FIXME let program2 = getPlc $ pastEndAt Never 5
-- FIXME print $ pretty $ unsafeEvaluateCk $ toTerm program2
-- (abs
--   out_Bool (type) (lam case_True out_Bool (lam case_False out_Bool case_False))
-- )
