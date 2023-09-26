module Exercise4 where

import Test.QuickCheck
import LTS

-- Not sure about the type signature
infix 1 `after`
after :: IOLTS -> IOLTS -> IOLTS
after = undefined

main :: IO ()
main = do 
  print "Hello World"