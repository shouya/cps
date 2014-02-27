
import CPS
import Lisp
import ObjectCode

{-
lispcode = makeList [ Symbol "a"
                    , Symbol "b"
                    , makeList [Symbol "c", Symbol "d"]
                    , makeList [Symbol "e"]
                    ]
-}
lispcode = makeList [ Symbol "a"
                    , Symbol "b" ]


main = do
  putStrLn $ printInstructions $ stackTransform lispcode
