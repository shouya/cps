module ObjectCode where
import Data.List

data Instruction = Call String
                 | PushI Integer
                 | PushS String  -- push a symbol
                 | PopRet
                 | CallRet
                 deriving (Show)

printInstructions :: [Instruction] -> String
printInstructions = intercalate "\n" . map show
