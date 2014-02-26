module ObjectCode where

data Instruction = Call String
                 | PushI Integer
                 | PushS String  -- push a symbol
                 | PopRet
                 | CallRet



