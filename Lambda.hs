module Lambda where

data Expr a = Var { getVar :: String }
            | Lamb { getPar :: String
                   , getBody :: a }
            | App { getAppA :: a
                  , getAppB :: a }
          deriving (Eq)

instance Show (Expr Expr) where
  show (Var x) = x
  show (Lamb a b) = case b of 
    (App _ _) -> "\\" ++ a ++ "(" ++ show b ++ ")"
    _         -> "\\" ++ a ++ show b
  show (App a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  
