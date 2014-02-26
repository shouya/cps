
import Lisp
import ObjectCode

stackTransform :: Elem -> [Instruction]
stackTransform (Symbol x) = [PushS x]
stackTransform (Integer x) = [PushI x]
stackTransform (Cons (Cons x xs) ys) =
  (stackTransform (Cons x xs)) ++ [PopRet] ++
  prepareArguments ys ++ [CallRet]



{-
(a (b c) (d e) f) <=>

push c
call b
push eax
push e
call d
push eax
push f
call a

((a b) c)
push b
call a
popret
callret

-}


