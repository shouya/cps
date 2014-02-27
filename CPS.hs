module CPS where

import Lisp
import ObjectCode

prepareArguments :: Elem -> [Instruction]
prepareArguments (Cons z zs) = stackTransform z ++ prepareArguments zs
prepareArguments Null        = []
prepareArguments _ = error "x"

stackTransform :: Elem -> [Instruction]
stackTransform Null = []
stackTransform (Symbol x) = [PushS x]
stackTransform (Integer x) = [PushI x]
stackTransform (Cons (Cons x xs) ys) = stackTransform (Cons x xs) ++
                                       [PopRet] ++
                                       prepareArguments ys ++
                                       [CallRet]
stackTransfrom (Cons (Symbol x) xs) =
  prepareArguments xs ++ [Call x]
stackTransfrom (Cons x xs) = []
stackTransform _ = error "x"






{-
(a (b c) (d e) f):

push c
call b
push eax
push e
call d
push eax
push f
call a

((a b) c):

push b
call a
popret
push c
callret

-}
