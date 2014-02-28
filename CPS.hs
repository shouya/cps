module CPS where

import Lisp
import ObjectCode

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

stackTransform :: Elem -> [Instruction]
stackTransform Null = []
stackTransform (Symbol x) = [PushS x]
stackTransform (Integer x) = [PushI x]
stackTransform (Cons x xs) =
  case x of
    (Symbol v) ->
      prepareArguments xs ++ [Call v]
    (Cons v vs) ->
      stackTransform (Cons v vs) ++
      [PopRet] ++
      prepareArguments xs ++
      [CallRet]
    otherwise -> error "x"
  where prepareArguments (Cons z zs) =
          stackTransform z ++ prepareArguments zs
        prepareArguments Null        = []
        prepareArguments _           = error "x"




