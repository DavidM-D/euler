{-# LANGUAGE OverloadedLists #-}
module Problems
    (
      q24
    , q25
    , q53
    ) where

import Data.List
import ListSetOps
import Data.Vector as V (scanl1', (!), cons)

-- this is clearly cheating
-- q24=concatMap show$(!!999999)$sort$permutations[0..9]

-- takes about 15 minutes to run on my machine, also warms up my room pretty well
-- there's much better ways to do this by representing the value as a sum of it's factorials and rotating the list
-- based upon those values but it's not shorter
q24=(!!999999)$filter hasNoDups$map((\s->if length s==10 then s else '0':s).show)[123456789..9876543210]

q25=let f a b i|b>=(10^999::Integer)=i|True=f b(a+b)(i+1)in f 1 1 2

--  you can save the odd character with
--  let f x|x==1=1|True=x*(f (x-1))
q53=let f=cons 0$V.scanl1'(*)[1..100]in length$filter(>1e6)$[f!n/(f!r*(f!(n-r)))|n<-[1..100],r<-[1..n-1]]
