rib p = reverse . dropWhile p . reverse . dropWhile p

{-|
Prelude> :t rib
rib :: (a -> Bool) -> [a] -> [a]
Prelude> rib (==1) [1,1,3,4,1,4,3,1,1]
[3,4,1,4,3]
Prelude> rib (=='x') "xxgenerix_oneliner_inefficient?xx"
"generix_oneliner_inefficient?"
-}
