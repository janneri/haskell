score :: [Int] -> Int
score (x:y:[]) = x+y
score (x:y:z:[]) | x + y >= 10 = x + y + z
score (x:y:rest) | x+y == 10  = x + y + head rest + score rest
score (x:rest) | x == 10 = x + (sum $ take 2 rest) + score rest
score (x:y:rest) = x + y + score rest

testbasic = score $ take 20 [1,1..]
-- 11 + 18 = 29
testspare = score [1,9, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1]

-- 18 + 11 = 29
testlastframespare = score [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,9,1]


-- 12 + 18 = 30
teststrike = score [10, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1]
-- 18 + 12
testlastframestrike = score [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 10,1,1]
-- 18 + 12
testallstrikes = score $ take 12 [10,10..]
