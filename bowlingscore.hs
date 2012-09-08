score :: [Int] -> Int
score (x:y:[]) = x+y
score (x:y:z:[]) | x + y >= 10 = x + y + z
score (x:y:rest) | x+y == 10  = x + y + head rest + score rest
score (x:rest) | x == 10 = x + (sum $ take 2 rest) + score rest
score (x:y:rest) = x + y + score rest

testbasic = (2*10, score $ take 20 [1,1..])
testspare = (11+18, score [1,9, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1])
testlastframespare = (18+11, score [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,9,1])
teststrike = (12+18, score [10, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1])
testlastframestrike = (18+12, score [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 10,1,1])
testallstrikes = (300, score $ take 12 [10,10..])

allresults = [testbasic, testspare, testlastframespare, teststrike, testlastframestrike, testallstrikes]
testsok = all (\test -> fst test == snd test) allresults

