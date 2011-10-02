import List

data Suit = Hearts | Clubs | Diamonds | Cross deriving (Ord, Eq, Show)
data Card = C {suit::Suit, nmb::Int} deriving (Eq, Show)

data HandCat = HighCard | Pair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | 
               FourOfAKind | StraightFlush | FifeOfAKind deriving (Show)

instance Ord Card where
    x <= y = nmb x <= nmb y

type Hand = [Card]

type Pair = Int
type TwoPair = (Int, Int)
type ThreeOfAKind = Int
type Straight = Int
type Flush = Suit
type FullHouse = (Int, Int)
type FourOfAKind = Int
type StraightFlush = Card
type FifeOfAKind = Int

is :: Maybe a -> Bool
is f = case f of Nothing -> False 
                 Just _ -> True

handCat :: Hand -> HandCat
handCat hand | is $ getFifeOfAKind hand = FifeOfAKind
             | is $ getStraightFlush hand = StraightFlush
             | is $ getFourOfAKind hand = FourOfAKind
             | is $ getFullHouse hand = FullHouse
             | is $ getFlush hand = Flush
             | is $ getStraight hand = Straight
             | is $ getThreeOfAKind hand = ThreeOfAKind
             | is $ getTwoPair hand = TwoPair
             | is $ getPair hand = Pair
             | otherwise = HighCard
          

getHighCard :: Hand -> Card
getHighCard = head . reverse . sort

getPair :: Hand -> Maybe Pair
getPair = getNofAKind 2

getThreeOfAKind :: Hand -> Maybe ThreeOfAKind
getThreeOfAKind = getNofAKind 3         

getFourOfAKind :: Hand -> Maybe FourOfAKind
getFourOfAKind = getNofAKind 4

getFifeOfAKind :: Hand -> Maybe FourOfAKind
getFifeOfAKind = getNofAKind 5

getNofAKind :: Int -> Hand -> Maybe Int
getNofAKind n hand | highGroups == [] = Nothing
                   | otherwise = Just highNmb
    where highGroups = getHighCardGroups hand n
          highNmb = head $ head highGroups

getTwoPair :: Hand -> Maybe TwoPair
getTwoPair hand | length highGroups < 2 = Nothing
                | otherwise = Just (fstHighNmb, sndHighNmb)
    where highGroups = getHighCardGroups hand 2
          fstHighNmb = head $ head highGroups
          sndHighNmb = head $ head $ drop 1 highGroups
         
toNumbers :: Hand -> [Int]
toNumbers = map nmb

getHighCardGroups :: Hand -> Int -> [[Int]]
getHighCardGroups hand gSize = getHighGroups gSize $ toNumbers hand

getHighGroups :: (Ord a) => Int -> [a] -> [[a]]
getHighGroups minGroupSize = delSmallGroups . group . reverse . sort
    where delSmallGroups = filter (\g -> length g >= minGroupSize)

getStraight :: Hand -> Maybe Straight
getStraight hand | tmpStraight == toNumbers hand = Just highNmb
                 | otherwise = Nothing
    where highNmb = head . reverse . sort $ toNumbers hand
          lowNmb = head . sort $ toNumbers hand
          tmpStraight = [lowNmb .. lowNmb + (length hand) - 1]   

getFlush :: Hand -> Maybe Flush
getFlush hand | suitGroup == [] = Nothing
              | otherwise = Just $ head $ map suit hand
    where suitGroup = getHighGroups 5 $ map suit hand

getFullHouse :: Hand -> Maybe FullHouse    
getFullHouse hand | and[hasPair, hasThreeOfAKind] = Just (nmbThree, nmbTwo)    
                  | otherwise = Nothing
    where hasPair = not $ (getPair hand) == Nothing
          hasThreeOfAKind = not $ (getThreeOfAKind hand) == Nothing
          nmbThree = case getThreeOfAKind hand of Just n -> n
          nmbTwo = case getPair hand of Just n -> n

getStraightFlush :: Hand -> Maybe StraightFlush
getStraightFlush hand | and[hasFlush, hasStraight] = Just $ getHighCard hand
                      | otherwise = Nothing
    where hasFlush = not $ getFlush hand == Nothing
          hasStraight = not $ getStraight hand == Nothing

testAll = map handCat testHandList          
testHighCards = map getHighCard testHandList
testStraights = map getStraight testHandList
testFlush = map getFlush testHandList
testFullHouse = map getFullHouse testHandList
testStraightFlush = map getStraightFlush testHandList

testHandList = [testDataPair, testDataTwoPair, testDataStraight, testDataFlush, testDataFullHouse]
    
testDataPair = [C Hearts 1, C Hearts 2, C Clubs 3, C Clubs 4, C Clubs 4]    
testDataTwoPair = [C Hearts 1, C Cross 1, C Clubs 3, C Clubs 4, C Clubs 4] 
testDataStraight = [C Hearts 1, C Cross 2, C Clubs 3, C Clubs 4, C Clubs 5] 
testDataFlush = [C Hearts 1, C Hearts 2, C Hearts 3, C Hearts 4, C Hearts 5] 
testDataFullHouse = [C Hearts 1, C Clubs 1, C Cross 3, C Diamonds 1, C Hearts 3] 