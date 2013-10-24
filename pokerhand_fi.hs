import Data.List

data Maa = Hertta | Pata | Risti | Ruutu deriving (Eq)
data Kortti = Kortti {maa:: Maa, numero:: Int}
data Kasi = ViisiSamaa | NeljaSamaa | Tayskasi | Vari | Kolmoset | Pari | Hai deriving (Show, Eq)
type Kortit = [Kortti]

        
samanNumeronRyhmienPituudet :: Kortit -> [Int]
samanNumeronRyhmienPituudet = reverse . sort . (map length) . samanNumeronRyhmat 
    where samanNumeronRyhmat = group . sort . (map numero)
        
onkoSamoja :: Int -> Kortit -> Bool
onkoSamoja montako kortit = (samojaNumeroita kortit) >= montako
  where samojaNumeroita = head . samanNumeronRyhmienPituudet
  
vari :: Kortit -> Bool
vari kortit = all (== (head maat)) maat
    where maat = (map maa) kortit          

parasKasi :: Kortit -> Kasi
parasKasi kortit | onkoSamoja 5 kortit = ViisiSamaa
                 | onkoSamoja 4 kortit = NeljaSamaa
                 | samanNumeronRyhmienPituudet kortit == [3,2] = Tayskasi
                 | vari kortit = Vari
                 | onkoSamoja 3 kortit = Kolmoset
                 | onkoSamoja 2 kortit = Pari
                 | otherwise = Hai


parikasi = [Kortti Hertta 10, Kortti Pata 1, Kortti Ruutu 10, Kortti Risti 1, Kortti Risti 7] 
tayskasi = [Kortti Hertta 10, Kortti Pata 1, Kortti Ruutu 10, Kortti Risti 1, Kortti Risti 10] 
varikasi = [Kortti Hertta 10, Kortti Hertta 1, Kortti Hertta 4, Kortti Hertta 2, Kortti Hertta 12] 
 
main = print (all (== True) 
    [parasKasi parikasi == Pari, 
     parasKasi tayskasi == Tayskasi,
     parasKasi varikasi == Vari])