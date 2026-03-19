module MyEvolModule where

data MyEvolution = LUCA | Cyanobacteria | Trilobite | Ichthyostega | Dimetrodon | Archaeopteryx | Morganucodon | Purgatorius | Australopithecine | Humans

instance Show MyEvolution where
    show LUCA = "Last Universal Common Ancestor"
    show Cyanobacteria = "Synechococcus"
    show Trilobite = "Paradoxides"
    show Ichthyostega = "Ichthyostega"
    show Dimetrodon = "Dimetrodon"
    show Archaeopteryx = "Archaeopteryx"
    show Morganucodon = "Morganucodon"
    show Purgatorius = "Purgatorius"
    show Australopithecine = "Australopithecus Afarensis"
    show Humans = "Homo Sapiens"
    
instance Read MyEvolution where
    readsPrec _ str = case str of
            'L':'a':'s':'t':' ':'U':'n':'i':'v':'e':'r':'s':'a':'l':' ':'C':'o':'m':'m':'o':'n':' ':'A':'n':'c':'e':'s':'t':'o':'r':rest -> [(LUCA, rest)]
            'S':'y':'n':'e':'c':'h':'o':'c':'o':'c':'c':'u':'s':rest -> [(Cyanobacteria, rest)]
            'P':'a':'r':'a':'d':'o':'x':'i':'d':'e':'s':rest -> [(Trilobite, rest)]
            'I':'c':'h':'t':'h':'y':'o':'s':'t':'e':'g':'a':rest -> [(Ichthyostega, rest)]
            'D':'i':'m':'e':'t':'r':'o':'d':'o':'n':rest -> [(Dimetrodon, rest)]
            'A':'r':'c':'h':'a':'e':'o':'p':'t':'e':'r':'y':'x':rest -> [(Archaeopteryx, rest)]
            'M':'o':'r':'g':'a':'n':'u':'c':'o':'d':'o':'n':rest -> [(Morganucodon, rest)]
            'P':'u':'r':'g':'a':'t':'o':'r':'i':'u':'s':rest -> [(Purgatorius, rest)]
            'A':'u':'s':'t':'r':'a':'l':'o':'p':'i':'t':'h':'e':'c':'u':'s':' ':'A':'f':'a':'r':'e':'n':'s':'i':'s':rest -> [(Australopithecine, rest)]
            'H':'o':'m':'o':' ':'S':'a':'p':'i':'e':'n':'s':rest -> [(Humans, rest)]


instance Eq MyEvolution where
    (==) LUCA LUCA = True
    (==) Cyanobacteria Cyanobacteria = True
    (==) Trilobite Trilobite = True
    (==) Ichthyostega Ichthyostega = True
    (==) Dimetrodon Dimetrodon = True
    (==) Archaeopteryx Archaeopteryx = True
    (==) Morganucodon Morganucodon = True
    (==) Purgatorius Purgatorius = True
    (==) Australopithecine Australopithecine = True
    (==) Humans Humans = True
    (==) _ _ = False
    (/=) x y = not (x == y)

instance Ord MyEvolution where
    compare x y = compare (stage x) (stage y)
        where
            stage LUCA = 0
            stage Cyanobacteria = 1
            stage Trilobite = 2
            stage Ichthyostega = 3
            stage Dimetrodon = 4
            stage Archaeopteryx = 5
            stage Morganucodon = 6
            stage Purgatorius = 7
            stage Australopithecine = 8
            stage Humans = 9

instance Enum MyEvolution where
    toEnum 0 = LUCA
    toEnum 1 = Cyanobacteria
    toEnum 2 = Trilobite
    toEnum 3 = Ichthyostega
    toEnum 4 = Dimetrodon
    toEnum 5 = Archaeopteryx
    toEnum 6 = Morganucodon
    toEnum 7 = Purgatorius
    toEnum 8 = Australopithecine
    toEnum 9 = Humans
    fromEnum LUCA = 0
    fromEnum Cyanobacteria = 1
    fromEnum Trilobite = 2
    fromEnum Ichthyostega = 3
    fromEnum Dimetrodon = 4
    fromEnum Archaeopteryx = 5
    fromEnum Morganucodon = 6
    fromEnum Purgatorius = 7
    fromEnum Australopithecine = 8
    fromEnum Humans = 9

instance Bounded MyEvolution where
    minBound = LUCA
    maxBound = Humans

data MyEvolution' = LUCA' | Cyanobacteria' | Trilobite' | Ichthyostega' | Dimetrodon' | Archaeopteryx' | Morganucodon' | Purgatorius' | Australopithecine' | Humans' 
    deriving (Show, Read, Eq, Ord, Enum, Bounded)