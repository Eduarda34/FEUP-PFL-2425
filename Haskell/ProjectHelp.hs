import Test.QuickCheck

prop_RevRev xs = reverse (reverse xs) == xs
    where types = xs :: [Int]

prop_RevLen xs = length xs == length (reverse xs)
    where types = xs :: [Int]

-- Para testar:
-- ghci> quickTest prop_RevRev