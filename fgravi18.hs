-- author fgravi18

data FreqElem = Frq Char Int | Node FreqElem FreqElem deriving Show
data DictElem = Dict Char [Char] deriving (Show, Read)

-- comparator for FreqElem
frequal :: FreqElem -> FreqElem -> Bool
frequal (Frq c1 i1) (Frq c2 i2)
  | c1 == c2 = True
  | otherwise = False
frequal (Node f1 f2) (Node f3 f4) = (frequal f1 f3) && (frequal f2 f4)
frequal _ _ = False

instance Eq FreqElem where
  f1 == f2 = frequal f1 f2

-- gets the size of a FreqElem
getFreqElemSize :: FreqElem -> Int
getFreqElemSize (Frq a b) = b
getFreqElemSize (Node a b) = (getFreqElemSize a) + (getFreqElemSize b)

-- max function for FreqElem
freqMax :: FreqElem -> FreqElem -> FreqElem
freqMax f1 f2 = let
  sf1 = getFreqElemSize f1
  sf2 = getFreqElemSize f2
  in
    if sf1 > sf2 then f1
    else f2

--gets the characters represented by the freqelem
getFreqElemCharacters :: FreqElem -> [Char]
getFreqElemCharacters (Frq c _) = c:[]
getFreqElemCharacters (Node c1 c2) = (getFreqElemCharacters c1) ++ (getFreqElemCharacters c2)

-- increments the frequency of a character in a frequency list
-- adds it if it does not exist
freqListIncrement :: Char -> [FreqElem] -> [FreqElem]
freqListIncrement char [] = (Frq char 1):[]
freqListIncrement char ((Frq c i):xs)
 | char == c = (Frq c (i+1)):xs
 | otherwise = (Frq c i):(freqListIncrement char xs)

-- tail recursive helper for makefreqlist
makeFreqListHelper :: [Char] -> [FreqElem] -> [FreqElem]
makeFreqListHelper [] frqlist = frqlist
makeFreqListHelper (c:cs) frqlist = makeFreqListHelper cs (freqListIncrement c frqlist)

-- takes the data we want to encode and returns a list of Frq with the character counts
makeFreqList :: [Char] -> [FreqElem]
makeFreqList chararray = makeFreqListHelper chararray []

-- function to find the lowest FreqElem in the list, uses foldl
findLowest :: [FreqElem] -> Maybe FreqElem
findLowest [] = Nothing
findLowest lst = (Just (foldl freqMax (Frq ' ' 0) lst)) --0 will always be lower than anything else in the list

-- removing freq elem from list using filter and a lambda expression
rmFreqFromList :: [FreqElem] -> FreqElem -> [FreqElem]
rmFreqFromList lst f=
  filter (\n -> (n == f) == False) lst -- using the overloaded ==


-- function that creates the tree, (let syntax just to prove I can use it)
makeFrqTree :: [FreqElem] -> FreqElem
makeFrqTree [] = (Frq 'a' 1) --will not reach this unless empty string as parameter;
makeFrqTree list = let (Just s1) = findLowest list -- finds the lowest element in the list
                       list2 = rmFreqFromList list s1 -- removes it from the list
                       sizes1 = getFreqElemSize s1 -- gets its size
                       (Just s2) = findLowest list2 -- takes the lowest element from the list without the prev lowest element
                       listr = rmFreqFromList list2 s2 -- removes it as well from the list
                       sizes2 = getFreqElemSize s2 -- gets its size
                   in
                     if (length list) == 1 then
                       --we are left with only one element, can't combine further
                       s1
                     else
                       -- continue making the freq tree on the remained list and a node combining s1 and s2
                       makeFrqTree ((Node s1 s2):[] ++ listr)

-- function that takes the input text and creates the tree
textToTree :: [Char] -> FreqElem
textToTree input = makeFrqTree (makeFreqList input)

-- helper for makeDictionaryFromTree
-- sofar keeps the path through the tree
makeDictionaryFromTreeHelper :: [Char] -> FreqElem -> [DictElem]
makeDictionaryFromTreeHelper sofar (Frq c i) = (Dict c sofar):[]
makeDictionaryFromTreeHelper sofar (Node c1 c2) =
    x ++ y
  where
    x = (makeDictionaryFromTreeHelper (sofar ++ "0") c1)
    y = (makeDictionaryFromTreeHelper (sofar ++ "1") c2)

-- function that creates the dictionary for encoding the string
makeDictionaryFromTree :: FreqElem -> [DictElem]
makeDictionaryFromTree tree = makeDictionaryFromTreeHelper "" tree

-- function that retrieves the encoding of a character from the dictionary
encodeFromDictionary :: Char -> [DictElem] -> [Char]
encodeFromDictionary a [] = "" -- this should use Nothing in a real program
encodeFromDictionary a ((Dict c e):xs)
  | a == c = e
  | otherwise = encodeFromDictionary a xs


-- function that takes the input string and the dictionary and encodes it
stringFromDictEncoder :: [Char] -> [DictElem] -> [Char]
stringFromDictEncoder [] _ = ""
stringFromDictEncoder (x:xs) dict = encd ++ stringFromDictEncoder xs dict
  where
    encd = encodeFromDictionary x dict

-----------------------------------------------------------------------------
-- main function to encode string, can be directly called with any string.---
-----------------------------------------------------------------------------

encodeMyString :: [Char] -> [Char]
encodeMyString input = stringFromDictEncoder input dict
  where
    dict = makeDictionaryFromTree (textToTree input)


-------------------------
-- begin decode logic ---
-------------------------

-- function that retrieves the char from the dictionary by the encoding
decodeFromDictionary :: [Char] -> [DictElem] -> Maybe Char
decodeFromDictionary a [] = Nothing
decodeFromDictionary a ((Dict c e):xs)
  | a == e = Just c
  | otherwise = decodeFromDictionary a xs

-- gets the first n chars in a string
getFirstNChars :: [Char] -> Int -> [Char]
getFirstNChars [] _ = ""
getFirstNChars (x:xs) n
  | n > 0 = x:(getFirstNChars xs (n - 1))
  | otherwise = ""

-- returns the string without the first n chars
removeFirstNChars :: [Char] -> Int -> [Char]
removeFirstNChars [] _ = ""
removeFirstNChars (x:xs) n
  | n > 1 = (removeFirstNChars xs (n - 1))
  | otherwise = xs

-- helper for the string decode function
-- increments n until it finds the code in the dictionary
-- then adds the char and recurses with the rest of the code
decodeMyStringHelper :: [Char] -> [DictElem] -> Int -> [Char]
decodeMyStringHelper [] _ _ = ""
decodeMyStringHelper code dict n =
  let
    mysubstr = getFirstNChars code n
    substrrest = removeFirstNChars code n
    decoded = (decodeFromDictionary mysubstr dict) -- can still do this due to lazy evaluation
  in
    if (n > (length code)) then
      ""
    else
      case decoded of
        Nothing -> decodeMyStringHelper code dict (n + 1)
        (Just dec) -> dec:(decodeMyStringHelper substrrest dict 1)

--------------------------------------------------------------
-- main function called to decode a code using a dictionary --
--------------------------------------------------------------

decodeMyString :: [Char] -> [DictElem] -> [Char]
decodeMyString "" _ = ""
decodeMyString string dict = decodeMyStringHelper string dict 1


--- TESTING ---

inputtext = "In computer science and information theory, a Huffman code is a particular type of optimal prefix code that is commonly used for lossless data compression. The process of finding and/or using such a code proceeds by means of Huffman coding, an algorithm developed by David A. Huffman while he was a Sc.D. student at MIT, and published in the 1952 paper 'A Method for the Construction of Minimum-Redundancy Codes.' -Wikipedia"


themadetree = textToTree inputtext
madedict = makeDictionaryFromTree themadetree
encodedstring = stringFromDictEncoder inputtext madedict
decodedstring = decodeMyString encodedstring madedict

-----------
--- IO ----
-----------

enc = do
  putStrLn "Please enter your string to be encoded"
  l <- readLn
  putStrLn "Dictionary:"
  putStrLn (show (makeDictionaryFromTree (textToTree l)))
  putStrLn "Encoded:"
  putStrLn (encodeMyString l)
  return ()

dec = do
  putStrLn "Please enter the dictionary:"
  dict <- readLn
  putStrLn "Please enter the encoded string:"
  l <- readLn
  putStrLn "Decoded:"
  putStrLn (decodeMyString l dict)
  return ()

