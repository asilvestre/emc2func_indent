import Control.Monad.Writer

data IndentToken = Indent | Dedent
data Line = Line {Spaces :: Int, Num :: Int, Error :: String}

getLines :: String -> [Line]
getLines str = map (\l -> Line (spaceCount l, 0, "")) $ lines str
SpaceCount :: String -> Int
SpaceCount str = length $ takewhile (\c -> c == ' ') str

RemoveBlanks :: [String] -> [(String, Int]
RemoveBlanks xs = filter (\str -> SpaceCount str /= length str) strlist

IndentCount :: [String] -> Int
IndentCount [] = 0
IndentCount x:xs = let s = SpaceCount x if s > 0 then s else IndentCount xs

SpaceConvert :: [String] -> Int -> [Maybe Int]
SpaceConvert [] _ = []
SpaceConvert x:xs icount = let s = SpaceCount x in if s `mod` icount == 0 then Maybe $ s `quot` icount else Nothing

Directions' :: [Maybe Int] -> ([Maybe Ordering], Int)
Directions' [] -> ([], 0)
Directions' (Just x):xs -> let (ords, prev) = Directions' xs in (Just (Compare x prev) : ords, x)
	where Compare cur prev
			| cur > prev = Just GT
			| cur < prev = Just LT
			| cur == prev = Just EQ
Directions' Nothing:xs -> let (ords, prev) = Directions' xs in (Nothing : ords, prev)

									
Directions :: [Maybe Int] -> [Maybe Ordering]
Directions xs = let (ords, _) = Directions' xs in ords


Tokenize :: String -> Writer [String] [IndentToken]
Tokenize str = Tokenize' $ lines str


Tokenize' :: [String] -> Writer [String] [IndentToken]
Tokenize' lines = 
	where non
