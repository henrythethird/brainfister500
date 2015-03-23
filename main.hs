import Data.Word
import Data.Char

data State = State
	{ program 	:: [Comm]
	, pcounter 	:: Int
	, memory 	:: [Word8]
	, memcnt	:: Int
	, out		:: [Word8]
	, stdin		:: [Word8]
	} deriving (Show)


data Comm = Print | Read | RShift | LShift | Incr | Decr | Loop | Pool deriving (Show, Eq)


readComm :: Char -> Comm
readComm a
	| a == '.' 	= Print
	| a == ','	= Read
	| a == '+'	= Incr
	| a == '-'	= Decr
	| a == '['	= Loop
	| a == ']'	= Pool
	| a == '<'	= LShift
	| a == '>'	= RShift


preFilter :: String -> [Comm]
preFilter s = [readComm x | x <- filter (\a -> elem a ".,+-[]<>") s]


setValAt :: [a] -> Int -> a -> [a]
setValAt l i v = let (x,_:xs) = splitAt i l in x ++ (v:xs)


rightSearch :: [Comm] -> Int -> Int -> Int
rightSearch _ index 0 = index
rightSearch comm index bc
	| com == Loop	= rs (bc + 1)
	| com == Pool	= rs (bc - 1)
	| otherwise	= rs bc
	where
		com = head comm
		rs = rightSearch (tail comm) (index + 1)


leftSearch :: [Comm] -> Int -> Int -> Int
leftSearch _ index 0 = index - 1
leftSearch comm index bc
	| com == Loop	= ls (bc - 1)
	| com == Pool	= ls (bc + 1)
	| otherwise	= ls bc
	where
		com = last comm
		ls = leftSearch (init comm) (index - 1)


head' :: (Num a) => [a] -> a
head' [] = 0
head' a = head a


exec :: State -> State
exec s = case progAt of 
	Print	-> s { out = memAt : out s }
	Read	-> s { memory = sVal (head' $ sin), stdin = tail $ sin }
	Incr 	-> s { memory = sVal (memAt + 1)  }
	Decr 	-> s { memory = sVal (memAt - 1)  }
	LShift 	-> s { memcnt = mc - 1 }
	RShift 	-> s { memcnt = mc + 1 }
	Loop	-> s { pcounter = if memAt == 0 then rightSearch (tail (snd $ splitAt pc p)) pc 1 else pc }
	Pool	-> s { pcounter = leftSearch (fst $ splitAt pc p) pc 1 }
	where
		progAt = program s !! (pcounter s)
		memAt = memory s !! (memcnt s)
		sVal = setValAt (memory s) (memcnt s)
		pc = pcounter s
		mc = memcnt s
		p = program s
		sin = stdin s


next :: State -> State
next s = newState { pcounter = pcounter newState + 1 }
	where
		newState = exec s


eval' :: State -> Maybe State
eval' s
	| pcounter s >= (length $ program s)	= Nothing
	| otherwise				= Just s


eval :: State -> IO ()
eval s = case state of
	Just state	-> eval $ next state 
	Nothing		-> print $ out s
	where
		state = eval' s


splitAtChar' :: String -> Char -> Int -> Maybe Int
splitAtChar' str delim ind
	| str == [] 		= Nothing
	| head str == delim 	= Just $ ind + 1
	| otherwise		= splitAtChar' (tail str) delim (ind + 1)


splitAtChar :: String -> Char -> (String, String)
splitAtChar str delim = case splitAtChar' str delim 0 of
	Just a	-> splitAt a str
	Nothing	-> (str,"")


main = do	prog <- getContents
		let (prog2, input) = splitAtChar prog '!'
		eval State { program	= preFilter prog2
			   , pcounter	= 0
			   , memory	= mem
			   , memcnt	= 0
			   , out	= []
			   , stdin	= map (\c -> toEnum (fromEnum c)::Word8) input
			   } 
	where
		mem = take 300 $ repeat 0
