import Data.Word

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
	| com == Loop	= rightSearch (tail comm) (index + 1) (bc + 1)
	| com == Pool	= rightSearch (tail comm) (index + 1) (bc - 1)
	| otherwise	= rightSearch (tail comm) (index + 1) bc
	where
		com = head comm


leftSearch :: [Comm] -> Int -> Int -> Int
leftSearch _ index 0 = index - 1
leftSearch comm index bc
	| com == Loop	= leftSearch (init comm) (index - 1) (bc - 1)
	| com == Pool	= leftSearch (init comm) (index - 1) (bc + 1)
	| otherwise	= leftSearch (init comm) (index - 1) bc
	where
		com = last comm


head' :: (Num a) => [a] -> a
head' [] = 0
head' a = head a


exec :: State -> State
exec s = case progAt of 
	Print	-> s { out = memAt : out s }
	Read	-> s { memory = setValAt (memory s) (memcnt s) (head' $ stdin s), stdin = tail $ stdin s }
	Incr 	-> s { memory = setValAt (memory s) (memcnt s) (memAt + 1)  }
	Decr 	-> s { memory = setValAt (memory s) (memcnt s) (memAt - 1)  }
	LShift 	-> s { memcnt = memcnt s - 1 }
	RShift 	-> s { memcnt = memcnt s + 1 }
	Loop	-> s { pcounter = if memAt == 0 then rightSearch (tail (snd $ splitAt (pcounter s) (program s))) (pcounter s) 1 else pcounter s }
	Pool	-> s { pcounter = leftSearch (fst $ splitAt (pcounter s) (program s)) (pcounter s) 1 }
	where
		progAt = program s !! (pcounter s)
		memAt = memory s !! (memcnt s)


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

main = eval State { program	= prog
		  , pcounter	= 0
		  , memory	= mem
		  , memcnt	= 0
		  , out		= []
		  , stdin	= input
		  } 
	where
		mem = take 300 $ repeat 0
		prog = preFilter "+++[>+++++<-]>."
		input = [2..5]
