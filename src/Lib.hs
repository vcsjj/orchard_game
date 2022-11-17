module Lib ( Dice (..), nextRound, play, score, Result (..), prob ) where

import System.Random

type State = (Int, Int, Int, Int, Int)
data Result = RavenWins | PlayersWin | Unfinished
    deriving (Show, Eq)
data Dice = Appel | Pear | Plum | Cherry | Raven | Basket
    deriving (Show, Enum, Bounded)

instance Random Dice where
    randomR (a, b) g = (toEnum x, g') where 
        (x, g')  = randomR (fromEnum a, fromEnum b) g
    random g = randomR (minBound, maxBound) g

ge0::Int -> Int
ge0 i   | i >  0    = i
        | otherwise = 0

thd :: (a, b, c) -> c
thd  (_, _, a) = a

snd' :: (a, b, c) -> b
snd' (_, a, _) = a


nextRound :: State -> Dice -> State
nextRound (a, b, p, k, r) dice =
    case dice of 
        Raven  -> (a, b, p, k, r+1)
        Basket -> pickOneFromFullestTree . pickOneFromFullestTree $ (a, b, p, k, r)
        Appel  -> (ge0 $ a-1, b, p, k, r)
        Pear   -> (a, ge0 $ b-1, p, k, r)
        Plum   -> (a, b, ge0 $ p-1, k, r)
        Cherry -> (a, b, p, ge0 $ k-1, r)

pickOneFromFullestTree :: State -> State
pickOneFromFullestTree (a,b,p,k,r) 
    | a >= b && a >= p && a >= k = (ge0 $ a-1, b, p, k, r)
    | b >= a && b >= p && b >= k = (a, ge0 $ b-1, p, k, r)
    | p >= a && p >= b && p >= k = (a, b, ge0 $ p-1, k, r)
    | otherwise                  = (a, b, p, ge0 $ k-1, r)

score :: State -> Result
score (0, 0, 0, 0, _) = PlayersWin
score (_, _, _, _, 9) = RavenWins
score _ = Unfinished


play :: [Dice] -> [State] -> ([State], Result, [Dice])
play (d:dice) (cur:xs) = 
    case score cur of 
        PlayersWin -> (cur:xs, PlayersWin, dice)
        RavenWins  -> (cur:xs, RavenWins, dice)
        Unfinished -> play dice $ (nextRound cur d):cur:xs

playOneGame :: [Dice] -> ([State], Result, [Dice])
playOneGame dice = play dice [(10, 10, 10, 10, 0)]

playMany :: Int -> [Dice] -> [Result]
playMany 0 _ = []
playMany n dice = result:(playMany (n-1) moreDice)
    where 
        s         = playOneGame dice
        result    = snd' s
        moreDice  = thd s

prob :: Int -> IO ()
prob i = do
    g <- newStdGen
    let p = length ( filter (\x -> x == RavenWins) $ playMany i (randoms g :: [Dice]))
    print $ (fromIntegral p :: Double)/(fromIntegral i :: Double) 
