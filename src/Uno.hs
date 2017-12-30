module Uno
    ( baseDeck,
      deal,
      dealM,
      apply,
      plays,
      playOut
    ) where

import Control.Exception.Base
import Control.Monad
import Control.Monad.Random.Class
import System.Random
import System.Random.Shuffle
import Data.List.Split
import Data.List
import Data.Maybe

data Color = Red | Blue | Green | Yellow | Black
  deriving (Show, Eq)

data Type = Zero | One | Two | Three | Four | Five | Six
  | Seven | Eight | Nine | Skip | Reverse | DrawTwo | Wild | DrawFour
  deriving (Show, Eq)

type Card = (Color, Type)
type Deck = [Card]
type Hand = [Card]

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

-- Draw deck, discard deck, wild color, player hands (in order of play)
type Game = (Deck, Deck, Color, [Hand])
discard :: Game -> Deck
discard (_, discard', _, _) = discard'

sample :: (MonadRandom m) => [a] -> m (Maybe a)
sample [] = return Nothing
sample xs = do i <- getRandomR (0, length xs - 1)
               return $ Just (xs !! i)

cartProd xs ys = [(x,y) | x <- xs, y <- ys]
double xs = concat [xs, xs]

baseDeck = concat [
  cartProd [Red, Blue, Green, Yellow] [Zero],

  double $ cartProd
    [Red, Blue, Green, Yellow]
    [One, Two, Three, Four, Five, Six, Seven, Eight, Nine,
     Skip, Reverse, DrawTwo],

  double $ double $ cartProd [Black] [Wild, DrawFour]
  ]

handSize :: Int
handSize = 7

skip :: Game -> Game
skip (deck, discard', wildColor, hands) =
  (deck, discard', wildColor, rotate 1 hands)

reverse :: Game -> Game
reverse (_, _, _, []) = error "Invalid game: no hands"
reverse (deck, discard', wildColor, cur:hands) =
  (deck, discard', wildColor, cur : (Data.List.reverse hands))

reshuffle :: (MonadRandom m) => Game -> m Game
reshuffle (_, [], _, _) = error "can't reshuffle game with no discard"
reshuffle (deck, topCard:discard', wildColor, hands) =
  do newDeck <- shuffleM (deck ++ discard')
     return (newDeck, [topCard], wildColor, hands)


drawCard :: (MonadRandom m) => Int -> Game -> m Game
drawCard _ (_, _, _, []) = error "invalid game: no hands"
drawCard 0 game = return game
drawCard n ([], topCard:discard', wildColor, cur:hands) =
  do shuffled <- reshuffle ([], topCard:discard', wildColor, cur:hands)
     drawCard n shuffled
drawCard n (pick:deck, discard', wildColor, cur:hands) =
  drawCard (n - 1)
           (deck, discard', wildColor, (pick:cur) : hands)

specialEffect :: (MonadRandom m) => Card -> Game -> m Game
specialEffect (_, Skip) game = return $ skip game
specialEffect (_, Reverse) game = return $ Uno.reverse game
specialEffect (_, DrawTwo) game = fmap skip (drawCard 2 game)
specialEffect (_, DrawFour) game = fmap skip (drawCard 4 game)
specialEffect (_, _) game = return game

  -- Skip | Reverse | DrawTwo | Wild | DrawFour

-- Color is the chosen wild color
apply :: (MonadRandom m) => Game -> Card -> Color -> m Game
apply game card color =
  let (deck, discard', _, hand : others) = game
  in  assert (card `elem` hand && canPlay game card)
             specialEffect card
                          (deck,
                           card:discard',
                           color,
                           others ++ [delete card hand])

colorChoices :: Card -> [Color]
colorChoices (Black, _) = [Red, Blue, Green, Yellow]
colorChoices _          = [Black]

-- Can play card on the current game?
canPlay :: Game -> Card -> Bool
canPlay (_, (c1, t1):_, c1', _) (c2, t2) =
  c1 == c2 || t1 == t2 || c2 == Black || (c1 == Black && c2 == c1')
canPlay (_, [], _, _) _ = error "Empty discard pile"

draw ::  (MonadRandom m) => Game -> m [Game]
draw ([], top:discard', wildColor, hands) = do
  discard'' <- shuffleM discard'
  draw (discard'', [top], wildColor, hands)
draw game =
  let (top:rest, discard', wildColor, hand:hands) = game
  in  if canPlay game top
    then return [(rest,
                  top:discard',
                  c,
                  hand:hands) | c <- colorChoices top]
    else draw (rest, discard', wildColor, (top:hand):hands)

plays :: (MonadRandom m) => Game -> m [Game]
plays game =
  let (_, _, _, hand:_) = game
      plays' = [
        (card, wildColor) |
        card <- filter (canPlay game) hand,
        wildColor <- colorChoices card]
  in if null plays'
     then draw game
     else sequence (liftM (\(card, wildColor) -> apply game card wildColor)
                          plays')

finished :: Game -> Bool
finished (_, _, _, hands) = null (last hands)

playOut' :: (MonadRandom m) => Game -> Int -> m Int
playOut' game n = if finished game
  then return   n
  else do
    nextSteps <- plays game
    step <- sample nextSteps
    playOut' (fromJust step) (n + 1)

playOut :: (MonadRandom m) => Game -> m Int
playOut game = playOut' game 0

deal :: (RandomGen a) => Deck -> Int -> a -> Game
deal deck players rng =
  let deck' = shuffle' deck (length deck) rng
      (hands, rest) = splitAt (players * handSize) deck'
      firstCard : deck'' = rest
  in (deck'', [firstCard], Red, chunksOf handSize hands)

dealM :: (MonadRandom m) => Deck -> Int -> m Game
dealM deck players = do
  deck' <- shuffleM deck
  let (hands, rest) = splitAt (players * handSize) deck'
      firstCard : deck'' = rest
  return (deck'',
          [firstCard],
          head (colorChoices firstCard),
          chunksOf handSize hands)
