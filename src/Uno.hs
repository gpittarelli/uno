{-# LANGUAGE TemplateHaskell #-}

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
import Control.Lens hiding (element)
import Control.Lens.TH
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

data Player = Player { _hand :: Hand }
  deriving (Show, Eq)

makeLenses ''Player

-- Draw deck, discard deck, wild color, player hands (in order of play)
data Game = Game { _deck :: Deck
                 , _discard :: Deck
                 , _wildColor :: Color
                 , _players :: [Player] }
  deriving (Show, Eq)

makeLenses ''Game

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
skip = over players $ rotate 1

headLens :: Lens' [a] a
headLens = lens head (\orig -> \newhead -> newhead : (tail orig))

tailLens :: Lens' [a] [a]
tailLens = lens tail (\orig -> \newtail -> (head orig) : newtail)

reverse :: Game -> Game
reverse = over (players . tailLens) Data.List.reverse

reshuffle :: (MonadRandom m) => Game -> m Game
reshuffle game =
  let discard' = _discard game
      topCard:_ = discard'
  in do newDeck <- shuffleM ((_deck game) ++ discard')
        return (game & deck .~ newDeck & discard .~ [topCard])

-- give card to current player
giveCard :: Card -> Game -> Game
giveCard c game =
  game & (players . headLens . hand) %~ ((:) c)

pick :: Game -> Game
pick game =
  let topCard:deck' = _deck game
  in giveCard topCard (game & deck .~ deck')

drawCard :: (MonadRandom m) => Int -> Game -> m Game
drawCard _ Game{_players=[]} = error "invalid game: no hands"
drawCard 0 game = return game
drawCard n game@Game{_deck=[]} =
  do shuffled <- reshuffle game
     drawCard n shuffled
drawCard n game = drawCard (n - 1) $ pick game

removeCard :: Card -> Player -> Player
removeCard c = over hand (delete c)

specialEffect :: (MonadRandom m) => Card -> Game -> m Game
specialEffect (_, Skip) game = return $ skip game
specialEffect (_, Reverse) game = return $ Uno.reverse game
specialEffect (_, DrawTwo) game = fmap skip (drawCard 2 game)
specialEffect (_, DrawFour) game = fmap skip (drawCard 4 game)
specialEffect (_, _) game = return game

apply :: (MonadRandom m) => Game -> Card -> Color -> m Game
apply game card wildColor =
  let deck = _deck game
      discard' = _discard game
      cur : others = _players game
  in  assert (card `elem` (_hand cur) && canPlay game card)
             specialEffect card
                          Game { _deck = deck
                               , _discard = card:discard'
                               , _wildColor = wildColor
                               , _players = others ++ [removeCard card cur]}

colorChoices :: Card -> [Color]
colorChoices (Black, _) = [Red, Blue, Green, Yellow]
colorChoices _          = [Black]

-- Can play card on the current game?
canPlay :: Game -> Card -> Bool
canPlay Game{_discard = (c1, t1):_, _wildColor = c1'} (c2, t2) =
  c1 == c2 || t1 == t2 || c2 == Black || (c1 == Black && c2 == c1')
canPlay Game{_discard=[]} _ = error "Empty discard pile"

-- Draw and play next cards
draw :: (MonadRandom m) => Game -> m [Game]
draw game@Game{_deck=[]} =
  do shuffled <- reshuffle game
     draw shuffled
draw game =
  let top:rest = _deck game
      discard' = _discard game
  in if canPlay game top
     then return [game { _deck = rest
                       , _discard = top:discard'
                       , _wildColor = c } | c <- colorChoices top]
     else draw $ giveCard top $ game {_deck = rest}

plays :: (MonadRandom m) => Game -> m [Game]
plays game =
  let hand:_ = map _hand $ _players game
      plays' = [
        (card, wildColor) |
        card <- filter (canPlay game) hand,
        wildColor <- colorChoices card]
  in if null plays'
     then draw game
     else sequence (liftM (\(card, wildColor) -> apply game card wildColor)
                          plays')

finished :: Game -> Bool
finished game = isJust $ find null (map _hand $ _players game)

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
  in Game{ _deck = deck''
         , _discard = [firstCard]
         , _wildColor = Red
         , _players = map (\hand -> Player{_hand = hand})
           $ chunksOf handSize hands}

dealM :: (MonadRandom m) => Deck -> Int -> m Game
dealM deck players = do
  deck' <- shuffleM deck
  let (hands, rest) = splitAt (players * handSize) deck'
      firstCard : deck'' = rest
  return Game { _deck = deck''
              , _discard = [firstCard]
              , _wildColor = head (colorChoices firstCard)
              , _players = map (\hand -> Player{_hand = hand}) $ chunksOf handSize hands}
