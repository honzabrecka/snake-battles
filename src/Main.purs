module Main
  ( initGame
  , tick
  , pause
  , resume
  , updateDirection
  , codeToDirection
  , Direction
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array (all, alterAt, any, concat, concatMap, cons, filter, foldl, group, length, mapWithIndex, range, replicate, take, unsafeIndex)
import Data.Array.Partial as Partial
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty(..))
import Data.Set (fromFoldable, member)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

data Direction = Left | Right | Up | Down

instance eqDirection :: Eq Direction where
  eq Left Left = true
  eq Right Right = true
  eq Up Up = true
  eq Down Down = true
  eq _ _ = false

type Point = Tuple Int Int

type OrientedPoint = Tuple Point Direction

type Body = Array OrientedPoint

type Snake =
  { body :: Body
  , ate :: Boolean
  , live :: Boolean
  , direction :: Direction
  }

type Board = Array (Array Point)

type Dimensions = Tuple Int Int

type Game =
  { snakes :: Array Snake
  , food :: Point
  , dimensions :: Dimensions
  , tick :: Int
  , startAfter :: Int
  , pause :: Maybe String
  , end :: Boolean
  }

directionToPoint :: Direction -> Point
directionToPoint Left  = Tuple (-1) 0
directionToPoint Right = Tuple 1 0
directionToPoint Down  = Tuple 0 (-1)
directionToPoint Up    = Tuple 0 1

heads :: Array OrientedPoint
heads =
  [ (Tuple (Tuple 6 2) Right)
  , (Tuple (Tuple 5 8) Left)
  , (Tuple (Tuple 2 4) Up)
  , (Tuple (Tuple 7 7) Down)
  ]

board :: Dimensions -> Board
board (Tuple width height) = map (\x -> map (\y -> Tuple x y) h) w
  where
    w = range 0 $ width - 1
    h = range 0 $ height - 1

point :: OrientedPoint -> Point
point = fst

concatSnakes :: Array Snake -> Array Point
concatSnakes = concatMap (map point <<< body)
  where
    body = \{ body } -> body

head :: Body -> Point
head = point <<< unsafePartial Partial.head

tail :: Body -> Body
tail = unsafePartial Partial.tail

inDimensions :: Dimensions -> Point -> Point
inDimensions (Tuple w h) (Tuple x y)
  | x == -1   = Tuple (w - 1) y
  | x == w    = Tuple 0 y
  | y == -1   = Tuple x (h - 1)
  | y == h    = Tuple x 0
  | otherwise = (Tuple x y)

nextHead :: Dimensions -> Snake -> OrientedPoint
nextHead dimensions { body, direction } = Tuple head' direction
  where
    head' = inDimensions dimensions $ (head body) + (directionToPoint direction)

grow :: Boolean -> Body -> Body
grow true  = id
grow false = unsafePartial Partial.init

moveSnake :: (Boolean -> Body -> Body) -> Dimensions -> Point -> Snake -> Snake
moveSnake grow dimensions food snake = snake { body = body', ate = ate }
  where
    head' = nextHead dimensions snake
    ate = food == point head'
    body' = grow ate $ cons head' snake.body

initSnake :: Dimensions -> Int -> OrientedPoint -> Snake
initSnake dimensions length (Tuple head direction) =
  foldl (flip $ moveSnake fakeGrow dimensions) snake $ replicate (length - 1) fakeFood
  where
    fakeGrow = \_ -> id
    fakeFood = Tuple (-5) (-5)
    snake =
      { body: [Tuple head direction]
      , direction: direction
      , ate: false
      , live: true
      }

nextFood :: forall eff. Dimensions -> Array Snake -> Eff (random :: RANDOM | eff) Point
nextFood dimensions snakes = do
  i <- randomInt 0 (length empty)
  pure $ unsafePartial unsafeIndex empty i
  where
    occupied = fromFoldable $ concatSnakes snakes
    empty = concatMap (filter (\p -> not $ member p occupied)) $ board dimensions

initGame :: forall eff. Int -> Int -> Int -> Eff (random :: RANDOM | eff) Game
initGame width height count = do
  food <- nextFood dimensions snakes
  pure
    { snakes: snakes
    , food: food
    , dimensions: dimensions
    , tick: 0
    , startAfter: 10
    , pause: Nothing
    , end: false
    }
  where
    dimensions = (Tuple width height)
    snakes = map (initSnake dimensions 3) $ take count heads

moveSnakes :: forall eff. Game -> Eff (eff) Game
moveSnakes game = pure game { snakes = snakes }
  where
    snakes = map (moveSnake grow game.dimensions game.food) game.snakes

checkHits :: forall eff. Game -> Eff (eff) Game
checkHits game = pure game { snakes = snakes }
  where
    snakes = map hit game.snakes
    hit :: Snake -> Snake
    hit snake = snake { live = not dead }
      where
        head' = head snake.body
        dead = any hit' game.snakes
        hit' :: Snake -> Boolean
        hit' { body } = member head' $ fromFoldable $ map point body'
          where
            body' =
              if head' == head body
              then tail body
              else body

addFood :: forall eff. Game -> Eff (random :: RANDOM | eff) Game
addFood game =
  if ate && (length $ concatSnakes game.snakes) < dimensions
  then
    do
      food <- nextFood game.dimensions game.snakes
      pure game { food = food }
  else pure game
  where
    ate = any (\{ ate } -> ate) game.snakes
    dimensions = (fst game.dimensions) * (snd game.dimensions)

checkEnd :: forall eff. Game -> Eff (eff) Game
checkEnd game = pure game { end = full || allDead }
  where
    dimensions = (fst game.dimensions) * (snd game.dimensions)
    full = (length $ concatSnakes game.snakes) == dimensions
    allDead = all (\{ live } -> not live) game.snakes

tick :: forall eff. Game -> Eff (random :: RANDOM | eff) Game
tick game
  | Nothing <- game.pause = do
    pure game { tick = game.tick + 1 }
     >>= moveSnakes
     >>= checkHits
     >>= addFood
     >>= checkEnd
  | otherwise = pure game

pause :: String -> Int -> Game -> Game
pause player tick game
  | tick == game.tick = game { pause = Just player }
  | otherwise = game

resume :: String -> Int -> Game -> Game
resume player tick game = resume' game.pause
  where
    resume' :: Maybe String -> Game
    resume' (Just player')
      | tick == game.tick && player == player' = game { pause = Nothing }
      | otherwise = game
    resume' Nothing = game

codeToDirection :: Int -> Direction
codeToDirection 0 = Left
codeToDirection 1 = Right
codeToDirection 2 = Up
codeToDirection _ = Down

directionToCode :: Direction -> Int
directionToCode Left  = 0
directionToCode Right = 1
directionToCode Up    = 2
directionToCode _     = 3

updateDirection :: Direction -> Int -> Int -> Game -> Game
updateDirection direction i tick game
  | tick == game.tick = game { snakes = fromMaybe [] snakes' }
    where
      snakes' = alterAt i (\snake -> Just snake { direction = direction }) game.snakes
  | otherwise = game

encodeSnake :: Tuple Int Snake -> Array (Array Int)
encodeSnake (Tuple index { body, live }) =
  [ [if live then 1 else 0, index]
  , [(fst head'), (snd head')]
  ]
  where
    head' = head body
    directions = group $ map snd body
    body' = map f directions
    f :: NonEmpty Array Direction -> Array Int
    f (NonEmpty h t) = [directionToCode h, length t + 1]

encode :: Game -> Array Int
encode game = []
  where
    header =
      [ if game.end then 1 else 0
      , game.tick
      , fst game.dimensions
      , snd game.dimensions
      ]
    snakes = map encodeSnake $ mapWithIndex (\i s -> Tuple i s) $ game.snakes
