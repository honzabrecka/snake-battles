module Main
  ( initGame
  , tick
  , pause
  , resume
  , switchPause
  , encode
  , updateDirection
  , codeToDirection
  , Direction
  , Game
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array (all, alterAt, any, concat, concatMap, cons, filter, foldl, group, length, mapWithIndex, range, replicate, reverse, take, unsafeIndex)
import Data.Array.Partial as Partial
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty as NE
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

type Body = NE.NonEmpty Array OrientedPoint

type Snake =
  { body :: Body
  , ate :: Boolean
  , live :: Boolean
  , direction :: Direction
  }

type Grid = Array (Array Point)

type Dimensions = Tuple Int Int

type Board =
  { snakes :: Array Snake
  , food :: Point
  , dimensions :: Dimensions
  }

data Game b t c p
  = Started c b
  | Playing t b
  | Paused t b p
  | Ended b

type Game' = Game Board Int Int String

directionToPoint :: Direction -> Point
directionToPoint Left  = Tuple (-1) 0
directionToPoint Right = Tuple 1 0
directionToPoint Up    = Tuple 0 (-1)
directionToPoint Down  = Tuple 0 1

heads :: Array OrientedPoint
heads =
  [ (Tuple (Tuple 6 2) Right)
  , (Tuple (Tuple 5 8) Left)
  , (Tuple (Tuple 2 4) Up)
  , (Tuple (Tuple 7 7) Down)
  ]

grid :: Dimensions -> Grid
grid (Tuple width height) = map (\x -> map (\y -> Tuple x y) h) w
  where
    w = range 0 $ width - 1
    h = range 0 $ height - 1

point :: OrientedPoint -> Point
point = fst

direction :: OrientedPoint -> Direction
direction = snd

bodyA :: Body -> Array OrientedPoint
bodyA (NE.NonEmpty head body) = cons head body

concatSnakes :: Array Snake -> Array Point
concatSnakes = concatMap (map point <<< bodyA <<< body)
  where
    body = \{ body } -> body

inDimensions :: Dimensions -> Point -> Point
inDimensions (Tuple w h) (Tuple x y)
  | x == -1   = Tuple (w - 1) y
  | x == w    = Tuple 0 y
  | y == -1   = Tuple x (h - 1)
  | y == h    = Tuple x 0
  | otherwise = (Tuple x y)

nextHead :: Dimensions -> Snake -> OrientedPoint
nextHead dimensions { body: (NE.NonEmpty head _), direction } = Tuple head' direction
  where
    head' = inDimensions dimensions $ (point head) + (directionToPoint direction)

grow :: Boolean -> Body -> Body
grow true  body = body
grow false (NE.NonEmpty head body) = NE.NonEmpty head $ unsafePartial Partial.init body

moveSnake :: (Boolean -> Body -> Body) -> Dimensions -> Point -> Snake -> Snake
moveSnake grow dimensions food snake =
  if snake.live
  then snake { body = body', ate = ate }
  else snake
  where
    head' = nextHead dimensions snake
    ate = food == point head'
    body' = grow ate $ f head' snake.body -- cons head' snake.body
    f :: OrientedPoint -> Body -> Body
    f head' (NE.NonEmpty head body) = NE.NonEmpty head' $ cons head body

initSnake :: Dimensions -> Int -> OrientedPoint -> Snake
initSnake dimensions length (Tuple head direction) =
  foldl (flip $ moveSnake fakeGrow dimensions) snake $ replicate (length - 1) fakeFood
  where
    fakeGrow = \_ -> id
    fakeFood = Tuple (-5) (-5)
    snake =
      { body: NE.NonEmpty (Tuple head direction) []
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
    empty = concatMap (filter (\p -> not $ member p occupied)) $ grid dimensions

initBoard :: forall eff. Dimensions -> Int -> Eff (random :: RANDOM | eff) Board
initBoard dimensions count = do
  food <- nextFood dimensions snakes
  pure
    { snakes: snakes
    , food: food
    , dimensions: dimensions
    }
  where
    snakes = map (initSnake dimensions 3) $ take count heads

moveSnakes :: forall eff. Board -> Eff (eff) Board
moveSnakes board = pure $ board { snakes = snakes }
  where
    snakes = map (moveSnake grow board.dimensions board.food) board.snakes

checkHits :: forall eff. Board -> Eff (eff) Board
checkHits board = pure $ board { snakes = snakes }
  where
    snakes = map hit board.snakes
    hit :: Snake -> Snake
    hit snake = snake { live = not dead }
      where
        head' = point $ NE.head snake.body
        dead = not snake.live || any hit' board.snakes
        hit' :: Snake -> Boolean
        hit' { body, live } = live && (member head' $ fromFoldable $ map point body')
          where
            body' =
              if snake.body == body
              then NE.tail $ body
              else bodyA body

addFood :: forall eff. Board -> Eff (random :: RANDOM | eff) Board
addFood board =
  if ate && (length $ concatSnakes board.snakes) < dimensions
  then
    do
      food <- nextFood board.dimensions board.snakes
      pure $ board { food = food }
  else pure $ board
  where
    ate = any (\{ ate } -> ate) board.snakes
    dimensions = (fst board.dimensions) * (snd board.dimensions)

end :: Board -> Boolean
end board = full || allDead
  where
    dimensions = (fst board.dimensions) * (snd board.dimensions)
    full = (length $ concatSnakes board.snakes) == dimensions
    allDead = all (\{ live } -> not live) board.snakes

tick :: forall eff. Game' -> Eff (random :: RANDOM | eff) Game'
tick (Started tick board) =
  pure
    if (tick - 1) == 0
    then Playing 0 board
    else Started (tick - 1) board
tick (Playing tick board) = do
  board' <- moveSnakes board >>= checkHits >>= addFood
  pure
    if end board'
    then Ended board'
    else Playing (tick + 1) board'
tick game = pure game

initGame :: forall eff. Int -> Int -> Int -> Eff (random :: RANDOM | eff) Game'
initGame width height count = do
  board <- initBoard (Tuple width height) count
  pure $ Started 5 board

pause :: String -> Game' -> Game'
pause player (Playing tick' board) = Paused tick' board player
pause _ game = game

resume :: String -> Game' -> Game'
resume player (Paused tick' board player')
  | player == player' = Playing tick' board
  | otherwise = Paused tick' board player'
resume _ game = game

switchPause :: String -> Game' -> Game'
switchPause player (Playing tick' board) = pause player (Playing tick' board)
switchPause player (Paused tick' board player') = resume player (Paused tick' board player')
switchPause _ game = game

codeToDirection :: Int -> Direction
codeToDirection 0 = Left
codeToDirection 1 = Right
codeToDirection 2 = Up
codeToDirection _ = Down

directionToCode :: Direction -> Int
directionToCode Left  = 0
directionToCode Right = 1
directionToCode Up    = 2
directionToCode Down  = 3

updateDirection :: Direction -> Int -> Int -> Game'  -> Game'
updateDirection direction' index tick (Playing tick' board) =
  Playing tick' board { snakes = fromMaybe [] snakes' }
    where
      snakes' = alterAt index (\snake -> Just snake { direction = beNice (headDirection snake) direction' }) board.snakes
      headDirection :: Snake -> Direction
      headDirection snake = direction $ NE.head snake.body
      beNice :: Direction -> Direction -> Direction
      beNice Left Right = Left
      beNice Right Left = Right
      beNice Up Down = Up
      beNice Down Up = Down
      beNice _ direction = direction
updateDirection _ _ _ game = game

encodeSnake :: Tuple Int Snake -> Array (Array Int)
encodeSnake (Tuple index { body, live }) = concat [header, body', [tail']]
  where
    head' = point $ NE.head body
    directions = group $ map snd $ bodyA body
    header =
      [ [if live then 1 else 0, index]
      , [(fst head'), (snd head')]
      ]
    body' = map f directions
    f :: NE.NonEmpty Array Direction -> Array Int
    f (NE.NonEmpty h t) = [directionToCode h, length t + 1]
    tail' = unsafePartial Partial.head $ reverse body'

encode' :: Int -> Int -> Board ->
  { header :: Array Int
  , snakes :: Array (Array (Array Int))
  }
encode' state tick { snakes, dimensions: (Tuple width height), food: (Tuple x y) } =
  { header:
     [ state
     , tick
     , width
     , height
     , x
     , y
     ]
  , snakes: map encodeSnake $ mapWithIndex (\i s -> Tuple i s) $ snakes
  }

encode :: Game' -> { header :: Array Int, snakes :: Array (Array (Array Int)) }
encode (Started tick board)  = encode' 0 tick board
encode (Playing tick board)  = encode' 1 tick board
encode (Paused tick board _) = encode' 2 tick board
encode (Ended board)         = encode' 3 0    board
