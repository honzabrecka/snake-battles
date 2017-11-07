module Main
  ( initBoard
  , tick
  , pause
  , resume
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
moveSnake grow dimensions food snake =
  if snake.live
  then snake { body = body', ate = ate }
  else snake
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

moveSnakes :: forall eff. Game' -> Eff (eff) Game'
moveSnakes (Playing tick board) = pure $ Playing tick board { snakes = snakes }
  where
    snakes = map (moveSnake grow board.dimensions board.food) board.snakes
moveSnakes game = pure game

checkHits :: forall eff. Game' -> Eff (eff) Game'
checkHits (Playing tick board) = pure $ Playing tick board { snakes = snakes }
  where
    snakes = map hit board.snakes
    hit :: Snake -> Snake
    hit snake = snake { live = not dead }
      where
        head' = head snake.body
        dead = not snake.live || any hit' board.snakes
        hit' :: Snake -> Boolean
        hit' { body } = member head' $ fromFoldable $ map point body'
          where
            body' =
              if snake.body == body
              then tail body
              else body
checkHits game = pure game

addFood :: forall eff. Game' -> Eff (random :: RANDOM | eff) Game'
addFood (Playing tick board) =
  if ate && (length $ concatSnakes board.snakes) < dimensions
  then
    do
      food <- nextFood board.dimensions board.snakes
      pure $ Playing tick board { food = food }
  else pure $ Playing tick board
  where
    ate = any (\{ ate } -> ate) board.snakes
    dimensions = (fst board.dimensions) * (snd board.dimensions)
addFood game = pure game

checkEnd :: forall eff. Game' -> Eff (eff) Game'
checkEnd (Playing tick board) =
  if full || allDead
  then pure $ Ended board
  else pure $ Playing tick board
  where
    dimensions = (fst board.dimensions) * (snd board.dimensions)
    full = (length $ concatSnakes board.snakes) == dimensions
    allDead = all (\{ live } -> not live) board.snakes
checkEnd game = pure game

tick :: forall eff. Game' -> Eff (random :: RANDOM | eff) Game'
tick (Playing tick board) =
  (pure $ (Playing (tick + 1) board))
    >>= moveSnakes
    >>= checkHits
    >>= addFood
    >>= checkEnd
tick game = pure game

initGame :: forall eff. Int -> Int -> Int -> Eff (random :: RANDOM | eff) Game'
initGame width height count = do
  board <- initBoard (Tuple width height) count
  pure $ Started 10 board

pause :: String -> Int -> Game' -> Game'
pause player tick (Playing tick' board) =
  if tick == tick'
  then Paused tick' board player
  else Playing tick' board
pause _ _ game = game

resume :: String -> Int -> Game' -> Game'
resume player tick (Paused tick' board player') =
  if tick == tick' && player == player'
  then Playing tick' board
  else Paused tick' board player'
resume _ _ game = game

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
updateDirection direction index tick (Playing tick' board)
  | tick == tick' = Playing tick' board { snakes = fromMaybe [] snakes' }
    where
      snakes' = alterAt index (\snake -> Just snake { direction = direction }) board.snakes
  | otherwise = Playing tick' board
updateDirection _ _ _ game = game

encodeSnake :: Tuple Int Snake -> Array (Array Int)
encodeSnake (Tuple index { body, live }) = concat [header, body', [tail']]
  where
    head' = head body
    directions = group $ map snd body
    header =
      [ [if live then 1 else 0, index]
      , [(fst head'), (snd head')]
      ]
    body' = map f directions
    f :: NonEmpty Array Direction -> Array Int
    f (NonEmpty h t) = [directionToCode h, length t + 1]
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
