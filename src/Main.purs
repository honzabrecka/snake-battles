module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array (alterAt, any, concatMap, cons, elem, filter, length, range, unsafeIndex)
import Data.Array.Partial (head, init, tail)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, fromFoldable, member, unions)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

type P = Tuple Int Int

type Body = Array P

data Direction = Left | Right | Up | Down

type Snake =
  { body :: Body
  , direction :: Direction
  }

type Game =
  { snakes :: Array Snake
  , food :: P
  , end :: Boolean
  }

type Board = Array (Array P)

dec :: Int -> Int
dec = (-)1

boardSize :: Int
boardSize = 10

board :: Board
board = map (\x -> map (\y -> Tuple x y) r) r
  where
    r = range 0 $ dec boardSize

board' :: Board -> Set P
board' = unions <<< map fromFoldable

initSnake :: Body -> Direction -> Snake
initSnake body direction =
  { body: body
  , direction: direction
  }

initGame :: Game
initGame =
  { snakes: [ (initSnake [(Tuple 4 4)] Right)
            , (initSnake [(Tuple 6 6)] Left)
            ]
  , food: Tuple 7 8
  , end: false
  }

directionToP :: Direction -> P
directionToP Left  = Tuple (-1) 0
directionToP Right = Tuple 1 0
directionToP Down  = Tuple 0 (-1)
directionToP Up    = Tuple 0 1

safeHead :: Body -> P
safeHead body = unsafePartial head body

nextHead :: Snake -> P
nextHead { body, direction } = (safeHead body) + (directionToP direction)

removeTail :: Boolean -> Body -> Body
removeTail true  body = body
removeTail false body = unsafePartial init body

type NextSnake =
  { snake :: Snake
  , eat :: Boolean
  }

nextSnake :: P -> Snake -> NextSnake
nextSnake food snake =
  { snake: snake { body = removeTail eat $ cons newHead snake.body }
  , eat: eat
  }
  where
    newHead = nextHead snake
    eat = food == newHead

snakesToSet :: Array Snake -> Set P
snakesToSet = unions <<< map \snake -> fromFoldable snake.body

nextFood :: forall eff. Board -> Array Snake -> Eff (random :: RANDOM | eff) P
nextFood board snakes = do
  i <- randomInt 0 (length empty)
  pure $ unsafePartial unsafeIndex empty i
  where
    occupied = snakesToSet snakes
    empty = concatMap (filter (flip member occupied)) board

hit :: Array NextSnake -> NextSnake -> Boolean
hit snakes { snake } = any (elem head' <<< hit') snakes
  where
    head' = (safeHead snake.body)
    hit' :: NextSnake -> Body
    hit' { snake } =
      if head' == (safeHead snake.body)
      then unsafePartial tail snake.body
      else snake.body

getSnake :: NextSnake -> Snake
getSnake { snake } = snake

tick :: forall eff. Game -> Eff (random :: RANDOM | eff) Game
tick { snakes, food } = do
  food' <- if eat'
           then nextFood board (map getSnake snakes')
           else pure food
  pure { food: food'
       , snakes: map getSnake snakes'
       , end: false -- TODO
       }
  where
    snakes' = map (nextSnake food) snakes
    eat' = any (\{ eat } -> eat) snakes'
    hit' = any (hit snakes') snakes'

codeToDirection :: Int -> Direction
codeToDirection 0 = Left
codeToDirection 1 = Right
codeToDirection 2 = Up
codeToDirection _ = Down

updateDirection :: Int -> Int -> Game -> Game
updateDirection i code game = game { snakes = snakes'' }
  where
    direction' = codeToDirection code
    snakes' = alterAt i (\snake -> Just snake { direction = direction' }) game.snakes
    snakes'' = fromMaybe [] snakes'

---

encode :: Game -> String
encode game = "foo"


showGame :: Game -> String
showGame { snakes, food } = ""



main :: forall eff. Eff (random :: RANDOM | eff) Game
main = do
  tick initGame
