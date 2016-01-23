import Color exposing (Color, black)
import Graphics.Collage exposing (collage, move, ngon, filled, group, Form, rotate)
import Graphics.Element exposing (Element)
import Mouse
import Signal
import Window
import List
import Time
import Debug
import Text
import Keyboard
import Transform2D
import Html

gameWidth = 500
gameHeight = 500

type GameStatus
  = Dead
  | Playing

type alias Input =
  { arrows : {x : Int, y: Int}
  , wasd : {x : Int, y: Int}
  , delta : Time.Time
  }

type alias Point =
  { x : Float
  , y : Float
  }

type alias BoundingBox = (Point, Point)

type alias GameObject =
  { pos : Point
  , c : Color
  }

type alias Game =
  { status : GameStatus
  , player : GameObject
  }

delta : Signal Time.Time
delta =
  Signal.map Time.inSeconds (Time.fps 35)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map3 Input
      Keyboard.arrows
      Keyboard.wasd
      delta

defaultGame : Game
defaultGame =
  { status = Playing
  , player = { pos = { x = 0, y = 0 }
              , c = black
            }
  }

stepPlayer : Input -> Game -> GameObject
stepPlayer input game =
  let
    arrows = input.arrows
    player = game.player
    pos = player.pos
    dx = if arrows.x < 0 then -1.0 else if arrows.x > 0 then 1.0 else 0.0
    dy = if arrows.y < 0 then -1.0 else if arrows.y > 0 then 0.5 else 0.0
    pos' = {pos | x = pos.x + dx, y = pos.y + dy }
  in
    { player | pos = pos' }

stepGame : Input -> Game -> Game
stepGame input game =
  let
    player' = stepPlayer input game
  in
    {game | player = player'}

gameState : Signal Game
gameState =
  Signal.foldp stepGame defaultGame input

renderPlayer : Game -> Form
renderPlayer game =
  let
    player = game.player
    pos = player.pos
    c = player.c
    triangle = filled c (ngon 3 10)
    t1 = triangle
      |> rotate (degrees 90)
      |> move (pos.x, pos.y)
    t2 = triangle
      |> rotate (degrees 90)
      |> move (pos.x, pos.y - 5)
  in
    group [t1, t2]


render : Game -> Element
render game =
  collage gameWidth gameHeight
    [ (renderPlayer game) ]

main =
  Signal.map render gameState
