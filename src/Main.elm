import Color exposing (Color, black)
import Graphics.Collage exposing (collage, move, ngon, filled, group, Form, rotate, circle)
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

type BUpdater = BUpdater Point (Float -> BUpdater)

forwardBulletCreate : Point -> List BUpdater
forwardBulletCreate pos =
  [ straightBulletUpdate pos 0 1 0 ]

diagonalBulletCreate : Point -> List BUpdater
diagonalBulletCreate pos =
  [ (straightBulletUpdate pos 1 1 0)
  , (straightBulletUpdate pos -1 1 0)
  ]

straightBulletUpdate : Point -> Float -> Float -> Float -> BUpdater
straightBulletUpdate pos dx dy delta =
  let
    newpos = { x = pos.x + dx * delta
             , y = pos.y + dy * delta
             }
  in
    BUpdater newpos (straightBulletUpdate pos dx dy)

sineBulletCreate : Point -> List BUpdater
sineBulletCreate pos =
    [ sineBulletUpdate pos pos 0 ]

sineBulletUpdate : Point -> Point -> Float -> BUpdater
sineBulletUpdate pos posInitial deltaT =
    let
      delta = deltaT*50
      newY = pos.y + delta*3
      deltaY = newY - posInitial.y
      newX = posInitial.x + 50*sin (deltaY/15)
      pos' = {x = newX, y = newY}
    in
      BUpdater pos' (sineBulletUpdate pos' posInitial)


type alias Game =
  { status : GameStatus
  , player : GameObject
  , playerBullets : List BUpdater
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
  , playerBullets = (sineBulletCreate { x = 0, y = 0 })
  }

stepPlayer : Input -> GameObject -> GameObject
stepPlayer input player =
  let
    arrows = input.arrows
    pos = player.pos
    dx = if arrows.x < 0 then -1.0 else if arrows.x > 0 then 1.0 else 0.0
    dy = if arrows.y < 0 then -1.0 else if arrows.y > 0 then 0.5 else 0.0
    pos' = {pos | x = pos.x + dx, y = pos.y + dy }
  in
    { player | pos = pos' }

stepBullets : Input -> List BUpdater -> List BUpdater
stepBullets input bullets =
    List.map (\updater -> (case updater of BUpdater pos update -> update input.delta)) bullets

stepBullet : Float -> BUpdater -> BUpdater
stepBullet delta bullet =
  case bullet of
    BUpdater pos updater -> updater delta

stepGame : Input -> Game -> Game
stepGame input game =
  let
    player' = stepPlayer input game.player
    playerBullets' = stepBullets input game.playerBullets
  in
    {game | player = player', playerBullets = playerBullets'}

gameState : Signal Game
gameState =
  Signal.foldp stepGame defaultGame input

renderPlayerBullets : List BUpdater -> Form
renderPlayerBullets bullets =
  let
    bullet = filled black (circle 5)
  in
    group (List.map (\updater -> (case updater of BUpdater pos _ -> (move (pos.x, pos.y) bullet))) bullets)

renderPlayer : GameObject -> Form
renderPlayer player =
  let
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
    [ (renderPlayer game.player), (renderPlayerBullets game.playerBullets) ]

main =
  Signal.map render gameState
