import Color exposing (Color, black)
import Graphics.Collage exposing (collage, move, ngon, filled, group, Form, rotate, circle, rect)
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
halfWidth = gameWidth/2
halfHeight = gameHeight/2

type GameStatus
  = Dead
  | Playing

type alias Input =
  { arrows : {x : Int, y: Int}
  , wasd : {x : Int, y: Int}
  , delta : (Time.Time, Time.Time)
  , space : Bool
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
  [ straightBulletUpdate pos 0 150 0 ]

diagonalBulletCreate : Point -> List BUpdater
diagonalBulletCreate pos =
  [ (straightBulletUpdate pos 150 150 0)
  , (straightBulletUpdate pos -150 150 0)
  ]

straightBulletUpdate : Point -> Float -> Float -> Float -> BUpdater
straightBulletUpdate pos dx dy delta =
  let
    newpos = { x = pos.x + dx * delta
             , y = pos.y + dy * delta
             }
  in
    BUpdater newpos (straightBulletUpdate newpos dx dy)

sineBulletCreate : Point -> Time.Time -> List BUpdater
sineBulletCreate pos t =
    [ sineBulletUpdate pos pos t 0 ]

sineBulletUpdate : Point -> Point -> Time.Time -> Float -> BUpdater
sineBulletUpdate pos posInitial initialT deltaT =
    let
      delta = deltaT*50
      newY = pos.y + delta*3
      deltaY = newY - posInitial.y
      newX = posInitial.x + 50* sin((deltaY/15.0) + (initialT/4.7))
      pos' = {x = newX, y = newY}
    in
      BUpdater pos' (sineBulletUpdate pos' posInitial initialT)


type alias Game =
  { status : GameStatus
  , player : GameObject
  , playerBullets : List BUpdater
  , ts : Time.Time
  }

delta : Signal (Time.Time, Time.Time)
delta =
  (Time.timestamp (Signal.map Time.inSeconds (Time.fps 30)))

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
      Keyboard.arrows
      Keyboard.wasd
      delta
      Keyboard.space

defaultGame : Game
defaultGame =
  { status = Playing
  , player = { pos = { x = 0, y = 0 }
             , c = black
             }
  , playerBullets = []
  , ts = 0
  }

stepPlayer : Input -> GameObject -> GameObject
stepPlayer i player =
  let
    arrows = i.arrows
    pos = player.pos
    delta = (snd i.delta)
    dx = if arrows.x < 0 then -75.0 else if arrows.x > 0 then 75.0 else 0.0
    dy = if arrows.y < 0 then -75.0 else if arrows.y > 0 then 50.0 else 0.0
    newx = clamp -halfWidth halfWidth (pos.x + dx * delta)
    newy = clamp -halfHeight halfHeight (pos.y + dy * delta)
    pos' = {pos | x = newx, y = newy }
  in
    { player | pos = pos' }

stepBullets : Input -> List BUpdater -> List BUpdater
stepBullets i bullets =
    List.map (\updater -> (case updater of BUpdater _ update -> update (snd i.delta))) bullets

addBullets : Bool -> Point -> Time.Time -> List BUpdater
addBullets space pos t =
  if space then (sineBulletCreate pos t) else []

stepGame : Input -> Game -> Game
stepGame i game =
  let
    player' = stepPlayer i game.player
    addedBullets = addBullets i.space game.player.pos (fst i.delta)
    playerBullets' = List.append addedBullets (stepBullets i game.playerBullets)
    -- playerBullets'' = cleanUpBullets playerBullets
  in
    {game | player = player', playerBullets = playerBullets', ts = (fst i.delta)}

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

renderBackground : Time.Time -> Form
renderBackground t =
  let
    background = filled Color.green (rect gameWidth gameHeight)
    stripe = filled Color.grey (rect gameWidth (gameHeight/20.0))
  in
    group (background :: (List.map (\i -> (move (0.0, ((i - 21.0) * 40 + gameHeight - (toFloat ((round (t/5.0)) % gameHeight//2)))) stripe)) [1..20]))


render : Game -> Element
render game =
  collage gameWidth gameHeight
    [ (renderBackground game.ts) , (renderPlayer game.player), (renderPlayerBullets game.playerBullets) ]

main =
  Signal.map render gameState
