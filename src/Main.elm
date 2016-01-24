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
  , ctrl : Bool
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
type Enemy = Enemy Point Form (Float -> (List Enemy))

forwardEnemyCreate : Point -> List Enemy
forwardEnemyCreate pos =
  (straightEnemyUpdate pos 0 -150 0)

explodingEnemyCreate : Point -> Float -> Float -> List Enemy
explodingEnemyCreate pos dx dy =
  explodingEnemyUpdate pos dx dy 100 0

explodingEnemyUpdate : Point -> Float -> Float -> Int -> Float -> List Enemy
explodingEnemyUpdate pos dx dy ticks delta =
  let
    newpos = { x = pos.x + dx * delta, y = pos.y + dy * delta }
    newSprite = filled Color.yellow (ngon 3 10)
    explodedEnemies = (List.concat [ (straightEnemyUpdate newpos -100  100 0)
                                   , (straightEnemyUpdate newpos -100 -100 0)
                                   , (straightEnemyUpdate newpos  100 -100 0)
                                   , (straightEnemyUpdate newpos  100  100 0)
                                   ])
  in
    if (ticks <= 0) then (List.concat [[Enemy newpos newSprite (explodingEnemyUpdate newpos dx dy 100)], explodedEnemies]) else
      [Enemy newpos newSprite (explodingEnemyUpdate newpos dx dy (ticks - 1))]

straightEnemyUpdate : Point -> Float -> Float -> Float -> List Enemy
straightEnemyUpdate pos dx dy delta =
  let
    newpos = { x = pos.x + dx * delta
             , y = pos.y + dy * delta
             }
    newSprite = filled Color.red (rect 10 10)
  in
    [ Enemy newpos newSprite (straightEnemyUpdate newpos dx dy) ]

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

circleBulletCreate : Point -> Time.Time -> List BUpdater
circleBulletCreate pos t =
  let
    dx = sin (t / 10)
    dy = cos (t / 10)
  in
    [ straightBulletUpdate pos (dx * 150) (dy * 150) 0, straightBulletUpdate pos (dy * 150) (dx * 150) 0 ]

type AttackType
  = StraightAttack
  | DiagonalAttack
  | SinAttack
  | CircleAttack

type alias Game =
  { status : GameStatus
  , attackType : AttackType
  , player : GameObject
  , playerBullets : List BUpdater
  , ts : Time.Time
  , enemies : List Enemy
  , prevCtrl : Bool
  }

delta : Signal (Time.Time, Time.Time)
delta =
  (Time.timestamp (Signal.map Time.inSeconds (Time.fps 30)))

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map5 Input
      Keyboard.arrows
      Keyboard.wasd
      delta
      Keyboard.space
      Keyboard.ctrl

defaultGame : Game
defaultGame =
  { status = Playing
  , player = { pos = { x = 0, y = 0 }
             , c = black
             }
  , playerBullets = []
  , ts = 0
  , enemies = []
  , attackType = StraightAttack
  , prevCtrl = False
  }

stepPlayer : Input -> GameObject -> GameObject
stepPlayer i player =
  let
    arrows = i.arrows
    pos = player.pos
    delta = (snd i.delta)
    dx = if arrows.x < 0 then -175.0 else if arrows.x > 0 then 175.0 else 0.0
    dy = if arrows.y < 0 then -175.0 else if arrows.y > 0 then 100.0 else 0.0
    newx = clamp -halfWidth halfWidth (pos.x + dx * delta)
    newy = clamp -halfHeight halfHeight (pos.y + dy * delta)
    pos' = {pos | x = newx, y = newy }
  in
    { player | pos = pos' }

stepBullets : Input -> List BUpdater -> List BUpdater
stepBullets i bullets =
    List.map (\updater -> (case updater of BUpdater _ update -> update (snd i.delta))) bullets

addBullets : Bool -> Point -> Time.Time -> AttackType -> List BUpdater
addBullets space pos t attack =
  if space then
    case attack of
    StraightAttack -> (forwardBulletCreate pos)
    DiagonalAttack -> (diagonalBulletCreate pos)
    SinAttack -> (sineBulletCreate pos t)
    CircleAttack -> (circleBulletCreate pos t)
  else []

stepEnemy : Input -> Enemy -> List Enemy
stepEnemy i enemy =
  case enemy of
    Enemy pos shape updater -> (updater (snd i.delta))

stepEnemies : Input -> Game -> List Enemy
stepEnemies i game =
  let
    enemystepper = stepEnemy i
  in
    List.concatMap enemystepper game.enemies

addEnemies : Input -> List Enemy
addEnemies i =
  let
    ts = (fst i.delta)
    spawnStraight  = ((round (ts / 10.0)) % 4) == 0
    spawnExploding = ((round (ts / 10.0)) % 5) == 0
    straightEnemies = if spawnStraight then (forwardEnemyCreate {x = (sin (fst i.delta)) * halfWidth, y = halfHeight}) else []
    explodingEnemies = if spawnExploding then (explodingEnemyCreate {x = (sin (fst i.delta)) * halfWidth, y = halfHeight} 0 -50) else []
  in
    List.concat [explodingEnemies, straightEnemies]

dist : Point -> Point -> Float
dist p1 p2 =
  let
    dx = p1.x - p2.x
    dy = p1.y - p2.y
  in
    dx*dx + dy*dy

checkEnemyBulletCollision : Enemy -> List BUpdater -> Bool
checkEnemyBulletCollision enemy bullets =
  let
    enemyPos = case enemy of Enemy pos _ _ -> pos
  in
    List.any (\bullet -> case bullet of BUpdater bulletPos _ -> ((dist enemyPos bulletPos) < 75)) bullets

checkBulletEnemyCollision : BUpdater -> List Enemy -> Bool
checkBulletEnemyCollision bullet enemies =
  let
    bulletPos = case bullet of BUpdater pos _ -> pos
  in
    List.any (\enemy -> case enemy of Enemy enemyPos _ _ -> ((dist enemyPos bulletPos) < 75)) enemies

checkBulletCollisions : List Enemy -> List BUpdater -> (List Enemy, List BUpdater)
checkBulletCollisions enemies bullets =
  let
    filteredEnemies = List.filter (\enemy -> not (checkEnemyBulletCollision enemy bullets)) enemies
    filteredBullets =  List.filter (\bullet -> not (checkBulletEnemyCollision bullet enemies)) bullets
  in
    (filteredEnemies, filteredBullets)

filterOOB : List Enemy -> List BUpdater -> (List Enemy , List BUpdater)
filterOOB enemies bullets =
  ( List.filter (\enemy -> case enemy of Enemy pos _ _ -> (pos.x > -halfWidth - 100) && (pos.x < halfWidth + 100) && (pos.y > -halfHeight - 100) && (pos.y < halfHeight + 100)) enemies
  , List.filter (\bullet -> case bullet of BUpdater pos _ -> (pos.x > -halfWidth - 100) && (pos.x < halfWidth + 100) && (pos.y > -halfHeight - 100) && (pos.y < halfHeight + 100)) bullets
  )

getNextAttack : AttackType -> AttackType
getNextAttack attack =
  case attack of
    StraightAttack -> DiagonalAttack
    DiagonalAttack -> SinAttack
    SinAttack -> CircleAttack
    CircleAttack -> StraightAttack

stepGame : Input -> Game -> Game
stepGame i game =
  let
    player' = stepPlayer i game.player
    attackType' = if (i.ctrl &&  (not game.prevCtrl)) then (getNextAttack game.attackType) else game.attackType
    addedBullets = addBullets i.space game.player.pos (fst i.delta) attackType'
    playerBullets' = List.append addedBullets (stepBullets i game.playerBullets)
    addedEnemies = addEnemies i
    enemies' =  List.append (stepEnemies i game) addedEnemies
    (aliveEnemies', aliveBullets') = checkBulletCollisions enemies' playerBullets'
    (inboundsEnemies', inboundsBullets') = filterOOB aliveEnemies' aliveBullets'
  in
    {game | player = player', playerBullets = inboundsBullets', ts = (fst i.delta), enemies = inboundsEnemies', attackType = attackType', prevCtrl = i.ctrl}

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

renderEnemies : List Enemy -> Form
renderEnemies enemies =
  group (List.map (\enemy -> case enemy of Enemy pos shape _ -> move (pos.x, pos.y) shape) enemies)

render : Game -> Element
render game =
  collage gameWidth gameHeight
    [ (renderBackground game.ts) , (renderEnemies game.enemies), (renderPlayer game.player), (renderPlayerBullets game.playerBullets) ]

main =
  Signal.map render gameState
