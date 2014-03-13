package com.kyrlach.peli

import scala.language.reflectiveCalls
import scala.language.postfixOps
import scalaz.Lens
import scalaz.Reader
import scalaz.concurrent._
import scalaz.effect.IO
import scalaz._
import Scalaz._
import scalaz.stream._
import java.io.InputStream

sealed trait RunState
case object Paused extends RunState
case object Running extends RunState

case class Rect(x: Double, y: Double, w: Double, h: Double)

trait BoundedEntity {
  def id: String
  def bounds: Rect
}

trait GameEntity extends BoundedEntity with Framed with Pen

case class Body(id: String, bounds: Rect, vx: Double, vy: Double) extends GameEntity with Collision with Gravity with Physics
case class Action(bottom: Body => Body = identity, right: Body => Body = identity, top: Body => Body = identity, left: Body => Body = identity, any: Body => Body = identity)

trait State

case class Board(bounds: Rect, color: String)

trait Channel
trait Key

trait Pattern

trait Context {
  def fillStyle(fill: String): Unit
  def fillStyle(pattern: Pattern): Unit
  def createPattern(fill: String, style: String): Pattern
  def beginPath(): Unit
  def moveTo(x: Double, y: Double): Unit
  def lineTo(x: Double, y: Double): Unit
  def quadraticCurveTo(a: Double, b: Double, c: Double, d: Double)
  def closePath: Unit
  def fill: Unit
  def fillRect(bounds: Rect): Unit
  def strokeStyle(style: String): Unit
  def lineWidth(w: Int): Unit
  def strokeRect(bounds: Rect): Unit
  def textBaseLine(position: String): Unit
  def font(fontFace: String): Unit 
  def measureText(text: String): Int
  def fillText(text: String, x: Double, y: Double)
}

case class World(board: Board, frame: Rect, entities: List[GameEntity], keyActions: List[(Key, World) => World], runState: RunState)
case class Game(worlds: List[World], activeWorld: World, state: List[Nothing])

trait Pen { self: GameEntity =>
  def draw(ctx: Context, frame: Rect): GameEntity = this
}

trait Framed { self: GameEntity =>
  def inFrame(frame: Rect): Boolean = Engine.overlap(this.bounds, frame)
}

trait Gravity { self: Body =>
  def gravity: Body = this
}

trait Physics { self: Body =>
  def physics(timeDiff: Int, board: Board): Body = this
}

trait Collision { self: Body =>
  def collide(body: Body): Body = this
}

case class Options(backgroundColor: String = "#cfcfcf", borderColor: String = "#000099", font: String = "12px Arial", fontColor: String = "#000099")

case class Block(id: String, fill: String, bounds: Rect, radii: (Int,Int,Int,Int)) extends GameEntity {
  override def draw(ctx: Context, frame: Rect): GameEntity = {
    val Rect(x,y,w,h) = Engine.translateCoords(frame).exec(bounds)    
    val r = x + w
    val b = y + h
    val (ul,ur,lr,ll) = radii
    val result = for {
      ctx <- IO(ctx)
      _ <- IO(ctx.fillStyle(fill))
      p <- IO(ctx.createPattern(fill, "repeat"))
      _ <- IO(ctx.fillStyle(p))
      _ <- IO(ctx.beginPath)
      _ <- IO(ctx.moveTo(x + ul, y))
      _ <- IO(ctx.lineTo(r - ur, y))
      _ <- IO(ctx.quadraticCurveTo(x, b, x, b - ll))
      _ <- IO(ctx.lineTo(r, b - lr))
      _ <- IO(ctx.quadraticCurveTo(r, b, r - lr, b))
      _ <- IO(ctx.lineTo(x +ll, b))
      _ <- IO(ctx.quadraticCurveTo(x, b, x, b - ll))
      _ <- IO(ctx.lineTo(x, y + ul))
      _ <- IO(ctx.quadraticCurveTo(x, y, x + ul, y))
      _ <- IO(ctx.closePath)
      _ <- IO(ctx.fill)
    } yield {}  
    result.unsafePerformIO
    this
  }
}

case class TextPrompt(id: String, bounds: Rect, text: String, hidden: Boolean, options: Options) extends GameEntity with Pen with Framed {
  override def draw(ctx: Context, frame: Rect): GameEntity = {
    if(!hidden) {
      val result = for {
        ctx <- IO(ctx)
        x <- IO((frame.w - bounds.w) / 2)
        y <- IO((frame.h - bounds.h) / 2)
        _ <- IO(ctx.fillStyle(options.backgroundColor))
        _ <- IO(ctx.fillRect(bounds))
        _ <- IO(ctx.strokeStyle(options.borderColor))
        _ <- IO(ctx.lineWidth(2))
        _ <- IO(ctx.strokeRect(bounds))
        _ <- IO(ctx.textBaseLine("middle"))
        _ <- IO(ctx.font(options.font))
        _ <- IO(ctx.fillStyle(options.fontColor))
        tx <- IO(bounds.x + ((bounds.w / 2)  - (ctx.measureText(text) / 2))) 
        ty <- IO(bounds.y + (bounds.h / 2))
        _ <- IO(ctx.fillText(text, tx, ty))
      } yield {}
      result.unsafePerformIO      
    }
    this
  } 
  
  override def inFrame(frame: Rect): Boolean = true
}

object Engine {
  val bw = Lens.lensu[Tuple1[Rect], Rect]((_, r) => Tuple1(r), _._1)
  val body_bounds = Lens.lensu[Body, Rect]((b, r) => b.copy(bounds = r), _.bounds)

  def overlap(r1: Rect, r2: Rect): Boolean = {
    val Rect(x1, y1, w1, h1) = r1
    val Rect(x2, y2, w2, h2) = r2
    val (lx1, ly1) = (x1 + w1) -> (y1 + h1)
    val (lx2, ly2) = (x2 + w2) -> (y2 + h2)
    lx1 > x2 && x1 <= lx2 && ly1 > y2 && y1 <= ly2
  }

  def checkBounds(n: Double, offset: Double, min: Double, max: Double): Double = {
    val no = n + offset
    if (n < min) min else if (no > max) n - (no - max) else n
  }

  val rect_x = Lens.lensu[Rect, Double]((r, x) => r.copy(x = x), _.x)
  val rect_y = Lens.lensu[Rect, Double]((r, y) => r.copy(y = y), _.y)
  val rect_w = Lens.lensu[Rect, Double]((r, w) => r.copy(w = w), _.w)
  val rect_h = Lens.lensu[Rect, Double]((r, h) => r.copy(h = h), _.h)

  val body_vx = Lens.lensu[Body, Double]((body, vx) => null, _.vx)
  val body_vy = Lens.lensu[Body, Double]((body, vy) => null, _.vy)

  def translateCoords(frame: Rect): scalaz.StateT[scalaz.Id.Id, Rect, (Double, Double)] = {
    val Rect(fx,fy,_,_) = frame;
    for {
      x <- rect_x -= fx
      y <- rect_y -= fy
    } yield (x, y)
  }

  def applyGravity: scalaz.StateT[scalaz.Id.Id, Body, (Double)] = for {
    vy <- body_vy %= (vy => if (vy > -5) vy - 0.2 else vy)
  } yield (vy)
  
  val board_bounds = Lens.lensu[Board, Rect]((b, r) => b.copy(bounds = r), _.bounds)

  def applyPhysics(timeDiff: Int, board: Board): scalaz.StateT[scalaz.Id.Id, Body, (Double, Double)] = {
    val Rect(_,_,bw,bh) = board.bounds
    for {
      vx <- body_vx
      vy <- body_vy
      w <- body_bounds >=> rect_w
      h <- body_bounds >=> rect_h
      x <- body_bounds >=> rect_x %= (x => checkBounds((x + vx), w, 0, bw))
      y <- body_bounds >=> rect_y %= (y => checkBounds((y + vy), h, 0, bh))
    } yield (x, y)
  }
  
  def collideAction(body1: Body, body2: Body, action: Action): Body = {
    val Rect(x, y, w, h) = body1.bounds
    val (vx, vy) = (body1.vx, body1.vy)
    val Rect(x2,y2,w2,h2) = body2.bounds
    val (vx2, vy2) = (body2.vx, body2.vy)
    val ox = x - vx
    val oy = y + vy
    val ox2 = x2 - vx2
    val oy2 = y2 + vy2
    val (lx, ly) = (ox + w) -> (oy + h)
    val (lx2, ly2) = (ox2 + w2) -> (oy2 + h2)
    if(ly <= oy2) action.bottom(body1) else
    if(lx <= ox2) action.right(body1) else
    if(oy >= ly2) action.top(body1) else
    if(ox >= lx2) action.left(body1) else
    if(ly <= oy2 ||
       lx <= ox2 ||
       oy >= ly2 ||
       ox >= lx2) action.any(body1)
    body1
  }
  
  def collideSolid(body1: Body, body2: Body): Body = {
    val Rect(x, y, h, w) = body1.bounds
    val Rect(x2,y2,h2,w2) = body2.bounds    
    def bottom = for {
      _ <- Engine.body_bounds >=> Engine.rect_y := (y2 - h - 0.1)
      _ <- Engine.body_vy := 0d 
    } yield {}
    def right = for {
      _ <- Engine.body_bounds >=> Engine.rect_x := (x2 - w - 0.1)
    } yield {}
    def top = for {
      _ <- Engine.body_bounds >=> Engine.rect_y := (y2 - h - 0.1)
      _ <- Engine.body_vy := 0d
    } yield {}
    def left = for {
      _ <- Engine.body_bounds >=> Engine.rect_x := (x2 + w2 + 0.1)
    } yield {}
    collideAction(body1, body2, new Action(
        bottom = bottom.exec,
        right = right.exec,
        top = top.exec,
        left = left.exec))
  }
  
  val world_bounds = Lens.lensu[World, Rect]((w, r) => w.copy(frame = r), _.frame)
  val world_entities = Lens.lensu[World, List[GameEntity with Framed with Pen]]((w, entities) => w.copy(entities = entities), _.entities)
  val world_board = Lens.lensu[World, Board]((w, b) => w.copy(board = b), _.board)
  val board_color = Lens.lensu[Board, String]((b,c) => b.copy(color = c), _.color)

  def removeEntity(id: String) = for {
    bodies <- world_entities %== ((entities: List[GameEntity]) => entities.filterNot(_.id == id))
  } yield {}
 
  def playSound(id: String): IO[Unit] = ??? //(.play (.getElementById js/document id)))

  def handleCollision(body: Body, bodies: List[Body]): Body = {
    bodies.filter(b => b.id != body.id && overlap(body.bounds, b.bounds))
      .foldLeft(body)((bresult, bcurrent) => bresult.collide(bcurrent))
  }

  def runPhysics(world: World): World = {
    val Rect(fx,fy,fw,fh) = world.frame
    val nx = fx - (fw / 2)
    val nw = fw * 2
    val ny = fy - (fh / 2)
    val nh = fh * 2
    val expandedBounds = Rect(nx, ny, nw, nh)
    val parts = world.entities
      .filter(_.inFrame(expandedBounds))
      .partition(_ match {
        case g: GameEntity with Gravity with Physics => true
        case _ => false
      })
    val afterPhysics = parts._1.flatMap(_ match {
      case g: GameEntity with Gravity with Physics => Some(g)
      case _ => None
    }).map(((b: Body) => b.gravity).compose(b => b.physics(0, world.board))) ++ parts._2
    val newParts = afterPhysics.partition(_ match {
      case b: Body => true
      case _ => false
    })
    val bodies = newParts._1
      .flatMap(_ match {
        case b: Body => Some(b)
        case _ => None
      })    
    world_entities.set(world, bodies.map(handleCollision(_, bodies.filter(_.inFrame(expandedBounds)))) ++ newParts._2)
  }
  
  def drawWorld(world: World, ctx: Context): IO[World] = for {
    ctx <- IO(ctx)
    _ <- IO(ctx.fillStyle((world_board >=> board_color).get(world)))
    _ <- IO(ctx.fillRect(Rect(0, 0, (world_bounds >=> rect_w).get(world), (world_bounds >=> rect_h).get(world))))
    world <- IO(world_entities.mod(_.filter(_.inFrame(world.frame)).map(_.draw(ctx, world.frame)), world))
  } yield world
  
  def getKeyCode(event: Nothing): Int = ???
//  (let [e (if event event (.-event js/window))
//        code (.-keyCode e)]
//    (if (and (.-charCode e) (= code 0))
//      (.-charCode e)
//      code)))
  
  def handleKeys(actions: Action): Nothing = ???
//  (let [on-down (set (filter #(:on-down (get actions %)) (keys actions)))
//        on-up (set (filter #(:on-up (get actions %)) (keys actions)))]
//    (set! (.-onkeydown js/document) 
//       #(let [code (get-key-code %)]
//          (if (on-down code)
//            (put! ch {:action :edit-world 
//                      :fn (get-in actions [code :on-down])}))))
//    (set! (.-onkeyup js/document) 
//       #(let [code (get-key-code %)]
//          (if (on-up code)
//            (put! ch {:action :edit-world
//                      :fn (get-in actions [code :on-up])}))))))
  
  def adjustFrame(world: World): World = {
    val Rect(x,y,w,h) = world.frame
    val dw = w * .2 // || buffer ???
    val dh = h * .2 // || buffer
    val tx = x + dw
    val lx = (x + w) - dw
    val ty = y + dh
    val ly = (y + h) - dh
    val Rect(hx, hy, _, _) = world.entities.head.bounds        
    val newWorld = world_bounds.mod(f => 
      if(hx < tx) rect_x.set(f, x - (tx - hx)) else
      if(hx > lx) rect_x.set(f, x + (hx - lx)) else
      if(hy < ty) rect_y.set(f, y - (ty - hy)) else
      if(hy > ly) rect_y.set(f, y + (hy - ly))
      else f
    , world)
    val xy = (rect_x.set(_: Rect, checkBounds(x, w, 0, newWorld.board.bounds.w)))
      .compose(rect_y.set(_: Rect, checkBounds(y, w, 0, newWorld.board.bounds.h)))
    world_bounds.mod(xy, newWorld)
  }
  
  def gameLoop(ctx: Context): List[IO[World => World]] = List(
    IO(runPhysics),
    IO(adjustFrame),
    IO(drawWorld(_: World, ctx).unsafePerformIO)
  )
  
  def drawAction(world: World, ctx: Context): IO[World] = world.runState match {
    case Paused => gameLoop(ctx).sequence.map(_.tail.foldLeft(world)((ow,wf) => wf(ow)))
    case Running => gameLoop(ctx).sequence.map(_.foldLeft(world)((ow,wf) => wf(ow))) 
  }

  def scheduleEdit(): Nothing = {
    val a = async.queue[String]
    a._2.run(a._1.dequeue)
    ???
  }
//  ([f ch timing] (schedule-edit f ch timing nil))
//  ([f ch timing type]
//     (let [action (or type :edit-world)]
//       (go (<! (timeout timing))
//           (put! ch {:action action :fn f})))))
}
//))))
//
//
//(defn run-loop [ch game ctx world-id]
//  (put! ch {:action :switch-world :data world-id})
//  ;;25 FPS
//  (go 
//    (while true
//     (<! (timeout 25))
//     (put! ch {:action :draw-world})))
//  
//  ;;action loop
//  (go-loop [g game]
//    (let [msg (<! ch)] 
//      (case (:action msg)
//     (assoc g
//        :draw-world (recur
//                       :active-world
//                       (draw-action ch (:active-world g) (:state g) ctx)))
//        :edit-world (recur (assoc g
//                             :active-world
//                             ((:fn msg) (:active-world g))))
//        :edit-game (recur ((:fn msg) g))
//        :switch-world (recur (let [world (get-in g [:worlds (:data msg)])]
//                               (when (:key-actions world)
//                                 (handle-keys ch (:key-actions world)))
//                               (assoc g :active-world world)))
//        (recur g)))))
//
//(defn run-game [game canvas-id world-id]
//  (let [ch (chan)
//        can (.getElementById js/document canvas-id)
//        ctx (.getContext can "2d")]
//    (run-loop ch game ctx world-id)
//    ch))