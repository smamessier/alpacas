package com.uber.atcp.dsl.mcs

import com.uber.atcp.dsl._
import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.transition._
import com.uber.atcp.dsl.assertion._

import scala.language.implicitConversions
import scala.annotation.tailrec
import com.uber.atcp.dsl.ast.Expr.Fvar

/** A generator of nested series/parallel systems, 
  * with optional shortcut connections from leaf component to leaf component. 
  * */
object SeriesParallel {

  /** Parallel system made of series system or leaf system. */
  sealed trait PL[A] {
    /** Generates variants of the system obtained by inserting y in all possible places in the hierarchy. */
    def insert(y: A): List[PL[A]]
  }

  /** Parallel set of series connected systems */
  case class P[A](xs: Set[S[A]]) extends PL[A] {
    override def toString: String = xs.map(_.toString).mkString("[", "||", "]")

    /** Generates variants of the system obtained by inserting y in all possible places in the hierarchy. */
    def insert(y: A): List[PL[A]] = {
      // insert inside each series group
      val inside = for {
        x <- xs.toList
          xx <- x.insert(y)
      } yield P((xs - x) + xx)
      // insert in parallel
      val outside = P(xs + S(List(L(y))))
      outside :: inside
    }
  }

  /** A leaf system. */
  case class L[A](x: A) extends PL[A] {
    override def toString: String = x.toString

    /** Generates variants of the system obtained by inserting y in all possible places in the hierarchy. */
    def insert(y: A): List[PL[A]] = {
      P(Set(S(List(L(x))), S(List(L(y))))) :: Nil
    }
  }

  /** A series composition of parallel or leaf systems. */
  case class S[A](xs: List[PL[A]]) {

    override def toString: String = xs.map(_.toString).mkString("(", "--", ")")

    /** Generates variants of the system obtained by inserting y in all possible places in the hierarchy. */
    def insert(y: A): List[S[A]] = {
      @tailrec
      def loop(todo: List[PL[A]], processed: List[PL[A]], resAcc: List[S[A]]): List[S[A]] =
        todo match {
          // for insert before an inside each parallel group
          case head :: tail =>
            val before = S((L(y) :: head :: tail).reverse_:::(processed))
            val inside = head.insert(y).map(res => S((res :: tail).reverse_:::(processed)))
            loop(tail, head :: processed, resAcc.reverse_:::(before :: inside))

          // insert at tail
          case Nil =>
            (S((L(y) :: processed).reverse) :: resAcc).reverse
        }

      loop(xs, Nil, Nil)
    }
  }

  /** Specifies direct connections between leaf components.
    * Presence of item (i, j) in conns specifies a direct connection
    * of input of leaf component i to output of leaf component j.  
    * */
  case class DirectConnections(conns: List[(Int, Int)]) {
    
    /** LeafComp uids having direct input connections. */
    val inputs: Set[Int] = 
      conns.map(_._1).toSet
      
    /** LeafComp uids having direct output connections. */
    val outputs: Set[Int] = 
      conns.map(_._2).toSet
      
    /** For an input, gives the list of outputs it connects to.  */
    val groups: Map[Int, List[Int]] = 
      conns
        .groupBy(_._1)
        .map { case (key, l) => (key, l.map(_._2)) }
  }

  /** A leaf component specification */
  case class CompSpec[A](
    
    /** unique id */
    uid: Int, 
    
    /** outputValue when in failed state. */
    failedValue: A,

    /** Delay distribution. */
    dist: InputDistribution,
  )
  
  /** Lists of direct connection inputs and outputs from sub components forwarded up in the hierarchy. */
  trait Fwd[A:Lifted] { self: Component =>
    
    /** Forwarded input flow variable with corresponding leaf component id. */
    def fwdIn: Vector[(Fvar[A], Int)]
    
    /** Forwarded output flow variable with corresponding leaf component id. */
    def fwdOut: Vector[(Fvar[A], Int)]
  }
  
  /** Delay component with urgent propagation event
    * Used to break acausal feedback loops.
    * The component monitors the difference betwee its current and previous input
    * and propagate changes to its output using an urgent event.
    *  */
  class Delay[A: Lifted: Ord](init: A) extends Component {
    val in = InFlow[A]
    val out = OutFlow[A]
    val state = State[A](init = init)
    val propagate = Event.urgent()
    transitions { When(propagate) If (!(in === state)) Then { state := in } }
    assertions { out := state }
  }
  
  /** A leaf of the component hierarchy that can fail. */
  case class LeafComp[A: Lifted : Ord](spec: CompSpec[A]) extends Component {
    val in = InFlow[A]
    val out = OutFlow[A]
    val failed = State(init = false)
    val fail = Event(spec.dist)
    transitions { When(fail) If (!failed) Then {failed := true} }    
    assertions { out := If(!failed) Then in Else spec.failedValue }
  }

  /** A parallel composition of n subsystems */
  class ParallelComp[A: Lifted : Ord](directConn: DirectConnections, structure: PL[CompSpec[A]]) extends Component with Fwd[A] {
    val in = InFlow[A]
    val out = OutFlow[A]
    
    private var _fwdIn = Vector.empty[(Fvar[A], Int)]
    def fwdIn = _fwdIn

    private var _fwdOut = Vector.empty[(Fvar[A], Int)]
    def fwdOut = _fwdOut
    
    structure match {
      
      case P(xs) => {
        require(xs.size > 0)
        // build a parallel composition of series components
        val l = xs.toList
        val subs = Subs(xs.size)(i => SeriesComp(directConn, l(i)))

        assertions {
          subs foreach {
            _.in := in
          }
          out := subs.map(_.out).reduce(_ max _)
        }

        // input forwarding
        val fins = for (sub <- subs; (fin, id) <- sub.fwdIn) yield (fin, id)
        val is = InFlows[A](fins.size)
        assertions {
          is.zip(fins) foreach { case (f, (fin, _)) => fin := f }
        }
        _fwdIn = is.zip(fins.map(_._2))

        // output forwarding
        val fouts = for (sub <- subs; (fout, id) <- sub.fwdOut) yield (fout, id)
        val os = OutFlows[A](fouts.size)
        assertions {
          os.zip(fouts) foreach { case (f, (fout, _)) => f := fout }
        }
        _fwdOut = os.zip(fouts.map(_._2))
      }
        
      case L(x) => {
        // build a single leaf component
        val sub = Sub(LeafComp(x))

        // input forwarding
        if (directConn.inputs.contains(x.uid)) {
          val fin = InFlow[A]
          assertions {
            sub.in := fin
          }
          _fwdIn = Vector((fin, x.uid))
        } else {
          assertions {
            sub.in := in
          }
          _fwdIn = Vector()
        }

        // output forwarding
        if (directConn.outputs.contains(x.uid)) {
          val fout = OutFlow[A]
          assertions {
            fout := sub.out
            out := sub.out
          }
          _fwdOut = Vector((fout, x.uid))
        } else {
          assertions {
            out := sub.out
          }
          _fwdOut = Vector()
        }
      }
    }
  }

  class SeriesComp[A: Lifted : Ord](directConn: DirectConnections, structure: S[CompSpec[A]]) extends Component with Fwd[A]{
    require(structure.xs.size > 0)
    
    val in = InFlow[A]
    val out = OutFlow[A]
    val subs = Subs(structure.xs.size)(i => ParallelComp(directConn, structure.xs(i)))
    
    // connect in series
    assertions {
      // input to first input
      subs.head.in := in
      // from one to the next
      subs.tail.foldLeft(subs.head) {
        case (prev, current) =>
          current.in := prev.out
          current
      }
      // last comp output to this output 
      out := subs.last.out
    }
    
    // input forwarding
    val fins = for (sub <- subs; (fin, id) <- sub.fwdIn) yield (fin, id)
    val is = InFlows[A](fins.size)
    assertions { is.zip(fins) foreach { case (f, (fin, _)) => fin := f } }
    def fwdIn = is.zip(fins.map(_._2))

    // output forwarding
    val fouts = for (sub <- subs; (fout, id) <- sub.fwdOut) yield (fout, id)
    val os = OutFlows[A](fouts.size)
    def fwdOut = os.zip(fouts.map(_._2))
    assertions { os.zip(fouts) foreach { case (f, (fout, _)) => f := fout } }
  }

  case class RootComp[A:Lifted:Ord](
    
    /** Direct connection maps. */
    directConn: DirectConnections,
    
    /** System structure specification. */
    structure: S[CompSpec[A]], 
    
    /** best failure mode (for initialisation of all states and delays. */
    bestMode: A,
    
    /** Mode that triggers the failure condition. */
    worstMode: A,
    
  ) extends Component {
    val obs = OutFlow[Boolean]
    val sub = Sub(SeriesComp(directConn, structure))
    assertions {
      // feed input of series system 
      sub.in := bestMode
      // connect output of series system to observer
      obs := !(sub.out === worstMode)
    }

    // direct connections
    val fwdIn = sub.fwdIn.map(_.swap).toMap
    val fwdOut = sub.fwdOut.map(_.swap).toMap
    val delays = Subs(directConn.groups.size)(i => Delay(bestMode))
    directConn.groups.zip(delays).foreach { case ((i, js), delay) =>
      assertions {
        delay.in := js.map(j => fwdOut(j)).reduce(_ max _)
        fwdIn(i) := delay.out
      }
    }
  }

  /** Generates all system variants. */
  def genAllSystems[A: Lifted : Ord](
    components: List[CompSpec[A]], 
    bestMode: A, 
    worstMode: A,
    directConnections: DirectConnections = DirectConnections(Nil),
  ): List[RootComp[A]] = {
    require(components.size > 0)
    val zero = S(L(components.head) :: Nil)
    components
      .tail
      .foldLeft(zero :: Nil) { case (acc, default) => acc flatMap { _.insert(default) } } 
      .map { RootComp(directConnections, _ , bestMode, worstMode) }
  }
}
