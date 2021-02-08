object Main extends App{
  import com.uber.atcp.dsl.Modelling.{_, given _}
  import com.uber.atcp.dsl.Analysis._
  import com.uber.atcp.dsl._
  
  import scala.language.implicitConversions 


  given Ord[Float]{
    def lt(x : Float, y : Float) = x < y
  }

  type Batteries = Vector[Battery]
  type Engines = Vector[Engine]


  enum Failure derives Lifted {
    case Ok
    case Fail
  }

  import Failure._


  given Ord[Failure] {
    def lt(x : Failure, y : Failure) = {
      x == Ok && y == Fail
    }
  }

  class Engine extends Component {
    val thrust = OutFlow[Failure]
    val power = InFlow[Failure]
    val state = State[Failure](init  = Ok)
    val failure = Event(InputDistribution.Exponential(1E-5))
    val repair = Event(InputDistribution.Dirac(1))
    transitions{
      When (failure) If state === Ok Then {state := Fail}
      When (repair) If state === Fail Then {state := Ok}
    }
    assertions{
      thrust := If (power === Ok && state === Ok) Then Ok Else Fail
    }
  }
  class Battery extends Component {
    val power = OutFlow[Failure]
    val state = State[Failure](init = Ok)
    val failure = Event(InputDistribution.Exponential(1E-5))
    val repair = Event(InputDistribution.Dirac(5), 1.0)
    transitions{
      When (failure) If state === Failure.Ok Then {state := Failure.Fail}
      When (repair) If state === Fail Then {state := Ok}
    }
    assertions{
      power := state
    }
  }

  class Powertrain(wiring : (Batteries, Engines) => Assertions, n : Int) extends Component {
    val batteries = Subs(n)(Battery()) 
    // the model is parametered by the number of engines and batteries
    val engines = Subs(n)(Engine())
    val observer = OutFlow[Boolean]
    val ccf = Event(InputDistribution.Exponential(1E-7))
    
    assertions{
      wiring(batteries, engines) // wiring is a parameter of the model (higher order parameter)
      observer := engines.map(_.thrust === Ok).reduce(_&&_)
    }
    transitions{
      Sync(ccf) With{
        batteries.map(_.failure.hard).reduce(_&_)
      }
    }
  }

  def one2one(b : Batteries, e : Engines) : Assertions = {
    e map (_.power) := b map (_.power) 
  }// Engine i powered by battery i

  def one2all(b : Batteries, e : Engines) : Assertions = {
    for {
      eng <- e
    } eng.power := (b map (_.power) reduce (_ min _)) 
  }// powered by the "least failed" battery

  val p1 = Powertrain(one2one, 2)
  val p = Powertrain(one2all, 2)


  /* println(mttf(p2, p2.observer, 1000000, 10)) // time to failure, average on 100 simulations */
  /* println(unavailability(p2, p2.observer, 1E10, 10)) // unavailability proportion on simulation with time limit 1E10 */
  
  for(model <- compCheck(p)){
    minimalCutSetsBFS(model, p.observer, 3).foreach(println(_))
    interactiveSimulationB(model) // interactive simulation
  }


  /* val (mcs1, ur1) = unreliabilityMCS(p1, p1.observer, 1E3, 4)
  println("Cut sets for p1 :")
  mcs1.foreach(println(_))
  println(s"\nMCS unreliability for p1 : $ur1")
  println(s"Monte Carlo unreliability for p1 : ${unreliability(p1, p1.observer, 1E3, 1000000, 10)}")
  val (mcs2, ur2) = unreliabilityMCS(p2, p2.observer, 1E3, 4)
  println("\nCut sets for p2 :")
  mcs2.foreach(println(_))
  println(s"\nMCS unreliability for p2 : $ur2")
  println(s"Monte Carlo unreliability for p2 : ${unreliability(p2, p2.observer, 1E3, 1000000, 10)}") */
}