package thrustRealloc


import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._
import com.uber.atcp.dsl.ast.Expr

import InputDistribution._
  
import scala.language.implicitConversions

type TrimValue = Double
/** Engine component model.
 * Three different failure rates depending on the trimming percentage applied to the engine:
 *   - [      0, thresh0] fails with lambda0
 *   - [thresh0, thresh1] fails with lambda1
 *   - [thresh1,     100] fails with lambda2
 * */
class Engine(
  lambda0: Double,
  lambda1: Double,
  lambda2: Double,
  thresh0: TrimValue = 10.0,
  thresh1: TrimValue = 50.0,
) extends Component {
  require(0.0 < thresh0 && thresh0 < 100.0)
  require(0.0 < thresh1 && thresh1 < 100.0)
  require(thresh0 < thresh1)

  /** Trim value in percent. */
  val trimValue = InFlow[TrimValue]

  /** True iff the engine can provide thrust. */
  val canProduceThrust = OutFlow[Boolean]

  /** Base thrust value (depends on trim and failure state). */
  val baseThrustValue = OutFlow[TrimValue]

  /** True iff the engine currently receives power. */
  val isPowered = InFlow[Boolean]

  /** Intrinsic engine failure state. */
  val isFailed = State[Boolean](init = false)

  /** Engine failure event in [0%, 10%] trim interval. */
  val fail0 = Event(Exponential(lambda0))

  /** Engine failure event in [10%, 50%] trim interval. */
  val fail1 = Event(Exponential(lambda1))

  /** Engine failure event in [50%, 100%] trim interval. */
  val fail2 = Event(Exponential(lambda2))

  transitions {
    When (fail0) If (
      canProduceThrust && trimValue <= thresh0
    ) Then {
      isFailed := true
    }
    When (fail1) If (
      canProduceThrust && thresh0 < trimValue && trimValue <= thresh1
    ) Then {
      isFailed := true
    }
    When (fail2) If (
      canProduceThrust && thresh1 <= trimValue
    ) Then {
      isFailed := true
    }
  }

  assertions {
    canProduceThrust := !isFailed && isPowered
    baseThrustValue  := If (canProduceThrust) Then { trimValue } Else { -100.0 }
  }
}

/** Battery component model. */
class Battery(lambda: Double) extends Component {

  /** True iff the battery produces power. */
  val power = OutFlow[Boolean]

  /** Intrinsic failure event. */
  val fail = Event(Exponential(lambda))

  /** Intrinsic failure state. */
  val isFailed = State[Boolean](init = false)

  transitions { When (fail) If (!isFailed) Then { isFailed := true } }
  assertions { power := !isFailed }
}

/** CPU component model. */
class CPU(lambda: Double) extends Component {
  
  /** True iff the component receives power. */
  val isPowered = InFlow[Boolean]
  
  /** True iff the CPU is running (depends on intrinsic failure state and received power). */
  val isRunning = OutFlow[Boolean]
  
  /** Intrisic failure state. */
  val isFailed = State[Boolean](init = false)
  
  /** Intrinsic failure event. */
  val fail = Event(Exponential(lambda))

  transitions { When (fail) If (!isFailed) Then { isFailed := true } }
  assertions { isRunning := isPowered && !isFailed }
}

/** Generic trim function for varying number of engines.
 * @param nofEngines number of engines to manage
 * @param opposite given a function index returns the index of the opposite engine
 *
 * A TrimFunctionLike component monitors an even number of engines,
 * and depending on the canProduceThrustSensed failure state of the engines,
 * makes trimming decisions for the engines.
 *
 * The function needs a CPU to be available to run.
 * */
class TrimFunction(
  val nofEngines: Int,
  val opposite: Int => Int,
) extends Component {
  require(nofEngines >= 6, "number of engines must be greated than 6")
  require(nofEngines % 2 == 0, "number of engines must be even")

  /** Sensed failure states for monitored engines. */
  val canProduceThrust = InFlows[Boolean](nofEngines)

  /** Trim values for monitored engines. */
  val trimValue = OutFlows[TrimValue](nofEngines)

  /** True iff one of the the CPUs supporting this function is running. */
  val hasCpu = InFlow[Boolean]

  // minimum id among failed engines
  val minFailedIds = OutFlows[Int](nofEngines)
  val minFailedId  = OutFlow[Int]

  // maximum id among failed engines
  val maxFailedIds = OutFlows[Int](nofEngines)
  val maxFailedId  = OutFlow[Int]

  // number of failed engines
  val failedCounts = OutFlows[Int](nofEngines)
  val failedCount  = OutFlow[Int]

  // cord distance on the circle betwee the min and max failed engines
  val cordDist = OutFlow[Int]

  assertions {
    // Minimum identifier of failed engines.
    minFailedIds.zipWithIndex.foreach { case (v, i) =>
      v := If(canProduceThrust(i)) Then { nofEngines + 1 } Else { i }
    }
    minFailedId := minFailedIds.reduce(_ min _)

    // Maximum identifier of failed engines.
    maxFailedIds.zipWithIndex.foreach { case (v, i) =>
      v := If(canProduceThrust(i)) Then { -1 } Else { i }
    }
    maxFailedId := maxFailedIds.reduce(_ max _)

    // Number of failed engines.
    failedCounts.zipWithIndex.foreach { case (v, i) =>
      v := If(canProduceThrust(i)) Then { 0 } Else { 1 }
    }
    failedCount := failedCounts.reduce(_ + _)

    // Distance between min and max failed engine IDs, assuming they are spread on a circle.
    // If distance is the number of engines divided by two then engines are opposite.
    cordDist := (maxFailedId - minFailedId) min (minFailedId - maxFailedId + nofEngines)

    trimValue.zipWithIndex.foreach { case (v, i) =>
      v :=
        If(hasCpu) Then {
          If(failedCount === 0) Then {
            // nominal mode, no trim applied
            0.0
          } ElseIf(failedCount === 1) Then {
            // single engine loss
            If (!canProduceThrust(i) || !canProduceThrust(opposite(i))) Then {
              // shutdown failed and opposite engine by trimming to -100
              -100.0
            } Else {
              // reallocate missing thrust to other engines
              200.0/(nofEngines - 2)
            }
          } ElseIf(failedCount === 2) Then {
            If (cordDist === nofEngines/2) Then {
              // failure is already symmetrical, only need to increase thrust by 33%
              If (!canProduceThrust(i) || !canProduceThrust(opposite(i))) Then {
                // shutdown failed and opposite engine by trimming to -100
                -100.0
              } Else {
                // reallocate missing thrust to other engines
                200.0/(nofEngines - 2)
              }
            } ElseIf(nofEngines >= 8) Then {
              // failure not symmetrical, need to shut down 4 engines.
              // increase thrust by 100% on all remaining engines
              If (!canProduceThrust(i) || !canProduceThrust(opposite(i))) Then {
                -100.0
              } Else {
                400.0/(nofEngines - 4)
              }
            } Else {
              0.0  // 2 failures but less than 8 engines, do not do anything
            }
          } Else {
            // 3 failures and up, do not do anything
            0.0
          }
        } Else {
          0.0  // no power, function does not compute trims
        }
    }
  }
}

/** Sensor component model. Senses the current failure state of an engine.
 *  @param lambda failure rate of the sensor
 *  @param optimistic controls the default value returned by the sensor
 *                    when it is in a failed state. Optimistic yields
 *                    engine is functionning, false yields engine is failed
 *  */
class Sensor(lambda: Double, optimistic: Boolean) extends Component {

  /** Sensed engine failure mode. */
  val canProduceThrustSensed = InFlow[Boolean]

  /** Transmitted failure mode */
  val canProduceThrustTransmitted = OutFlow[Boolean]

  /** True iff it receives power. */
  val isPowered = InFlow[Boolean]

  /** Intrisic failure state. */
  val isFailed = State[Boolean](init = false)

  /** Intrinsic failure event. */
  val fail = Event(Exponential(lambda))

  /** Memorises the value of canProduceThrustSensed
   *  when the sensor fails. */
  val lastValue = State[Boolean](init = optimistic)

  transitions {
    When (fail) If (!isFailed) Then {
      isFailed   := true
      lastValue := canProduceThrustSensed
    }
  }

  assertions {
    canProduceThrustTransmitted := If(isFailed || !isPowered) Then {
      lastValue && optimistic
    } Else {
      canProduceThrustSensed
    }
  }
}

/** Failure condition observer.
 * The produced thrust must be symmetric and total base thrust must lie in interval [-margin, +margin]
 *  */
class StableLiftObserver(
  nofEngines : Int,
  opposite: Int => Int,
  margin: TrimValue = 3.0,
) extends Component {
  require(6 <= nofEngines && nofEngines <= 12)
  require(margin >= 0)

  // base thrust direct from the engines.
  val baseThrustValues = InFlows[TrimValue](nofEngines)

  // true iff the failure condition is not reached at the current step.
  val isOk = OutFlow[Boolean]

  // true iff all engines and their opposites have the same base thrust value
  private val symmetricThrust = baseThrustValues.zipWithIndex.map {
    case (t, i) => t === baseThrustValues(opposite(i)) 
  }.reduce(_ && _)

  // sum of all base thrusts for all engines
  private val totalBaseThrust = baseThrustValues.fold(0.0:Expr[TrimValue])(_ + _)

  assertions {
    isOk :=
      symmetricThrust &&
      -margin <= totalBaseThrust && totalBaseThrust < margin
  }
}

/** Holds configuration parameter for an engine + battery configuration. */
case class EngineParameters(
  nofEngines: Int,
  lambda0: Double,
  lambda1: Double,
  lambda2: Double,
  lambdaBattery: Double,
)

type Wiring = (Vector[Engine], Vector[Sensor], Vector[Battery]) => Assertions

class GenericSystem(
  val engineParameters: EngineParameters,
  val lambdaSensor: Double,
  val lambdaCpu: Double,
  val nofCpus: Int,
  val optimisticSensor: Boolean,
  val oppositeEngine: Int => Int,
  val wiring : Wiring,
  val seg: Boolean,
) extends Component {
  import engineParameters._
  // components of the system
  val engines   = Subs(nofEngines)(Engine(lambda0, lambda1, lambda2))
  val batteries = Subs(nofEngines)(Battery(lambdaBattery))
  val sensors   = Subs(nofEngines)(Sensor(lambdaSensor, optimisticSensor))
  val cpus      = Subs(nofCpus)(CPU(lambdaCpu))
  val function  = Sub(TrimFunction(nofEngines, oppositeEngine))
  val observer  = Sub(StableLiftObserver(nofEngines, oppositeEngine))

  assertions {
    // batteries to cpus mapping
    cpus.foreach(_.isPowered := batteries.map(_.power).reduce(_||_))

    // cpu to functions mapping
    function.hasCpu := cpus.map(!_.isFailed).reduce(_||_)

    // batteries to engines
    // batteries to sensors
    wiring(engines, sensors, batteries)

    engines.map(_.trimValue) := function.trimValue
    function.canProduceThrust := sensors.map(_.canProduceThrustTransmitted)

    // engines to sensors
    sensors.map(_.canProduceThrustSensed) := engines.map(_.canProduceThrust)

    // engines to observer mapping
    observer.baseThrustValues := engines.map(_.baseThrustValue)
  }
}

/** Produces a wiring function given two functions that return a
 * pair of battery components when given a engine or sensor battery.
 * @param engineIdtoBatteryIds returns the pair of battery Ids for an engine Id.
 * @param sensorIdToBatteryIds returns the pair of battery Ids for a sensor Id.
 *  */
def genericWiring(
  engineIdtoBatteryIds: Int => (Int, Int),
  sensorIdToBatteryIds: Int => (Int, Int),
): Wiring =
  (engines : Vector[Engine], sensors : Vector[Sensor], batteries : Vector[Battery]) => {
    require(engines.size == sensors.size)
    require(engines.size == batteries.size)
    val nofEngines = engines.size
    for {
      i <- 0 until nofEngines
      (eb1, eb2) = engineIdtoBatteryIds(i)
      (sb1, sb2) = sensorIdToBatteryIds(i)
    }{
      engines(i).isPowered := batteries(eb1).power || batteries(eb2).power
      sensors(i).isPowered := batteries(sb1).power || batteries(sb2).power
    }
  }

/** The opposite function for circular arrangements of engines. */
def opposite(nofEngines: Int)(n: Int) = {
  require(nofEngines > 0)
  require(nofEngines % 2 == 0)
  (n + nofEngines/2) % nofEngines
}

/** Maps an id to itself an the opposite id. */
def oppositeWiring(nofEngines: Int)(n: Int) = {
  require(nofEngines > 0)
  require(nofEngines % 2 == 0)
  (n, opposite(nofEngines)(n))
}

/** Maps an id to an offset id. */
def offset(nofEngines: Int)(n: Int) = {
  require(nofEngines > 0)
  require(nofEngines % 2 == 0)
  (n + 1) % nofEngines
}

/** Maps an id i to an offset Id and its opposite. */
def offsetWiring(nofEngines: Int)(n: Int) =
  (offset(nofEngines)(n), opposite(nofEngines)(offset(nofEngines)(n)))

/** Wiring which connects
 * - an engine and its opposite engine to the same battery pair.
 * - a sensor to the same battery pair as its engine
 *  */
def stdWiring(nofEngines: Int): Wiring =
  genericWiring(oppositeWiring(nofEngines), oppositeWiring(nofEngines))

/** Wiring which connects
 * - An engine and its opposite engine to the same battery pair.
 * - A sensor to an offset battery pair relative to its engine.
 *  */
def segWiring(nofEngines: Int): Wiring =
  genericWiring(oppositeWiring(nofEngines), offsetWiring(nofEngines))



object PowerManagementGen extends App {

  // definition of the design space
  val systems = for {
    engineParams <- List(
      EngineParameters(nofEngines = 6, lambda0 = 1E-5, lambda1 = 1E-4, lambda2 = 2E-4, lambdaBattery = 1E-5),
      EngineParameters(nofEngines = 8, lambda0 = 2E-5, lambda1 = 1E-4, lambda2 = 2E-4, lambdaBattery = 1E-5),
      EngineParameters(nofEngines = 10, lambda0 = 2.5E-5, lambda1 = 1E-4, lambda2 = 2E-4, lambdaBattery = 1E-5),
    )
    nofEngines = engineParams.nofEngines
    lambdaSensor <- List(1E-5, 1E-10)
    lambdaCpu = 1E-10
    nofCpus = 2
    optimisticSensor <- List(true, false)
    oppositeEngine = opposite(nofEngines)
    (wiringFunc, seg) <- List(
        (stdWiring(nofEngines), false),
        (segWiring(nofEngines), true),
    )
  } yield GenericSystem(
    engineParams,
    lambdaSensor,
    lambdaCpu,
    nofCpus,
    optimisticSensor,
    oppositeEngine,
    wiringFunc,
    seg,
  )

  case class Result(
    engineParameters: EngineParameters,
    lambdaSensor: Double,
    optimisticSensor: Boolean,
    segregatedSensors: Boolean,
    nofMcsOrder1: Int,
    nofMcsOrder2: Int,
    nofMcsOrder3: Int,
    unreliability: Double,
  ) {
    def csvHeader: String = 
      """$n$,$\lambda_0$,$\lambda_1$,$\lambda_2$,$\lambda_b$,$\lambda_s$,$\mathit{opt}$,$\mathit{seg}$,$\#$ order 1 mcs,$\#$ order 2 mcs,$\#$ order 3 mcs,$\mathit{U}(T)$""".stripMargin
    
    def csvLine: String = {
      val n = engineParameters.nofEngines
      val l0 = engineParameters.lambda0
      val l1 = engineParameters.lambda1
      val l2 = engineParameters.lambda2
      val lb = engineParameters.lambdaBattery
      val u = f"$unreliability%1.3f"
      s"""$n,$l0,$l1,$l2,$lb,$lambdaSensor,$optimisticSensor,$segregatedSensors,$nofMcsOrder1,$nofMcsOrder2,$nofMcsOrder3,$u"""
    }
  }
  var results = List.empty[Result]
  for {
    system <- systems 
    model <- stochasticCheck(system)
    mcs <- minimalCutSetsBFS(model, system.observer.isOk, 3)
    ur <- unreliability(model, system.observer.isOk, 1E3, 100000, 4)
  } {
    // interactiveSimulationB(model)
    println(system.engineParameters.nofEngines)
    println(system.optimisticSensor)
    val nofmcs1 = mcs.count(_.events.size == 1)
    val nofmcs2 = mcs.count(_.events.size == 2)
    val nofmcs3 = mcs.count(_.events.size == 3)
    val mcs1 = mcs.filter(_.events.size == 1)
    val mcs2 = mcs.filter(_.events.size == 2)
    val mcs3 = mcs.filter(_.events.size == 3)
    val minorder = mcs.map(_.events.size).fold(10000)(_ min _)
    println(
      s"""
        |----------------------
        |segregate:
        | ${system.seg}
        |nofEngines: 
        | ${system.engineParameters.nofEngines}
        |lambda sensor:
        | ${system.lambdaSensor}
        |optimistic sensor:
        | ${system.optimisticSensor}
        |minorder:
        | $minorder
        |order:
        | 1 => $nofmcs1 
        | 2 => $nofmcs2
        | 3 => $nofmcs3
        |unreliability:
        |  $ur
        |""".stripMargin
    )
    results ::= Result(
      engineParameters = system.engineParameters,
      lambdaSensor = system.lambdaSensor,
      optimisticSensor = system.optimisticSensor,
      segregatedSensors = system.seg,
      nofMcsOrder1 = nofmcs1,
      nofMcsOrder2 = nofmcs2,
      nofMcsOrder3 = nofmcs3,
      unreliability = ur,
    )
  }
  
  // TODO format results to Latex
  val header = results.head.csvHeader
  val all = header :: results.reverse.map(_.csvLine)
  println(all.mkString("\n"))
  
  
}
