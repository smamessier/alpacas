import com.uber.atcp.dsl.Modelling.{_, given _}
import com.uber.atcp.dsl.Analysis._
import com.uber.atcp.dsl._

import scala.language.implicitConversions


object VerticalExample extends App {

  class Engine extends Component {
    val power = InFlow[Boolean]
    val thrust = OutFlow[Boolean]
    val working = State[Boolean](init = true)
    val failure = Event()
    transitions{
      When (failure) If working Then {working := false}
    }
    assertions{
      thrust := working && power
    }
  }

  class Battery extends Component {
    val power = OutFlow[Boolean]
    val working = State[Boolean](init = true)
    val failure = Event()
    transitions{
      When (failure) If working Then {working := false}
    }
    assertions{
      power := working
    }
  }

  class System() extends Component {

    val b1 = Sub(Battery()) 
    val b2 = Sub(Battery()) 

    val e1 = Sub(Engine()) 
    val e2 = Sub(Engine()) 

    val totalLoss = OutFlow[Boolean]
    val partialLoss = OutFlow[Boolean]

    assertions{
      e1.power := b1.power || b2.power
      e2.power := b1.power || b2.power

      totalLoss := !e1.thrust && !e2.thrust 
      partialLoss := !e1.thrust || !e2.thrust 
    }
  }

  val system = System()
  for(model <- compCheck(system)){

    println("MCS for total loss :")
    val s = minimalCutSequencesBFS(model, !system.totalLoss, 4)
    s.foreach(println(_))

    println("\nMCS for partial loss :")
    minimalCutSetsBFS(model, !system.partialLoss, 4).foreach(println(_))
  }

}