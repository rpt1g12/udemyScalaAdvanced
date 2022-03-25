package rpt.udemy.advancedScala.section6

object SelfTypes extends App {
  // Requiring a type to be mixed in

  trait Instrumentalist {
    def play:Unit
  }

  trait Singer { self: Instrumentalist =>
    def sing:Unit
  }

  /*
  This does not compile
  class LeadSinger extends Singer {
    def sing:Unit = println("La, la laaa")
  }
  */
  // This compiles because implements both
  class LeadSinger extends Singer with Instrumentalist {
    def sing:Unit = println("La, la laaa")
    def play:Unit = println("GMaj")
  }

  class Guitarrist extends Instrumentalist {
    override def play: Unit = println("DminorMajor Scale")
  }

  // THis is also valid.
  val ericClaption = new Guitarrist with Singer {
    override def sing: Unit = println("Tears in heaven")
  }

  // Vs Inheritance
  class A
  class B extends A
  // B is an A

  trait S
  trait T { self:S =>
  }
  // T requires an S

  /*
    Cake Pattern: Dependency Injection
  */
  class Component

  // Cake patternt in Java
  class ComponentA extends Component
  class ComponentB extends Component
  class DependentComponent(component:Component)

  val dca = new DependentComponent(new ComponentA)
  val dcb = new DependentComponent(new ComponentB)

  // Cake pattern in Scala
  trait ScalaComponent {
    def action:Int
  }
  trait ScalaDependentComponent {self: ScalaComponent=>
    def dependentActions:Int = action+1
  }

  // Layer 1
  trait Picture extends ScalaComponent
  trait Text extends ScalaComponent

  // Layer 2
  trait Profile extends ScalaDependentComponent with Picture with Text

  // Cyclical Dependencies
//  class X extends Y
//  class Y extends X

  // Reciprocal they both require each other.
  trait X {self:Y =>}
  trait Y {self:X =>}

}
