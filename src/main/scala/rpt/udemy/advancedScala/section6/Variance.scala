package rpt.udemy.advancedScala.section6

object Variance extends App {

  trait Animal
  class Dog extends Animal
  class Cat extends Animal
  class Kitten extends Cat
  class Crocodile extends Animal

  // "inheritance" type substitution in generics
  // Should a Cage[Dog] inherit from Cage[Animal]?
  class CCage[+T] // Covariant => Yes, it should not
  val cCage:CCage[Animal] = new CCage[Dog]
  class ICage[T] // Invariant => No, it should not
//  val iCage:ICage[Animal] = new ICage[Dog] this won't compile
  class XCage[-T] // Contra-Variant => ABSOLUTELY NO!, it should actually be opossite!
  val xCage:XCage[Dog] = new XCage[Animal]

  // Examples
  class InvariantCage[T](val animal: T)
  class CovariantCage[+T](val animal: T)
  class CotravariantCage[-T](animal: T) // Cannot use val (contravariant in covariant possition)
  /*
    This prevents us from doing:
    val catCage:ContravariantCage[Cat] = new ContravariantCage[Animal](new Crocodile)

    This also applies to:
    class CovariantVariableCage[+T](var animal: T) // covariant in contravariant (and covariant!) position
    val covariantCage:CovariantVariableCage[Animal] = new CovariantVariableCage[Cat](new Cat)
    covariantCage.animal = new Dog // !! cannot put a dog in a cat cage
  */

  /*
    Cannot add animals to a covariant cage
    Arguments are in CONTRAVARIANT position
  class AnotherCovariantCage[+T] {
    def cage(animal: T)
  }

    But we can add to contravariant ones
  */
  class AnotherContraVariantCage[-T](animal: T) {
    def cage(animal:T):AnotherContraVariantCage[T] = new AnotherContraVariantCage[T](animal)
  }

  {
    val acc: AnotherContraVariantCage[Cat] = new AnotherContraVariantCage[Animal](new Cat)
    val catCage: AnotherContraVariantCage[Cat] = acc.cage(new Cat)
    val kittenCage: AnotherContraVariantCage[Cat] = acc.cage(new Kitten) // still a cat cage
    // acc.cage(new Crocodile) cannot because Crocodile does not extend Cat
  }

  // We can however:
  class AnotherCovariantCage[+T](animal:T) {
    // Widening the type contained
    def cage[B >: T](animal: B):AnotherCovariantCage[B] = new AnotherCovariantCage[B](animal)
  }
  val kittyCage:AnotherCovariantCage[Kitten] = new AnotherCovariantCage[Kitten](new Kitten)
  val catCage: AnotherCovariantCage[Cat] =  kittyCage.cage(new Cat)
  val animalCage: AnotherCovariantCage[Animal] = kittyCage.cage(new Dog)

  // Method return types
  /*
    Return types are in COVARIANT position.
    Otherwise...
  trait PetShop[-T] {
    def get():T
  }
  val catShop:PetShop[Cat] = new PetShop[Animal] {
    override def get():Animal = new Cat
  }
  val dogShop:PetShop[Dog] = catShop
  val weirdDog = dogShop.get()

  We can sove this by
  */
  class PetShop[-T] {
    def get[B<:T](cls: Class[B]):B = cls.getConstructor().newInstance()
  }
  val dogShop:PetShop[Dog] = new PetShop[Animal]
//  val weidDog = dogShop.get(classOf[Cat]) not Allowed, but
  class BorderCollie extends Dog
  val collie: BorderCollie = dogShop.get(classOf[BorderCollie])

  /*
    Take-Outs
      1. Method arguments are in CONTRAVARIANT position
      2. Method return types are in COVARIANT position
    Check signature of Fucntion
    Argument is Contravariant and Return is Covariant
    type Function[-A, +B] = Function1[A, B]

    Exercises:
      * CoVariant/Invariant/Contravariant Parking[T <: Vehicle](vehicles:List[T])
  */
  trait Vehicle
  class Car extends Vehicle
  class BMW extends Car
  class MotorBike extends Vehicle
  class IList[T]

  class IParking[T](vehicles:List[T]=Nil) {
    def park(vehicle: T):IParking[T] = ???
    def impound(vehicles: List[T]):IParking[T] = ???
    def check:List[T] = ???
    def flatMap[V](f:T=>IParking[V]):IParking[V] = ???
  }
  class CParking[+T](vehicles:List[T]=Nil) {
    def park[V>:T](vehicle: V):CParking[V] = ???
    def impound[V>:T](vehicles: List[V]):CParking[V] = ???
    def check:List[T] = ???
    def flatMap[V](f:T=>CParking[V]):CParking[V] = ???
  }
  class ICParking[+T](vehicles:IList[T]) {
    def park[V>:T](vehicle: V):ICParking[V] = ???
    def impound[V>:T](vehicles: IList[V]):ICParking[V] = ???
    def check[V>:T]:IList[V] = ???
    def flatMap[V](f:T=>ICParking[V]):ICParking[V] = ???
  }
  class XParking[-T](vehicles:List[T]=Nil) {
    def park(vehicle: T):XParking[T] = ???
    def impound(vehicles: List[T]):XParking[T] = ???
    def check[V<:T]:List[V] = ???
    def flatMap[R<:T, V](f:R=>XParking[V]):XParking[V] = ??? // Double contravariant = covariant
  }
  class IXParking[-T](vehicles:IList[T]) {
    def park(vehicle: T):IXParking[T] = ???
    def impound[V<:T](vehicles: IList[V]):IXParking[T] = ???
    def check[V<:T]:IList[V] = ???
    def flatMap[R<:T, V](f:R=>IXParking[V]):XParking[V] = ??? // Double contravariant = covariant
  }

  /*
    As a Rule of Thumb:
      * Use Covariance when used as a collection of things
      * Use Contravariance if used as a collection of actions
  */

  {
    val bmwPark:CParking[BMW] = new CParking[BMW]().park(new BMW)
    val carPark: CParking[Car] = bmwPark.park(new Car) // Widens to Car
    val stillACarPark: CParking[Car] = carPark.park(new BMW)
    val vehiclePark: CParking[Vehicle] = stillACarPark.park(new MotorBike) // Widens to Vehicle
  }

  {
    val bmwPark:IParking[BMW] = new IParking[BMW]().park(new BMW)
    // val carPark = bmwPark.park(new Car) // Only Allows BMWs
    val carPark:IParking[Car] = new IParking[Car]().park(new Car)
    val stillACarPark: IParking[Car] = carPark.park(new BMW) // Widens to Car
    // val vehiclePark = stillACarPark.park(new MotorBike) // Only allows cars
  }

  {
    val bmwPark: XParking[BMW] = new XParking[Car]()
    // val carPark = bmwPark.park(new Car) // Only allows BMWs
    val carPark: XParking[Car] = new XParking[Vehicle]()
    val stillACarPark: XParking[Car] = carPark.park(new BMW)
    // val vehiclePark = stillACarPark.park(new MotorBike) // Motorbikes not allowed
  }

}
