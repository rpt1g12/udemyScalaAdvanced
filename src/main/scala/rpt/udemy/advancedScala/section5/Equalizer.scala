package rpt.udemy.advancedScala.section5

 trait Equalizer[T] {
    def apply(left:T, right:T):Boolean
  }

  object Equalizer {
    def apply[T](left:T, right:T)(implicit equalizer: Equalizer[T]):Boolean = equalizer(left,right)
    def apply[T](implicit equalizer: Equalizer[T]):Equalizer[T] = equalizer
  }