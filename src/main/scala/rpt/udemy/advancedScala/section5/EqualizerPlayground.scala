package rpt.udemy.advancedScala.section5

object EqualizerPlayground extends App {

  implicit class EqualizerEnrichment[T](value: T) {
    def ===(other:T)(implicit equalizer: Equalizer[T]):Boolean = {
      Equalizer(value,other)
    }
    def =!=(other:T)(implicit equalizer: Equalizer[T]):Boolean = ===(other)
  }

}
