package rythmics

case class BPM(full: Int, frac: Int = 0) {
  def computed: Double = full + {if(frac > 0) 1.0/frac else 0}
}