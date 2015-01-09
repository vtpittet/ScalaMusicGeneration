package rythmics

case class RythmCell(durations: List[BPM]) {
  val size = durations.size
  val duration = (0.0 /: durations) { _ + _.computed }

  def +:(bpm: BPM): RythmCell = RythmCell(bpm :: durations)
}

object RythmCell {
  implicit def bpmToRythmCell[A <: BPM](bpm: A): RythmCell = RythmCell(List(bpm))
}
