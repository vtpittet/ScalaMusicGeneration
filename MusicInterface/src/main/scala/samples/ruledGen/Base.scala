package ruledGen

import grammar._
import grammar.ImplicitsWords2Elements._
import generation._
import chord._
import rythmics._
import tonalSystem._
import tonalSystem.Tone._


object Base extends App {


  lazy val chords: Grammar[Chord] = 
    Triad(I) ** Triad(IV) ** Triad(V) ** Triad(I)

//  lazy val root: GrammarElement[


}
