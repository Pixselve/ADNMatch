package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatchlib._

/**
 * Application ADNMatch version 1.
 */
object ADNMatchV1 extends App {

  println("ADNMatch Version 1")
  // TODO V1 - A compléter
  val exampleBase1: List[Base] = A :: A :: G :: Nil
  val exampleBase2: List[Base] = A :: C :: G :: T :: Nil
  val exampleBase3: List[Base] = C :: C :: T :: Nil


  println(listeBasesToString(exampleBase1))

  val exampleRExp1 = UneBase(A)
  val exampleRExp2 = Choix(UneBase(G), UneBase(T))
  val exampleRExp3 = Repete(UneBase(G))

  println(litRExp("(AT)|A"))
}