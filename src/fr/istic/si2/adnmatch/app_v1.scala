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

  val exampleBase1: List[Base] = A :: A :: G :: Nil
  val exampleBase2: List[Base] = A :: C :: G :: T :: Nil
  val exampleBase3: List[Base] = C :: C :: T :: Nil

  val exampleRExp1 = UneBase(A)
  val exampleRExp2 = Choix(UneBase(G), UneBase(T))
  val exampleRExp3 = Repete(UneBase(G))

  def userLoop(): Unit = {
    val exp = askSequence()
    println(rExpToString(exp))
    println(listeBasesToString(getOrElseNil(deroule(exp))))
    println("Recommencer ? y/N")

    scala.io.StdIn.readLine() match {
      case "y" => userLoop()
      case _   =>
    }

  }


  def askSequence(): RExp = {
    println("Entrez une expression")
    litRExp(scala.io.StdIn.readLine()) match {
      case None      => println("Expression incorrecte"); askSequence()
      case Some(exp) => exp
    }
  }

  userLoop()

}