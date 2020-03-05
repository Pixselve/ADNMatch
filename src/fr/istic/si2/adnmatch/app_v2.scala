package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.ADNMatchV1._
import fr.istic.si2.adnmatch.RExpMatcher._
import fr.istic.si2.adnmatchlib._

/**
 * Application ADNMatch version 2
 */
object ADNMatchV2 extends App {

  println("ADNMatch Version 2")

  def userLoop(): Unit = {
    val exp = askExpression()
    println(rExpToString(exp))
    val seq = askSequence()
    matchComplet(exp, seq) match {
      case true  => println(s"La séquence : ${listeBasesToString(seq)} correspond bien entièrement à l'expression : ${rExpToString(exp)}")
      case false => println(s"La séquence : ${listeBasesToString(seq)} ne correspond pas bien entièrement à l'expression : ${rExpToString(exp)}")
    }
    println("Recommencer ? y/N")

    scala.io.StdIn.readLine() match {
      case "y" => userLoop()
      case _   =>
    }

  }

  def askSequence(): List[Base] = {
    println("Entrez une séquence")
    lireSequence() match {
      case None      => println("Séquence incorrecte"); askSequence()
      case Some(exp) => exp
    }
  }

//  userLoop()


  println(prefixeMatch(litRExp("T(T*)(A|C)").getOrElse(Impossible), List(A, T, G, C, T, C, C, T, G, G, C, T, G, T, T, T, T, G, T, A, C, T, T, T, T, T, A)))


}