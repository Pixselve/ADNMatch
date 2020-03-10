package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.SequencesImages._
import fr.istic.si2.adnmatch.Marqueur


/**
 * Application ADNMatch version 3
 */
object ADNMAtchV3 extends App {

  // Lancement de l'interface graphique.
  bigbang(ADNMatchUniverse)
  /*
  val jeanmichelADN : List[(Marqueur, Base)] = List((Marque, A),(Marque, C),(Marque, T),(Marque, T),(Marque, C),(Marque, C),(Marque, G),(Marque, C),(Marque, G),(Marque, A))
  printall(lignes(jeanmichelADN,4))
  def printl(l : List[(Marqueur, Base)]) : Unit = {
    l match  {
      case Nil => println()
      case first :: reste => first match {
        case (_, A) => {
          print("A")
          printl(reste)
        }
        case (_, T) => {
          print("T")
          printl(reste)
        }
        case (_, G) => {
          print("G")
          printl(reste)
        }
        case (_, C) => {
          print("C")
          printl(reste)
        }
      }
    }
  }

  def printall(l : List[List[(Marqueur, Base)]]) : Unit = {
    l match  {
      case Nil => println("")
      case first :: rest => {
        printl(first)
        printall(rest)
      }
    }
  }*/

}