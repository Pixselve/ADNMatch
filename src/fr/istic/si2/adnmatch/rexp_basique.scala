package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._

/**
 * Type algébrique modélisant
 * les bases azotées composant les séquences ADN.
 */
sealed trait Base

case object A extends Base

case object T extends Base

case object G extends Base

case object C extends Base

/**
 * Type algébrique récursif modélisant les
 * expressions régulières sur les bases azotées.
 */
sealed trait RExp

case object Impossible extends RExp

case object Vide extends RExp

case object Nqb extends RExp

case class UneBase(b: Base) extends RExp

case class Choix(e1: RExp, e2: RExp) extends RExp

case class Concat(e1: RExp, e2: RExp) extends RExp

case class Repete(e: RExp) extends RExp

case class NFois(e: RExp, n: Int) extends RExp

object FonctionsRExp {

  /**
   * @param lb une liste de bases azotées
   * @return une chaîne de caractères représentant les bases de lb, dans l'ordre
   */
  // TODO V1
  def listeBasesToString(lb: List[Base]): String = {
    lb match {
      case Nil => ""
      case base :: rest => base + listeBasesToString(rest)
    }
  }

  /**
   * @param e une expression régulière
   * @return la représentation textuelle de e, avec toutes les parenthèses nécessaires
   */
  // TODO V1
  def rExpToString(e: RExp): String = {
    e match {
      case Impossible => "@"
      case Vide => "%"
      case Nqb => "."
      case UneBase(b) => b.toString
      case Choix(e1, e2) => rExpToString(e1) + "|" + rExpToString(e2)
      case Concat(e1, e2) => (e1, e2) match {
        case (UneBase(_), UneBase(_)) => rExpToString(e1) + rExpToString(e2)
        case (_, UneBase(_)) => "(" + rExpToString(e1) + ")" + rExpToString(e2)
        case (UneBase(_), _) => rExpToString(e1) + "(" + rExpToString(e2) + ")"
      }
      case Repete(e) => rExpToString(e) + "*"
      case NFois(e, n) => rExpToString(e) + "{" + n + "}"
    }
  }

  /**
   * @param e une expression régulière
   * @return une liste de bases obtenue en déroulant e tout le temps de la même manière.
   * @note Indiquez ici vos choix réalisés pour les répétitions, les choix, les Nqb.
   */
  // TODO V1
  def deroule(e: RExp): Option[List[Base]] = ???

}