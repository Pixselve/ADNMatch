package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._

/**
 * Type algébrique décrivant les différents marqueurs
 * indiquant les résultats de recherche.
 */
sealed trait Marqueur

case object BelongToASubSequence extends Marqueur

case object DoNotBelongToASubSequence extends Marqueur

object RExpMatcher {

  /**
   * @param e une expression régulière
   * @param b une base azotée
   * @return la dérivée de Brzozowski de e par rapport à b
   */
  def derivee(e: RExp, b: Base): RExp = {
    e match {
      case Impossible    => Impossible
      case Vide          => Impossible
      case UneBase(base) => if (base == b) Vide else Impossible
      case Nqb           => Vide

      case NFois(e1, n)   => (derivee(e1, b), n) match {
        case (Impossible, _)   => Impossible
        case (_, 0)            => Impossible
        case (_, 1)            => Vide
        case (Vide, 2)         => e1
        case (Vide, _)         => NFois(e1, n - 1)
        case (expDeriveeE1, 2) => Concat(expDeriveeE1, e1)
        case (expDeriveeE1, _) => Concat(expDeriveeE1, NFois(e1, n - 1))
      }
      case Choix(e1, e2)  =>
        (derivee(e1, b), derivee(e2, b)) match {
          case (Impossible, Impossible)     => Impossible
          case (Vide, Vide)                 => Vide
          case (Impossible, expDeriveeE2)   => expDeriveeE2
          case (expDeriveeE1, Impossible)   => expDeriveeE1
          case (expDeriveeE1, expDeriveeE2) => Choix(expDeriveeE1, expDeriveeE2)
        }
      case Repete(e1)     =>
        derivee(e1, b) match {
          case Impossible => Impossible
          case Vide       => Repete(e1)
          case value      => Concat(value, Repete(e1))
        }
      case Concat(e1, e2) =>
        if (checkIfRExpEqualsEmpty(e1)) {
          //          if e1 if empty
          (derivee(e1, b), derivee(e2, b)) match {
            case (Impossible, _)              => Impossible
            case (_, Impossible)              => Impossible
            case (Vide, Vide)                 => Vide
            case (Vide, expDeriveeE2)         => Choix(e2, expDeriveeE2)
            case (expDeriveeE1, Vide)         => Concat(expDeriveeE1, e2)
            case (expDeriveeE1, expDeriveeE2) => Choix(Concat(expDeriveeE1, e2), expDeriveeE2)
          }
        } else {
          //          else
          derivee(e1, b) match {
            case Vide       => e2
            case Impossible => Impossible
            case _          => Concat(derivee(e1, b), e2)
          }
        }
    }
  }

  /**
   * @param e une expression régulière
   * @return vrai ssi e représente une séquence vide
   */
  def checkIfRExpEqualsEmpty(e: RExp): Boolean = {
    e match {
      case Impossible     => true
      case Vide           => true
      case NFois(_, 0)    => true
      case UneBase(_)     => false
      case Nqb            => false
      case NFois(e, _)    => checkIfRExpEqualsEmpty(e)
      case Repete(e)      => checkIfRExpEqualsEmpty(e)
      case Concat(e1, e2) => checkIfRExpEqualsEmpty(e1) && checkIfRExpEqualsEmpty(e2)
      case Choix(e1, e2)  => checkIfRExpEqualsEmpty(e1) || checkIfRExpEqualsEmpty(e2)
    }
  }

  /**
   * @param e  une expression régulière
   * @param lb une liste de bases azotées
   * @return vrai ssi la liste lb entière est décrite par e
   */
  def matchComplet(e: RExp, lb: List[Base]): Boolean = {
    lb match {
      case Nil           => false
      case first :: rest =>
        (derivee(e, first), rest) match {
          case (Vide, Nil)     => true
          case (_, Nil)        => false
          case (Impossible, _) => false
          case (Vide, _)       => false
          case (value, _)      => matchComplet(value, rest)
        }
    }
  }

  /**
   * @param lb une liste de bases azotées
   * @return la liste des bases de lb, dans l'ordre, marquées pour indiquer
   *         que la totalité de lb est décrite
   */
  def sequenceDecrite(lb: List[Base]): List[(Marqueur, Base)] = {
    lb match {
      case Nil           => Nil
      case first :: rest => (BelongToASubSequence, first) :: sequenceDecrite(rest)
    }
  }

  /**
   * @param lb une liste de bases azotées
   * @return la liste des bases de lb, dans l'ordre, marquées pour indiquer
   *         que la totalité de lb n'est pas décrite
   */
  def sequenceNonDecrite(lb: List[Base]): List[(Marqueur, Base)] = {
    lb match {
      case Nil           => Nil
      case first :: rest => (DoNotBelongToASubSequence, first) :: sequenceDecrite(rest)
    }
  }


  def listWithoutLastElement(list: List[Base]): List[Base] = {
    list match {
      case Nil             => Nil
      case _ :: Nil        => Nil
      case element :: rest => List(element) ++ listWithoutLastElement(rest)
    }
  }

  /**
   * @param e  une expression régulière
   * @param lb une liste de bases azotées
   * @return s'il existe, le plus petit prefixe de lb qui est décrit par e
   */
  // TODO V2
  def prefixeMatchBACK(e: RExp, lb: List[Base]): Option[List[Base]] = {
    lb match {
      case Nil           => None
      case first :: rest =>
        if (checkIfThereIsARegExMatch(e, first :: rest)) {
          Some(first :: rest)
        } else {
          prefixeMatch(e, rest)
        }
    }
  }

  def prefixeMatch(e: RExp, lb: List[Base]): Option[List[Base]] = {
    (e, lb) match {
      case (Vide, _)          => Some(lb)
      case (Impossible, _)    => None
      case (_, Nil)           => None
      case (_, first :: rest) =>
        prefixeMatch(derivee(e, first), lb) match {
          case None    => prefixeMatch(e, rest)
          case Some(_) => Some(lb)
        }
    }
  }

  def checkIfThereIsARegExMatch(e: RExp, lb: List[Base]): Boolean = {
    lb match {
      case Nil           => false
      case first :: rest => derivee(e, first) match {
        case Vide       => true
        case Impossible => false
        case value      => checkIfThereIsARegExMatch(value, rest)
      }
    }



    //    (e, lb) match {
    //      case (Vide, _)              => true
    //      case (Impossible, _)        => false
    //      case (_, Nil)               => false
    //      case (value, first :: rest) => checkIfThereIsARegExMatch(derivee(value, first), rest)
    //    }
  }

  /**
   * @param pref une liste de bases azotées *préfixe* de lb
   * @param lb   une liste de bases azotées
   * @return la sous-liste de lb située après le préfixe pref
   */
  // TODO V2
  def suppPrefixe(pref: List[Base], lb: List[Base]): List[Base] = ???

  /**
   * @param e  une expression régulière
   * @param lb une liste de bases
   * @return une liste  (m1, base1)::...::(mN,baseN)::Nil, qui marque,
   *         base après base, les sous-listes de lb décrites par e.
   *         Les basei sont les bases de lb dans l'ordre.
   */
  // TODO V2
  def tousLesMatchs(e: RExp, lb: List[Base]): List[(Marqueur, Base)] = ???

  /**
   * @param lbm une liste de bases marquées selon un résultat de recherche
   * @return une description textuelle du résultat pour l'utilisateur
   */
  // TODO V2
  def messageResultat(lbm: List[(Marqueur, Base)]): String = ???

  /**
   * @param lb une liste de bases azotées marquées
   * @return liste des mêmes bases que lb, mais où tous les marqueurs indiquent
   *         une non-correspondance
   */
  // TODO V3
  def annulerResultat(lb: List[(Marqueur, Base)]): List[(Marqueur, Base)] = ???

  /**
   * @param lbm une liste de bases azotées marquées
   * @return la liste des bases de lbm dont on a oublié les marqueurs, en conservant l'ordre
   */
  // TODO V3
  def sansMarqueurs(lbm: List[(Marqueur, Base)]): List[Base] = ???

}