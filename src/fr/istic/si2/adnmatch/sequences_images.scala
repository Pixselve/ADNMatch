package fr.istic.si2.adnmatch

import fr.istic.si2.scribble._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._

object SequencesImages {

  /**
   * @param lmb    une liste de bases marquées
   * @param tligne entier strictement positif, représentant la taille d'une ligne en nombre de bases marquées
   * @return une liste contenant des sous-listes de lmb, toutes de taille tligne, sauf la dernière qui
   *         peut être de taille inférieure.
   */
  def lignes(lmb: List[(Marqueur, Base)], tligne: Int): List[List[(Marqueur, Base)]] = {
    recupereReste(lmb, tligne) match {
      case Nil  => List(lmb)
      case rest: List[(Marqueur, Base)] => List(recupereLigne(lmb, tligne)) ++ lignes(rest, tligne)
    }
  }

  /**
   * @param lmb    une liste de bases marquées
   * @param tligne entier strictement positif, représentant la taille d'une ligne en nombre de bases marquées
   * @return la liste lmb sans les tligne premieres base marquées
   */
  def recupereReste(lmb: List[(Marqueur, Base)], tligne: Int): List[(Marqueur, Base)] = {
    tligne match {
      case 0 => lmb
      case _ => lmb match {
        case Nil           => Nil
        case _ :: rest => recupereReste(rest, tligne - 1)
      }
    }
  }

  /**
   * @param lmb    une liste de bases marquées
   * @param tligne entier strictement positif, représentant la taille d'une ligne en nombre de bases marquées
   * @return les "tligne" premieres bases marquées de la liste lmb
   */
  def recupereLigne(lmb: List[(Marqueur, Base)], tligne: Int): List[(Marqueur, Base)] = {
    tligne match {
      case 0 => Nil
      case _ => lmb match {
        case Nil           => Nil
        case first :: rest => List(first) ++ recupereLigne(rest, tligne - 1)
      }
    }
  }

  /**
   * Taille du texte à utiliser pour représenter
   * graphiquement les bases azotées.
   */
  val fontSizeBase: Int = 14

  /**
   * Couleur du texte quand il est marqué
   */
  val color_Marque: Color = Color(255, 94, 25, 255)

  /**
   * Couleur du texte quand il est marqué variante 2
   */
  val color_MarqueOdd: Color = Color(229, 25, 255, 255)
  /**
   * Couleur du texte quand il n'est pas marqué
   */
  val color_NonMarque: Color = Color(0, 0, 0, 255)

  /**
   * @param mb une base azotée marquée
   * @return une image représentant la base avec son marqueur visuel
   */
  def marqueurBaseToImage(mb: (Marqueur, Base)): Image = {
    mb match {
      case (Marque, base)    => FillColor(Text(base.toString, fontSizeBase), color_Marque)
      case (MarqueOdd, base) => FillColor(Text(base.toString, fontSizeBase), color_MarqueOdd)
      case (NonMarque, base) => FillColor(Text(base.toString, fontSizeBase), color_NonMarque)
    }
  }

  /**
   * @param ligne une liste de bases azotées marquées
   * @return une image représentant les bases marquées de ligne, dans l'ordre, toutes sur la même ligne
   */
  def imageUneLigne(ligne: List[(Marqueur, Base)]): Image = {
    ligne match {
      case Nil           => Empty
      case first :: rest => Beside(marqueurBaseToImage(first), imageUneLigne(rest))
    }
  }

  /**
   * @param llignes une liste de listes de bases azotées marquées
   * @return une image représentant les bases marquées de llignes, dans l'ordre.
   *         Chaque élément de llignes est sur une ligne distincte.
   *         Les lignes sont visualisées les unes en dessous des autres.
   */
  def imagePlusieursLignes(llignes: List[List[(Marqueur, Base)]]): Image = {
    llignes match {
      case Nil           => Empty
      case first :: rest => Below(imageUneLigne(first), imagePlusieursLignes(rest))
    }
  }

}