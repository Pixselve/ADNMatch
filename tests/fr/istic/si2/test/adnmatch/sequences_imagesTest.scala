package fr.istic.si2.test.adnmatch

import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.SequencesImages.{color_Marque, fontSizeBase, _}
import fr.istic.si2.scribble.{Below, Beside, Color, Empty, FillColor, Text}
import org.junit.Assert.assertEquals
import org.junit.Test

class sequences_imagesTest {
  /**
   * Tests
   */
  @Test
  def ligneTest(): Unit = {
    val liste: List[(Marqueur, Base)] = List((Marque, A), (NonMarque, T), (Marque, G))
    val liste_el1_1: List[List[(Marqueur, Base)]] = List(List((Marque, A)), List((NonMarque, T)), List((Marque, G)))
    assertEquals("1 element par ligne, sequence simple", liste_el1_1, lignes(liste, 1))
    val liste_el2_2: List[List[(Marqueur, Base)]] = List(List((Marque, A), (NonMarque, T)), List((Marque, G)))
    assertEquals("1 element par ligne, sequence simple", liste_el2_2, lignes(liste, 2))
    val liste_el3_3: List[List[(Marqueur, Base)]] = List(List((Marque, A), (NonMarque, T), (Marque, G)))
    assertEquals("1 element par ligne, sequence simple", liste_el3_3, lignes(liste, 3))

    val liste_1: List[(Marqueur, Base)] =
      List((Marque, C), (NonMarque, T), (Marque, G), (Marque, C), (NonMarque, T), (NonMarque, C), (Marque, A),
        (NonMarque, G), (NonMarque, G), (Marque, A), (NonMarque, T), (Marque, C))
    val liste_el1: List[List[(Marqueur, Base)]] =
      List(List((Marque, C)), List((NonMarque, T)), List((Marque, G)), List((Marque, C)), List((NonMarque, T)),
        List((NonMarque, C)), List((Marque, A)), List((NonMarque, G)), List((NonMarque, G)), List((Marque, A)), List((NonMarque, T)), List((Marque, C)))
    assertEquals("1 element par ligne", liste_el1, lignes(liste_1, 1))
    val liste_el2: List[List[(Marqueur, Base)]] =
      List(List((Marque, C), (NonMarque, T)), List((Marque, G), (Marque, C)), List((NonMarque, T), (NonMarque, C)),
        List((Marque, A), (NonMarque, G)), List((NonMarque, G), (Marque, A)), List((NonMarque, T), (Marque, C)))
    assertEquals("2 element par ligne", liste_el2, lignes(liste_1, 2))
    val liste_el3: List[List[(Marqueur, Base)]] =
      List(List((Marque, C), (NonMarque, T), (Marque, G)), List((Marque, C), (NonMarque, T), (NonMarque, C)),
        List((Marque, A), (NonMarque, G), (NonMarque, G)), List((Marque, A), (NonMarque, T), (Marque, C)))
    assertEquals("3 element par ligne", liste_el3, lignes(liste_1, 3))
  }

  /**
   * Tests
   */
  @Test
  def baseMarqueToImageTest(): Unit = {
    val fontSizeBase: Int = 14
    val color_Marque: Color = new Color(255, 0, 0, 255)
    val color_NonMarque: Color = new Color(0, 0, 0, 255)

    assertEquals("A marqué", FillColor(Text("A", fontSizeBase), color_Marque),
      marqueurBaseToImage((Marque, A)))
    assertEquals("C marqué", FillColor(Text("C", fontSizeBase), color_Marque),
      marqueurBaseToImage((Marque, C)))
    assertEquals("T marqué", FillColor(Text("T", fontSizeBase), color_Marque),
      marqueurBaseToImage((Marque, T)))
    assertEquals("G marqué", FillColor(Text("G", fontSizeBase), color_Marque),
      marqueurBaseToImage((Marque, G)))
    assertEquals("A non marqué", FillColor(Text("A", fontSizeBase), color_NonMarque), marqueurBaseToImage((NonMarque, A)))
    assertEquals("C non marqué", FillColor(Text("C", fontSizeBase), color_NonMarque), marqueurBaseToImage((NonMarque, C)))
    assertEquals("T non marqué", FillColor(Text("T", fontSizeBase), color_NonMarque), marqueurBaseToImage((NonMarque, T)))
    assertEquals("G non marqué", FillColor(Text("G", fontSizeBase), color_NonMarque), marqueurBaseToImage((NonMarque, G)))
  }

  /**
   * Tests
   */
  @Test
  def imageUneLigneTest(): Unit = {
    val liste_el1_1: List[(Marqueur, Base)] = List((NonMarque, A),
      (NonMarque, T), (NonMarque, G))
    assertEquals("Une ligne ATG non marqué", Beside(FillColor(Text("A", fontSizeBase), color_NonMarque),
      Beside(FillColor(Text("T", fontSizeBase), color_NonMarque),
        Beside(FillColor(Text("G", fontSizeBase), color_NonMarque), Empty))), imageUneLigne(liste_el1_1))
    val liste_el1_2: List[(Marqueur, Base)] = List((Marque, A), (Marque, T), (Marque, G), (Marque, C))
    assertEquals("Une ligne ATGC marqué", Beside(FillColor(Text("A", fontSizeBase), color_Marque),
      Beside(FillColor(Text("T", fontSizeBase), color_Marque), Beside(FillColor(Text("G", fontSizeBase), color_Marque),
        Beside(FillColor(Text("C", fontSizeBase), color_Marque), Empty)))), imageUneLigne(liste_el1_2))
  }

  /**
   * Tests
   */
  @Test
  def imagePlusieursLignesTest(): Unit = {
    val liste_el: List[List[(Marqueur, Base)]] = List(List((Marque, A), (NonMarque, T)), List((Marque, G)))
    assertEquals("Deux lignes pour 3 bases marqués",
      Below(Beside(FillColor(Text("A", fontSizeBase), color_Marque), Beside(FillColor(Text("T", fontSizeBase), color_NonMarque),
        Empty)), Below(Beside(FillColor(Text("G", fontSizeBase), color_Marque), Empty), Empty)), imagePlusieursLignes(liste_el))
    val liste_el2: List[List[(Marqueur, Base)]] = List(List((Marque, A), (NonMarque, T), (Marque, G)))
    assertEquals("Deux lignes pour 3 bases marqués",
      Below(Beside(FillColor(Text("A", fontSizeBase), color_Marque), Beside(FillColor(Text("T", fontSizeBase), color_NonMarque),
        Beside(FillColor(Text("G", fontSizeBase), color_Marque), Empty))), Empty), imagePlusieursLignes(liste_el2))

  }
}
