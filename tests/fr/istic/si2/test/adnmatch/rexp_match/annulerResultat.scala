package fr.istic.si2.test.adnmatch.rexp_match

import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch._
import org.junit.Assert._
import org.junit.Test
import fr.istic.si2.adnmatch.RExpMatcher._

class annulerResultat {
  @Test
  def test(): Unit = {
    assertEquals("first element marked", List((NonMarque, A), (NonMarque, T), (NonMarque, G)), annulerResultat(List((Marque, A), (NonMarque, T), (NonMarque, G))))
    assertEquals("middle element marked", List((NonMarque, A), (NonMarque, T), (NonMarque, G)), annulerResultat(List((NonMarque, A), (Marque, T), (NonMarque, G))))
    assertEquals("last element marked", List((NonMarque, A), (NonMarque, T), (NonMarque, G)), annulerResultat(List((NonMarque, A), (NonMarque, T), (Marque, G))))
    assertEquals("two elements marked, first and last", List((NonMarque, A), (NonMarque, T), (NonMarque, G)), annulerResultat(List((Marque, A), (NonMarque, T), (Marque, G))))
    assertEquals("two elements marked, that follow each other", List((NonMarque, A), (NonMarque, T), (NonMarque, G)), annulerResultat(List((NonMarque, A), (Marque, T), (Marque, G))))

  }
}
