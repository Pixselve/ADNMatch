package fr.istic.si2.test.adnmatch.rexp_match

import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch._
import org.junit.Assert._
import org.junit.Test
import fr.istic.si2.adnmatch.RExpMatcher._

class sansMarqueurs {
  @Test
  def test(): Unit = {
    assertEquals("first element marked", List(A, T, G), sansMarqueurs(List((Marque, A), (NonMarque, T), (NonMarque, G))))
    assertEquals("middle element marked", List(A, T, G), sansMarqueurs(List((NonMarque, A), (Marque, T), (NonMarque, G))))
    assertEquals("last element marked", List(A, T, G), sansMarqueurs(List((NonMarque, A), (NonMarque, T), (Marque, G))))
    assertEquals("two elements marked, first and last", List(A, T, G), sansMarqueurs(List((Marque, A), (NonMarque, T), (Marque, G))))
    assertEquals("two elements marked, that follow each other", List(A, T, G), sansMarqueurs(List((NonMarque, A), (Marque, T), (Marque, G))))

  }
}
