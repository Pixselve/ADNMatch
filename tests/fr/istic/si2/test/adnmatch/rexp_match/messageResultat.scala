package fr.istic.si2.test.adnmatch.rexp_match

import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch._
import org.junit.Assert._
import org.junit.Test
import fr.istic.si2.adnmatch.RExpMatcher._

class messageResultat {
  @Test
  def withoutSubSequences(): Unit = {
    assertEquals("Nil list", "Il n'y a pas de sous-séquence", messageResultat(Nil))
    assertEquals("1 element not marked", "Il n'y a pas de sous-séquence", messageResultat(List((NonMarque, A))))
    assertEquals("2 elements not marked", "Il n'y a pas de sous-séquence", messageResultat(List((NonMarque, A), (NonMarque, T))))
    assertEquals("3 elements not marked", "Il n'y a pas de sous-séquence", messageResultat(List((NonMarque, A), (NonMarque, T), (NonMarque, G))))
  }

  @Test
  def withSubSequences(): Unit = {
    assertEquals("first element marked", "Il y a au moins une sous-séquence", messageResultat(List((Marque, A), (NonMarque, T), (NonMarque, G))))
    assertEquals("middle element marked", "Il y a au moins une sous-séquence", messageResultat(List((NonMarque, A), (Marque, T), (NonMarque, G))))
    assertEquals("last element marked", "Il y a au moins une sous-séquence", messageResultat(List((NonMarque, A), (NonMarque, T), (Marque, G))))
    assertEquals("two elements marked, first and last", "Il y a au moins une sous-séquence", messageResultat(List((Marque, A), (NonMarque, T), (Marque, G))))
    assertEquals("two elements marked, that follow each other", "Il y a au moins une sous-séquence", messageResultat(List((NonMarque, A), (Marque, T), (Marque, G))))
  }
}
