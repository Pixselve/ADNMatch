package fr.istic.si2.test.adnmatch

import fr.istic.si2.adnmatch.RExpMatcher._
import fr.istic.si2.adnmatch.{A, G, T, UneBase, _}
import org.junit.Test
import org.junit.Assert._

class RExpMatcherTest {
  /**
   * Tests de la fonction sansMarqueurs
   */
  @Test def sansMarqueursTest(): Unit = {
    assertEquals("first element marked", List(A, T, G), sansMarqueurs(List((Marque, A), (NonMarque, T), (NonMarque, G))))
    assertEquals("middle element marked", List(A, T, G), sansMarqueurs(List((NonMarque, A), (Marque, T), (NonMarque, G))))
    assertEquals("last element marked", List(A, T, G), sansMarqueurs(List((NonMarque, A), (NonMarque, T), (Marque, G))))
    assertEquals("two elements marked, first and last", List(A, T, G), sansMarqueurs(List((Marque, A), (NonMarque, T), (Marque, G))))
    assertEquals("two elements marked, that follow each other", List(A, T, G), sansMarqueurs(List((NonMarque, A), (Marque, T), (Marque, G))))
  }

  /**
   * Tests de la fonction tousLesMatchs
   */
  @Test def tousLesMatchsTest(): Unit = {
    assertEquals("Empty List", List(), tousLesMatchs(UneBase(T), List()))
    assertEquals("One element marked", List((Marque, A)), tousLesMatchs(UneBase(A), List(A)))
    assertEquals("One element not marked", List((NonMarque, A)), tousLesMatchs(UneBase(T), List(A)))
    assertEquals("Two element marked", List((Marque, A), (Marque, T)), tousLesMatchs(Concat(UneBase(A), UneBase(T)), List(A, T)))
    assertEquals("Two element not marked", List((NonMarque, A), (NonMarque, T)), tousLesMatchs(Concat(UneBase(T), UneBase(A)), List(A, T)))
    assertEquals("1 of 2 element marked (first)", List((Marque, A), (NonMarque, T)), tousLesMatchs(UneBase(A), List(A, T)))
    assertEquals("1 of 2 element marked (last)", List((NonMarque, A), (Marque, T)), tousLesMatchs(UneBase(T), List(A, T)))
  }

  /**
   * Tests de la fonction sequenceNonDecrite
   */
  @Test def sequenceNonDecriteTest(): Unit = {
    assertEquals("Empty list", List(), sequenceNonDecrite(List()))
    assertEquals("1 element", List((NonMarque, A)), sequenceNonDecrite(List(A)))
    assertEquals("2 elements", List((NonMarque, A), (NonMarque, T)), sequenceNonDecrite(List(A, T)))
    assertEquals("3 elements", List((NonMarque, A), (NonMarque, T), (NonMarque, G)), sequenceNonDecrite(List(A, T, G)))
  }

  /**
   * Tests de la fonction messageResultat
   */
  @Test def messageResultatTest(): Unit = {
    assertEquals("Nil list", "Il n'y a pas de sous-séquence", messageResultat(Nil))
    assertEquals("1 element not marked", "Il n'y a pas de sous-séquence", messageResultat(List((NonMarque, A))))
    assertEquals("2 elements not marked", "Il n'y a pas de sous-séquence", messageResultat(List((NonMarque, A), (NonMarque, T))))
    assertEquals("3 elements not marked", "Il n'y a pas de sous-séquence", messageResultat(List((NonMarque, A), (NonMarque, T), (NonMarque, G))))
    assertEquals("first element marked", "Il y a au moins une sous-séquence", messageResultat(List((Marque, A), (NonMarque, T), (NonMarque, G))))
    assertEquals("middle element marked", "Il y a au moins une sous-séquence", messageResultat(List((NonMarque, A), (Marque, T), (NonMarque, G))))
    assertEquals("last element marked", "Il y a au moins une sous-séquence", messageResultat(List((NonMarque, A), (NonMarque, T), (Marque, G))))
    assertEquals("two elements marked, first and last", "Il y a au moins une sous-séquence", messageResultat(List((Marque, A), (NonMarque, T), (Marque, G))))
    assertEquals("two elements marked, that follow each other", "Il y a au moins une sous-séquence",
      messageResultat(List((NonMarque, A), (Marque, T), (Marque, G))))
  }

  /**
   * Tests de la fonction annulerResultat
   */
  @Test def annulerResultatTest(): Unit = {
    assertEquals("first element marked", List((NonMarque, A), (NonMarque, T),
      (NonMarque, G)), annulerResultat(List((Marque, A), (NonMarque, T), (NonMarque, G))))
    assertEquals("middle element marked", List((NonMarque, A), (NonMarque, T),
      (NonMarque, G)), annulerResultat(List((NonMarque, A), (Marque, T), (NonMarque, G))))
    assertEquals("last element marked", List((NonMarque, A), (NonMarque, T),
      (NonMarque, G)), annulerResultat(List((NonMarque, A), (NonMarque, T), (Marque, G))))
    assertEquals("two elements marked, first and last", List((NonMarque, A),
      (NonMarque, T), (NonMarque, G)), annulerResultat(List((Marque, A), (NonMarque, T), (Marque, G))))
    assertEquals("two elements marked, that follow each other", List((NonMarque, A),
      (NonMarque, T), (NonMarque, G)), annulerResultat(List((NonMarque, A), (Marque, T), (Marque, G))))
  }

  /**
   * Tests de la fonction checkIfRExpEqualsEmpty
   */
  @Test def checkIfRExpEqualsEmptyTest(): Unit = {
    assertTrue("Empty", checkIfRExpEqualsEmpty(Vide))
    assertFalse("Impossible", checkIfRExpEqualsEmpty(Impossible))
  }

  /**
   * Tests de la fonction derivee
   */
  @Test def deriveeTest(): Unit = {
    assertEquals("Impossible", Impossible, derivee(Impossible, A))
    assertEquals("Empty", Impossible, derivee(Vide, A))
    assertEquals("Single base valid", Vide, derivee(UneBase(A), A))
    assertEquals("Single base invalid", Impossible, derivee(UneBase(C), A))
    assertEquals("Repeat single base invalid", Impossible, derivee(Repete(UneBase(C)), A))
    assertEquals("Concat single bases", Concat(UneBase(T), Repete(Concat(UneBase(A), UneBase(T)))), derivee(Repete(Concat(UneBase(A), UneBase(T))), A))
    assertEquals("Choice single base valid", Vide, derivee(Choix(UneBase(A), UneBase(C)), A))
    assertEquals("Choice single base valid (other)", Vide, derivee(Choix(UneBase(A), UneBase(C)), C))
    assertEquals("Choice single base invalid", Impossible, derivee(Choix(UneBase(A), UneBase(C)), T))
    assertEquals("Any base with single base", Vide, derivee(Nqb, A))
    assertEquals("Any base with single base", Vide, derivee(Nqb, C))
    assertEquals("Any base with single base", Vide, derivee(Nqb, G))
    assertEquals("Any base with single base", Vide, derivee(Nqb, T))
    assertEquals("", Impossible, derivee(NFois(UneBase(A), 0), A))
    assertEquals("Repeat one time", Vide, derivee(NFois(UneBase(A), 1), A))
    assertEquals("Repeat two times", UneBase(A), derivee(NFois(UneBase(A), 2), A))
    assertEquals("Repeat three times", NFois(UneBase(A), 2), derivee(NFois(UneBase(A), 3), A))
    assertEquals("Repeat three times choice", NFois(Choix(UneBase(A), UneBase(T)), 2), derivee(NFois(Choix(UneBase(A), UneBase(T)), 3), A))
    assertEquals("Repeat two times choice", Choix(UneBase(A), UneBase(T)), derivee(NFois(Choix(UneBase(A), UneBase(T)), 2), A))
    assertEquals("TA does not starts by an A", Impossible, derivee(Concat(UneBase(T), UneBase(A)), A))
    assertEquals("AT starts by an A and leave T", UneBase(T), derivee(Concat(UneBase(A), UneBase(T)), A))
    assertEquals("(A|T)(G|C) could starts with a A and leaves (G|C)",
      Choix(UneBase(G), UneBase(C)), derivee(Concat(Choix(UneBase(A), UneBase(T)), Choix(UneBase(G), UneBase(C))), A))
  }

  /**
   * Tests de la fonction prefixeMatch
   */
  @Test def prefixeMatchTest(): Unit = {
    assertEquals("One element list which is prefix", Some(List(A)), prefixeMatch(UneBase(A), List(A)))
    assertEquals("One element list which is not", None, prefixeMatch(UneBase(T), List(A)))
    assertEquals("Single base prefix", Some(List(A)), prefixeMatch(UneBase(A), List(A, G, G, C)))
    assertEquals("Concat prefix", Some(List(A, T)), prefixeMatch(Concat(UneBase(A), UneBase(T)), List(A, T, G, G, T, A)))
    assertEquals("Choice prefix", Some(List(A)), prefixeMatch(Choix(UneBase(A), UneBase(G)), List(A, C, T, C, T, C)))
    assertEquals("Repeat three times prefix", Some(List(T, T, T)), prefixeMatch(NFois(UneBase(T), 3), List(T, T, T, A, G, A)))
    assertEquals("None prefix", None, prefixeMatch(NFois(UneBase(T), 3), List(T, T, A, G, A)))

  }

  /**
   * Tests de la fonction sequenceDecrite
   */
  @Test def sequenceDecriteTest(): Unit = {
    assertEquals("Empty list", List(), sequenceDecrite(List()))
    assertEquals("1 element", List((Marque, A)), sequenceDecrite(List(A)))
    assertEquals("2 elements", List((Marque, A), (Marque, T)), sequenceDecrite(List(A, T)))
    assertEquals("3 elements", List((Marque, A), (Marque, T), (Marque, G)), sequenceDecrite(List(A, T, G)))
  }

  /**
   * Tests de la fonction matchComplet
   */
  @Test def matchCompletTest(): Unit = {
    assertTrue("single base", matchComplet(UneBase(A), List(A)))
    assertTrue("concat single bases", matchComplet(Concat(UneBase(A), UneBase(T)), List(A, T)))
    assertTrue("choice single bases 1/2", matchComplet(Choix(UneBase(A), UneBase(T)), List(T)))
    assertTrue("choice single bases 2/2", matchComplet(Choix(UneBase(A), UneBase(T)), List(A)))
    assertTrue("repeat", matchComplet(Repete(UneBase(A)), List(A)))
    assertTrue("repeatChoice", matchComplet(Repete(Choix(UneBase(A), UneBase(T))), List(A, T, T, T, A)))
  }

  /**
   * Tests de la fonction suppPrefixe
   */
  @Test def suppPrefixeTest(): Unit = {
    assertEquals("One element prefix", List(G, G, C), suppPrefixe(List(A), List(A, G, G, C)))
    assertEquals("Two elements prefix", List(G, G, T, A), suppPrefixe(List(A, T), List(A, T, G, G, T, A)))
    assertEquals("Three elements prefix", List(G, T, A), suppPrefixe(List(A, T, G), List(A, T, G, G, T, A)))
  }
}