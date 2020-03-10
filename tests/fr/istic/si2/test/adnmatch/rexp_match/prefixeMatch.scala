package fr.istic.si2.test.adnmatch.rexp_match

import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch._
import org.junit.Assert._
import org.junit.Test
import fr.istic.si2.adnmatch.RExpMatcher._

class prefixeMatch {
  @Test
  def singleBase(): Unit = {
    assertEquals(Some(List(A, G, G, C)), prefixeMatch(UneBase(A), List(T, T, A, G, G, C)))
  }

  @Test
  def concatTwoSingleBase(): Unit = {
    assertEquals(Some(List(A, T, G, G, T, A)), prefixeMatch(Concat(UneBase(A), UneBase(T)), List(C, C, A, T, G, G, T, A)))
    assertEquals(Some(List(A, T, G, G, T, A)), prefixeMatch(Concat(UneBase(A), UneBase(T)), List(A, C, C, A, T, G, G, T, A)))
  }

  @Test
  def choiceTwoSingleBase(): Unit = {
    assertEquals(Some(List(A, C, T, C, T, C)), prefixeMatch(Choix(UneBase(A), UneBase(G)), List(C, C, A, C, T, C, T, C)))
    assertEquals(Some(List(G, C, T, C, T, C)), prefixeMatch(Choix(UneBase(A), UneBase(G)), List(C, C, G, C, T, C, T, C)))
  }

  @Test
  def repeatSingleBase(): Unit = {
//    assertEquals("Do not starts with a T, search for a repetition of 3 T", Some(List(T, T, T, A, G, A)), prefixeMatch(NFois(UneBase(T), 3), List(C, C, T, T, T, A, G, A)))
    assertEquals("Starts with a T, search for a repetition of 3 T", Some(List(T, T, T, A, G, A)), prefixeMatch(NFois(UneBase(T), 3), List(T, A, T, T, C, C, T, T, T, A, G, A)))

  }

}
