package fr.istic.si2.test.adnmatch.rexp_match

import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch._
import org.junit.Assert._
import org.junit.Test
import fr.istic.si2.adnmatch.RExpMatcher._

class derivee {
  @Test
  def impossible(): Unit = {
    assertEquals(Impossible, derivee(Impossible, A))
  }

  @Test
  def vide(): Unit = {
    assertEquals(Impossible, derivee(Vide, A))
  }

  @Test
  def singleBaseSame(): Unit = {
    assertEquals(Vide, derivee(UneBase(A), A))
  }

  @Test
  def singleBaseDifferent(): Unit = {
    assertEquals(Impossible, derivee(UneBase(C), A))
  }

  @Test
  def repeatSingleBaseDifferent(): Unit = {
    assertEquals(Impossible, derivee(Repete(UneBase(C)), A))
  }

  @Test
  def repeat(): Unit = {
//    assertArrayEquals("Where the repetition is empty")
  }
  @Test
  def choiceGoodCase(): Unit = {
    assertEquals(Vide, derivee(Choix(UneBase(A), UneBase(C)), A))
    assertEquals(Vide, derivee(Choix(UneBase(A), UneBase(C)), C))
  }

  @Test
  def choiceBadCase(): Unit = {
    assertEquals(Impossible, derivee(Choix(UneBase(A), UneBase(C)), T))
    assertEquals(Impossible, derivee(Choix(UneBase(A), UneBase(C)), G))
  }

  @Test
  def anyBase(): Unit = {
    assertEquals(Vide, derivee(Nqb, A))
    assertEquals(Vide, derivee(Nqb, C))
    assertEquals(Vide, derivee(Nqb, G))
    assertEquals(Vide, derivee(Nqb, T))
  }

  @Test
  def nRepeat(): Unit = {
    assertEquals(Impossible, derivee(NFois(UneBase(A), 0), A))
    assertEquals(Vide, derivee(NFois(UneBase(A), 1), A))
    assertEquals(UneBase(A), derivee(NFois(UneBase(A), 2), A))
    assertEquals(NFois(UneBase(A), 2), derivee(NFois(UneBase(A), 3), A))
    assertEquals(NFois(Choix(UneBase(A), UneBase(T)), 2), derivee(NFois(Choix(UneBase(A), UneBase(T)), 3), A))
    assertEquals(Choix(UneBase(A), UneBase(T)), derivee(NFois(Choix(UneBase(A), UneBase(T)), 2), A))
  }

  @Test
  def concatE1NotEmpty(): Unit = {
    assertEquals("TA does not starts by an A", Impossible, derivee(Concat(UneBase(T), UneBase(A)), A))
    assertEquals("AT starts by an A and leave T", UneBase(T), derivee(Concat(UneBase(A), UneBase(T)), A))
    assertEquals("(A|T)(G|C) could starts with a A and leaves (G|C)", Choix(UneBase(G), UneBase(C)), derivee(Concat(Choix(UneBase(A), UneBase(T)), Choix(UneBase(G), UneBase(C))), A))
  }

}
