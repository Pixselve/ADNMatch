package fr.istic.si2.test.adnmatch.rexp_match

import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch._
import org.junit.Assert._
import org.junit.Test
import fr.istic.si2.adnmatch.RExpMatcher._

class matchComplet {
  @Test
  def singleBase(): Unit = {
    assertTrue("single base", matchComplet(UneBase(A), List(A)))
  }

  @Test
  def concatSingleBases(): Unit = {
    assertTrue("concat single bases", matchComplet(Concat(UneBase(A), UneBase(T)), List(A, T)))

  }

  @Test
  def choiceSingleBases(): Unit = {
    assertTrue("choice single bases 1/2", matchComplet(Choix(UneBase(A), UneBase(T)), List(T)))
    assertTrue("choice single bases 2/2", matchComplet(Choix(UneBase(A), UneBase(T)), List(A)))

  }

  @Test
  def repeatSingleBase(): Unit = {
    assertTrue("repeat", matchComplet(Repete(UneBase(A)), List(A)))

  }

  @Test
  def repeatChoice(): Unit = {
    assertTrue("repeatChoice", matchComplet(Repete(Choix(UneBase(A), UneBase(T))), List(A, T, T, T, A)))
  }

  @Test
  def repeatConsideredEmptyConcatWithSingleBase(): Unit = {
    assertTrue(matchComplet(Concat(Repete(UneBase(A)), UneBase(T)), List(T)))
  }
}
