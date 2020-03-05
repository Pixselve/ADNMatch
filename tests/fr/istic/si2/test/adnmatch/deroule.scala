package fr.istic.si2.test.adnmatch

import fr.istic.si2.adnmatch
import org.junit.Test
import org.junit.Assert._
import util.Random
import fr.istic.si2.testerApp._
import fr.istic.si2.moreAssertions._
import fr.istic.si2.math._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch.RExpMatcher._
import fr.istic.si2.adnmatchlib._

class deroule {
  @Test
  def impossible(): Unit = {
    assertEquals(None, deroule(Impossible))
  }

  @Test
  def vide(): Unit = {
    assertEquals(None, deroule(Vide))
  }

  @Test
  def nqb(): Unit = {
    assertEquals(Some(List(A)), deroule(Nqb))
  }

  @Test
  def uneBase(): Unit = {
    assertEquals(Some(List(A)), deroule(UneBase(A)))
  }

  @Test
  def choix(): Unit = {
    assertEquals(Some(List(A)), deroule(Choix(UneBase(A), UneBase(T))))
  }

  @Test
  def concat(): Unit = {
    assertEquals(Some(List(A, T)), deroule(Concat(UneBase(A), UneBase(T))))
  }

  @Test
  def repeat(): Unit = {
    assertEquals(Some(List(A, A, A, A, A)), deroule(Repete(UneBase(A))))
  }

  @Test
  def repeatN(): Unit = {
    assertEquals(Some(List(A, A)), deroule(NFois(UneBase(A), 2)))
  }
}
