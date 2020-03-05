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

class listeBasesToString {
  @Test
  def empty(): Unit = {
    assertEquals("", listeBasesToString(List()))
  }

  @Test
  def oneBase(): Unit = {
    assertEquals("A", listeBasesToString(List(A)))
  }

  @Test
  def twoBase(): Unit = {
    assertEquals("AT", listeBasesToString(List(A, T)))
  }

  @Test
  def threeBase(): Unit = {
    assertEquals("ATC", listeBasesToString(List(A, T, C)))
  }


}
