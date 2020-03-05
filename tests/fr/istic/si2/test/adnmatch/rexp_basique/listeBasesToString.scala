package fr.istic.si2.test.adnmatch.rexp_basique

import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch._
import org.junit.Assert._
import org.junit.Test

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
