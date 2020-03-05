package fr.istic.si2.test.adnmatch.rexp_basique

import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch._
import org.junit.Assert._
import org.junit.Test

class getOrElseNil {
  @Test
  def noneEntry(): Unit = {
    assertEquals(Nil, getOrElseNil(None))
  }

  @Test
  def someEntry(): Unit = {
    assertEquals(List(A, T, C), getOrElseNil(Some(List(A, T, C))))
  }
}
