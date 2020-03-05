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
