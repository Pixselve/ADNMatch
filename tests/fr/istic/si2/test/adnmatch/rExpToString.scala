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


class rExpToString {

  def checkIfSame(stringToCheck: String): Boolean = {
    litRExp(stringToCheck) match {
      case None => false
      case Some(exp) => rExpToString(exp) == stringToCheck
    }
  }


  @Test
  def oneBase(): Unit = {
    assertTrue(checkIfSame("A"))
  }

  @Test
  def anyBase(): Unit = {
    assertTrue(checkIfSame("."))
  }

  @Test
  def concatenate(): Unit = {
    assertTrue(checkIfSame("AT"))
  }

  @Test
  def concatenateGroupLeft(): Unit = {
    assertTrue(checkIfSame("(A)T"))
  }

  @Test
  def concatenateGroupRight(): Unit = {
    assertTrue(checkIfSame("A(T)"))
  }

  @Test
  def concatenateGroupBoth(): Unit = {
    assertTrue(checkIfSame("(A)(T)"))
  }

  @Test
  def repeat(): Unit = {
    assertTrue(checkIfSame("A*"))
  }

  @Test
  def repeatGroup(): Unit = {
    assertTrue(checkIfSame("(A)*"))
  }

  @Test
  def repeatN(): Unit = {
    assertTrue(checkIfSame("A{4}"))
  }

  @Test
  def repeatGroupN(): Unit = {
    assertTrue(checkIfSame("(A){4}"))
  }

  @Test
  def impossible(): Unit = {
    assertTrue(checkIfSame("%"))
  }

  @Test
  def empty(): Unit = {
    assertTrue(checkIfSame("@"))
  }
}
