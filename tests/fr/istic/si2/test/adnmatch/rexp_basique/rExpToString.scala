package fr.istic.si2.test.adnmatch.rexp_basique

import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatchlib._
import org.junit.Assert._
import org.junit.Test


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
    assertTrue(checkIfSame("(A|C)T"))
  }

  @Test
  def concatenateGroupRight(): Unit = {
    assertTrue(checkIfSame("A(T|C)"))
  }

  @Test
  def concatenateGroupBoth(): Unit = {
    assertTrue(checkIfSame("(A|C)(T|C)"))
  }

  @Test
  def repeat(): Unit = {
    assertTrue(checkIfSame("A*"))
  }

  @Test
  def repeatGroup(): Unit = {
    assertTrue(checkIfSame("(A|C)*"))
  }

  @Test
  def repeatN(): Unit = {
    assertTrue(checkIfSame("A{4}"))
  }

  @Test
  def repeatGroupN(): Unit = {
    assertTrue(checkIfSame("(A|C){4}"))
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