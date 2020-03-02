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

  def checkIfSame(): Boolean = {

  }


  @Test
  def oneBase(): Unit = {
    assertTrue(rExpToString(UneBase(A)) == "A")
  }

  @Test
  def anyBase(): Unit = {
    assertTrue(rExpToString(Nqb) == ".")
  }

  @Test
  def concatenate(): Unit = {
    assertTrue(rExpToString(Concat(UneBase(A), UneBase(T))) == "AT")
  }

  @Test
  def repeat(): Unit = {
    assertTrue(rExpToString(Repete(UneBase(A))) == "A*")
  }

  @Test
  def repeatN(): Unit = {
    assertTrue(rExpToString(NFois(UneBase(A), 5)) == "A{5}")
  }

  @Test
  def impossible(): Unit = {
    assertTrue(rExpToString(Impossible) == "@")
  }

  @Test
  def empty(): Unit = {
    assertTrue(rExpToString(Vide) == "%")
  }

  @Test
  def complexAllInOne1(): Unit = {
    assertTrue(rExpToString(Vide) == "A(TG)*A")
  }

  @Test
  def complexAllInOne2(): Unit = {
    assertTrue(rExpToString(Vide) == "%")
  }

  @Test
  def complexAllInOne3(): Unit = {
    assertTrue(rExpToString(Vide) == "%")
  }
}
