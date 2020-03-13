package fr.istic.si2.test.adnmatch

import fr.istic.si2.adnmatch.FonctionsRExp._
import fr.istic.si2.adnmatch._
import fr.istic.si2.adnmatchlib.litRExp
import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

class FonctionsRExpTest {
  /**
   * @param stringToCheck une expression régulière sous forme d'une chaine de caractère
   * @return si rExpToString de l'expression régulière donne bien la même chaine de caractère
   */
  def checkIfSame(stringToCheck: String): Boolean = {
    litRExp(stringToCheck) match {
      case None      => false
      case Some(exp) => rExpToString(exp) == stringToCheck
    }
  }

  /**
   * Tests de la fonction rExpToString
   */
  @Test def rExpToStringTest(): Unit = {
    assertTrue("Single base", checkIfSame("A"))
    assertTrue("Any Base", checkIfSame("."))
    assertTrue("Concatenate basic", checkIfSame("AT"))
    assertTrue("Concatenate group left", checkIfSame("(A|C)T"))
    assertTrue("Concatenate group right", checkIfSame("A(T|C)"))
    assertTrue("Concatenate group both sides", checkIfSame("(A|C)(T|C)"))
    assertTrue("Repeat basic", checkIfSame("A*"))
    assertTrue("Repeat group", checkIfSame("(A|C)*"))
    assertTrue("Repeat N time basic", checkIfSame("A{4}"))
    assertTrue("Repeat N time group", checkIfSame("(A|C){4}"))
    assertTrue("Empty", checkIfSame("%"))
    assertTrue("Impossible", checkIfSame("@"))
  }

  /**
   * Tests de la fonction listeBasesToString
   */
  @Test def listeBasesToStringTest(): Unit = {
    assertEquals("Empty", "", listeBasesToString(List()))
    assertEquals("1 base", "A", listeBasesToString(List(A)))
    assertEquals("2 bases", "AT", listeBasesToString(List(A, T)))
    assertEquals("3 bases", "ATC", listeBasesToString(List(A, T, C)))

  }

  /**
   * Tests de la fonction deroule
   */
  @Test def derouleTest(): Unit = {
    assertEquals("Impossible", None, deroule(Impossible))
    assertEquals("Empty", None, deroule(Vide))
    assertEquals("Any base", Some(List(A)), deroule(Nqb))
    assertEquals("Single base", Some(List(A)), deroule(UneBase(A)))
    assertEquals("Choice", Some(List(A)), deroule(Choix(UneBase(A), UneBase(T))))
    assertEquals("Concatenation", Some(List(A, T)), deroule(Concat(UneBase(A), UneBase(T))))
    assertEquals("Repeat", Some(List(A, A, A, A, A)), deroule(Repete(UneBase(A))))
    assertEquals("Repeat N time", Some(List(A, A)), deroule(NFois(UneBase(A), 2)))

  }
}