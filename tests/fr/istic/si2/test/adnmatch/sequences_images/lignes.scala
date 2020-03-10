package fr.istic.si2.test.adnmatch.sequences_images

import fr.istic.si2.adnmatch.{A, Impossible}
import fr.istic.si2.adnmatch.RExpMatcher.derivee
import org.junit.Assert.assertEquals
import org.junit.Test

class lignes {

  // 2 par ligne
  @Test
  def Deux_par_ligne(): Unit = {
    assertEquals(Impossible, derivee(Impossible, A))
  }
}
