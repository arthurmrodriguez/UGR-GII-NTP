
import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class ConjuntoSuite extends FunSuite {

   // Se importan las declaraciones  en ConjuntoFuncional
   import Conjunto._


   // Se crea un trait incluyendo tres conjuntos, que se
   // usan en cada test
   trait TestSets {
      val s1 = conjuntoUnElemento(1)
      val s2 = conjuntoUnElemento(2)
      val s3 = conjuntoUnElemento(3)
   }

   /**
     * Se comprueba que el elemento 1 esta contenido en s1
     */
   test("s1 contiene 1") {

      // Se crea instancia de los conjuntos
      new TestSets {
         // Si falla el assert se muestra el mensaje de error
         // que aparece como segundo argumento
         assert(s1(1), "fallo: s1 no contiene a 1")
      }
   }

   /**
     * Test for union
     */
   test("test de union") {
      new TestSets {
         val s = union(s1, s2)
         assert(s(1), "fallo: s no contiene a 1")
         assert(s(2), "fallo: s no contiene a 2")
         assert(!s(3), "fallo: s contiene a 3")
      }
   }

   /**
     * Otro test para la union
     */
   test("Test de union (general)") {
      val conjunto1 = Conjunto((x: Int) => x > 3)
      val conjunto2 = Conjunto((x: Int) => x > 5)

      // Conjunto union: enteros mayores de 3
      val conjuntoUnion:Conjunto = Conjunto.union(conjunto1, conjunto2)

      // 4, 5, 6 y 7  pertenecen a la union
      assert(conjuntoUnion(4))
      assert(conjuntoUnion(5))
      assert(conjuntoUnion(6))
      assert(conjuntoUnion(7))

      // 3 y 0 no pertenecen a la union
      assert(!conjuntoUnion(3))
      assert(!conjuntoUnion(0))
   }

   /**
     * Test para interseccion
     */
   test("Test de interseccion") {
      val conjunto1 = Conjunto((x: Int) => x > 3)
      val conjunto2 = Conjunto((x: Int) => x > 5)

      // Formacion de la interseccion: solo a partir de 5
      val conjuntoInterseccion = interseccion(conjunto1, conjunto2)

      // 6 pertenece
      assert(conjuntoInterseccion(6))

      // no 4 ni 5 pertenecen
      assert(!conjuntoInterseccion(4))
      assert(!conjuntoInterseccion(5))
   }

   /**
     * Test de diferencia
     */
   test("Test de diferencia") {
      val conjunto1 = Conjunto((x: Int) => x > 3)
      val conjunto2 = Conjunto((x: Int) => x < 10)

      // Diferencia: mayores de 3 pero no menores de 10
      val conjuntoDiferencia = diferencia(conjunto1, conjunto2)

      // 6 no pertenece y 11 si
      assert(!conjuntoDiferencia(6))
      assert(conjuntoDiferencia(11))
   }

   /**
     * Test de filtrado
     */
   test("Test de filtrado") {
      val conjunto1 = Conjunto((x: Int) => x > 3)
      val conjunto2 = Conjunto((x: Int) => x < 10)

      // Deja en conjunto1 los elementos de conjunto2
      val conjuntoFiltrado = filtrar(conjunto1, conjunto2.funcionCaracteristica)

      // 6 debe pertenecer y 11 no
      assert(conjuntoFiltrado(6))
      assert(!conjuntoFiltrado(11))
   }

   /**
     * Test de forall
     */
   test("Test de paraTodo") {
      val conjunto = Conjunto((x: Int) => x < 10)

      // No todos los elementos del conjunto son > 0
      assert(!paraTodo(conjunto, x => x > 0))

      // Si que todos son menores de 15
      assert(paraTodo(conjunto, x => x < 15))
   }

   /**
     * Test para existe
     */
   test("Test para existe") {
      val conjunto = Conjunto((x: Int) => x < 10)

      // No existe en el conjunto ningun elemento mayor de 10
      assert(!existe(conjunto, x => x > 10))

      // Si existe en el conjunto algun elemento menor de 15
      assert(existe(conjunto, x => x < 15))
   }

   /**
     * Test de map
     */
   test("Test de map") {
      // Definicion del conjunto
      val conjunto = Conjunto((x: Int) => x < 10)

      // Mapeo: sumar 25 a todos los elementos del conjunto
      val resultado = map(conjunto, (x => x + 25))

      // 30 y 31 pertenecen al conjunto resultado, ya que
      // 5 y 6 pertenecen al conjunto de partida
      assert(resultado(30))
      assert(resultado(31))

      // 125 no pertenece, porque 100 no esa en el conjunto
      // de partida
      assert(!resultado(125))
   }

}

