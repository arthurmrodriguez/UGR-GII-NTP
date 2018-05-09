
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, throws, AnyOperators}
import org.scalacheck.Gen._

object ConjuntoSuitePropiedades extends Properties("Test sobre conjunto") {
   val valor = choose(0, 10)

   /**
     * Generacion de secuencia de tamaño
     *
     * @param tam
     * @return
     */
   def secuencia(tam: Int): Range = {
      val inicio = valor.sample.getOrElse(0)
      inicio to (inicio + tam)
   }

   /**
     * Propiedad para probar el metodo de obtencion de la longitud
     */
   property("conjunto de tamaño uno") =
      forAll(valor) {
         valor => {
            // Se crea el conjunto de un elemento
            val conjunto = Conjunto.conjuntoUnElemento(valor)

            // Se comprueba que el conjunto contiene el valor
            conjunto(valor) == true
         }
      }

   property("conjunto union") =
      forAll(valor) {
         valor => {
            val secuencia1 = secuencia(10)
            val secuencia2 = secuencia(10)

            // Se generan los conjuntos a unir
            val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)
            val conjunto2 = Conjunto(x => x >= secuencia2.min && x <= secuencia2.max)

            // Se produce la union
            val union = Conjunto.union(conjunto1, conjunto2)

            // Se itera sobre la union de ambos rangos y se comprueba la
            // pertenencia al conjunto
            val rangoUnion = secuencia1.toList ::: secuencia2.toList

            // De cumplirse que cada elemento esta en el conjunto union
            val resultado = rangoUnion.map(valor => {
               union(valor) == true
            })

            val global: Boolean = resultado.forall(res => res == true)
            global == true
         }
      }
}