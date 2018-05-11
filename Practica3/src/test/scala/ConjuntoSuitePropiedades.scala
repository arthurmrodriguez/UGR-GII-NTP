
import ConjuntoSuitePropiedades.valor
import org.scalacheck.Properties
import org.scalacheck.Prop.{AnyOperators, forAll, throws,all}
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

   property("Conjunto de Tamaño Uno") =
     forAll(valor) {
        valor => {
           // Se crea el conjunto de un elemento
           val conjunto = Conjunto.conjuntoUnElemento(valor)

           // Se comprueba que el conjunto contiene el valor
           conjunto(valor) == true
        }
     }

   /**
     * Propiedad para probar el metodo union de Conjunto
     */
   property("Conjunto Union") =
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

   /**
     * Propiedad para probar el metodo interseccion de Conjunto
     */
   property("Conjunto Interseccion") =
     forAll(valor) {
        valor => {
           val secuencia1 = secuencia(10)
           val secuencia2 = secuencia(10)

           // Se generan los conjuntos
           val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)
           val conjunto2 = Conjunto(x => x >= secuencia2.min && x <= secuencia2.max)

           // Se produce la interseccion
           val interseccion = Conjunto.interseccion(conjunto1, conjunto2)

           // Se itera sobre la interseccion de ambos rangos y se comprueba la
           // pertenencia al conjunto
           val rangoInterseccion = secuencia1.toList.intersect(secuencia2.toList)

           // De cumplirse que cada elemento esta en el conjunto union
           val resultado = rangoInterseccion.map(valor => {
              interseccion(valor) == true
           })

           val global: Boolean = resultado.forall(res => res == true)
           global == true
        }
     }

   /**
     * Propiedad para probar el metodo diferencia de Conjunto
     */
   property("Conjunto Diferencia") =
     forAll(valor) {
        valor => {
           val secuencia1 = secuencia(10)
           val secuencia2 = secuencia(10)

           // Se generan los conjuntos
           val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)
           val conjunto2 = Conjunto(x => x >= secuencia2.min && x <= secuencia2.max)

           // Se produce la diferencia
           val diferencia = Conjunto.diferencia(conjunto1, conjunto2)

           // Se itera sobre la interseccion de ambos rangos y se comprueba la
           // pertenencia al conjunto
           val rangoDiferencia = secuencia1.toList.filterNot(secuencia2.toList.contains(_))

           // De cumplirse que cada elemento esta en el conjunto union
           val resultado = rangoDiferencia.map(valor => {
              diferencia(valor) == true
           })

           val global: Boolean = resultado.forall(res => res == true)
           global == true
        }
     }

   /**
     * Propiedad para probar el metodo filter de Conjunto
     */
   property("Conjunto Filter") =
     forAll(valor) {
        valor => {
           val secuencia1 = secuencia(10)
           val secuencia2 = secuencia(10)

           // Se generan los conjuntos
           val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)
           val conjunto2 = Conjunto(x => x >= secuencia2.min && x <= secuencia2.max)

           // Se produce el filtrado
           val filtrado = Conjunto.filtrar(conjunto1, conjunto2.funcionCaracteristica)

           // Se itera sobre la interseccion de ambos rangos y se comprueba la
           // pertenencia al conjunto
           val rangoFiltrado = secuencia1.toList.intersect(secuencia2.toList)

           // De cumplirse que cada elemento esta en el conjunto union
           val resultado = rangoFiltrado.map(valor => {
              filtrado(valor) == true
           })

           val global: Boolean = resultado.forall(res => res == true)
           global == true
        }
     }

   /**
     * Propiedad para probar el metodo paraTodo de Conjunto
     */
   property("Conjunto ForAll") =
     forAll(valor) {
        valor => {

           // Se generan los conjuntos
           val conjunto1 = Conjunto(x => x <= 15)

           val cond1 = Conjunto.paraTodo(conjunto1,x=> x <= 15)
           val cond2 = !Conjunto.paraTodo(conjunto1,x => x%2 == 0)

           all(cond1,cond2)
        }
     }

   /**
     * Propiedad para probar el metodo existe de Conjunto
     */
   property("Conjunto Existe") =
     forAll(valor) {
        valor => {

           // Se generan los conjuntos
           val conjunto1 = Conjunto(x => x <= 15)

           val cond1 = Conjunto.existe(conjunto1,x=> x == 10)
           val cond2 = Conjunto.existe(conjunto1,x => x > 60)

           all(cond1,!cond2)
        }
     }

   /**
     * Propiedad para probar el metodo map de Conjunto
     */
   property("Conjunto Map") =
     forAll(valor) {
        valor => {

           // Se generan los conjuntos
           val conjunto1 = Conjunto(x => x < 10)
           val conjuntoMap = Conjunto.map(conjunto1, (x => x*2))

           val cond1 = !conjuntoMap(40)
           val cond2 = conjuntoMap(16)
           all(cond1 && cond2)
        }
     }




}