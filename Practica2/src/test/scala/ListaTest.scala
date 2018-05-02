import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, throws, AnyOperators}
import org.scalacheck.Gen._

object ListaTest extends Properties("ListaTest"){

  // Metodo de generacion de listas de valores enteros
  val secuenciaEnteros = listOf(choose(0,10))

  // LAS SIGUIENTES PROPERTIES SON PARA COMPROBAR QUE LOS METODOS
  // CREADOS EN LA CLASE LISTA FUNCIONANBIEN

  property("Longitud de lista") =
    forAll(secuenciaEnteros) {
      xs => {
        //Creamos lista a partir de xs
        val lista : Lista[Int] = Lista(xs : _*)
        val longitudList = xs.length
        val longitudLista = Lista.longitud(lista)
        //Con ?= hacemos lo mismo que con == solo que muestra info de depuracion
        //en caso de que no se cumpla la igualdad
        longitudList ?= longitudLista
      }
    }

  property("Suma de enteros") =
    forAll(secuenciaEnteros) {
      xs => {
        // objeto a partir de la clase Lista
        val lista:Lista[Int] = Lista(xs :_*)
        val sumaList = xs.map(x => x.toDouble).sum
        val sumaLista = Lista.sumaEnteros(lista)

        // se comprueba la igualdad
        sumaList ?= sumaLista
      }
    }

  property("Producto de enteros") =
    forAll(secuenciaEnteros) {
      xs => {
        // objeto a partir de la clase Lista
        val lista:Lista[Int] = Lista(xs :_*)
        val productoList = xs.map(x => x.toDouble).product
        val productoLista = Lista.productoEnteros(lista)

        // se comprueba la igualdad
        productoList ?= productoLista
      }
    }

  property("Concatenar listas") =
    forAll(secuenciaEnteros) {
      xs => {
        // objeto a partir de la clase Lista
        val lista:Lista[Int] = Lista(xs :_*)
        val concatList = xs:::xs
        val concatLista = Lista.concatenar(lista,lista)

        // se comprueba la igualdad
        concatList ?= Lista.toList(concatLista)
      }
    }


}
