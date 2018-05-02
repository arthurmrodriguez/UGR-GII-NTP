import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, throws, AnyOperators}
import org.scalacheck.Gen._

object ListaTest extends Properties("ListaTest"){

  // Metodo de generacion de listas de valores enteros
  val secuenciaEnteros = listOf(choose(0,10))
  def mayor2(x:Int):Boolean = x>2
  def suma(x:Int, y:Int) = x+y
  def producto(x:Int, y:Int) = x*y

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


  property("Sumar fold right") =
    forAll(secuenciaEnteros) {
      xs => {
        // objeto a partir de la clase Lista
        val lista:Lista[Int] = Lista(xs :_*)
        val sumaFRList = xs.foldRight(0)(suma)
        val sumaFRLista = Lista.sumaFoldRight(lista)

        // se comprueba la igualdad
        sumaFRList.toDouble ?= sumaFRLista
      }
    }

  property("Producto fold right") =
    forAll(secuenciaEnteros) {
      xs => {
        // objeto a partir de la clase Lista
        val lista:Lista[Int] = Lista(xs :_*)
        val prodFRList = xs.foldRight(1)(producto)
        val prodFRLista = Lista.productoFoldRight(lista)

        // se comprueba la igualdad
        prodFRList.toDouble ?= prodFRList
      }
    }

  property("Asignar cabeza") =
    forAll(secuenciaEnteros) {
      xs => {
        // objeto a partir de la clase Lista
        val lista:Lista[Int] = Lista(xs :_*)
        val asignarCabezaList = xs match {
          case List() => List(6)
          case a::c => 6::c
        }
        val asignarCabezaLista = Lista.asignarCabeza(lista,6)

        // se comprueba la igualdad
        asignarCabezaList ?= Lista.toList(asignarCabezaLista)
      }
    }

  property("Tail clase Lista") =
    forAll(secuenciaEnteros) {
      xs => {
        // objeto a partir de la clase Lista
        val lista:Lista[Int] = Lista(xs :_*)
        val tailList = xs match {
          case List() => xs
          case a::c => xs.tail
        }
        val tailLista = Lista.tail(lista)

        // se comprueba la igualdad
        tailList ?= Lista.toList(tailLista)
      }
    }

  property("Eliminar n primeros elementos") =
    forAll(secuenciaEnteros) {
      xs => {
        // objeto a partir de la clase Lista
        val lista:Lista[Int] = Lista(xs :_*)
        val eliminarList = xs match {
          case List() => xs
          case a::c => xs.drop(2)
        }
        val eliminarLista = Lista.eliminar(lista,2)

        // se comprueba la igualdad
        eliminarList ?= Lista.toList(eliminarLista)
      }
    }

  property("Eliminar mientras") =
    forAll(secuenciaEnteros) {
      xs => {
        // objeto a partir de la clase Lista
        val lista:Lista[Int] = Lista(xs :_*)
        val eliminarMientrasList = xs match {
          case List() => xs
          case a::c => xs.dropWhile(mayor2)
        }
        val eliminarMientrasLista = Lista.eliminarMientras(lista,mayor2)

        // se comprueba la igualdad
        eliminarMientrasList ?= Lista.toList(eliminarMientrasLista)
      }
    }

  property("Eliminar ultimo de la lista") =
    forAll(secuenciaEnteros) {
      xs => {
        // objeto a partir de la clase Lista
        val lista:Lista[Int] = Lista(xs :_*)
        val eliminarUltimoList = xs match {
          case List() => xs
          case a::c => xs.dropRight(1)
        }
        val eliminarUltimoLista = Lista.eliminarUltimo(lista)

        // se comprueba la igualdad
        eliminarUltimoList ?= Lista.toList(eliminarUltimoLista)
      }
    }

  property("Sumar fold left") =
    forAll(secuenciaEnteros) {
      xs => {
        // objeto a partir de la clase Lista
        val lista:Lista[Int] = Lista(xs :_*)
        val sumarFoldLeftList = xs.map(x=>x.toDouble).sum
        val sumarFoldLeftLista = Lista.foldLeft(lista,0)(suma)

        // se comprueba la igualdad
        sumarFoldLeftList.toDouble ?= sumarFoldLeftLista
      }
    }







}
