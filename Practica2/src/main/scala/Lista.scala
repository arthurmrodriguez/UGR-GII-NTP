/**
  * Interfaz generica para la lista
  * sealed indica una clase sellada
  * trait indica que es una interfaz
  * @tparam A
  */
sealed trait Lista[+A]

/**
  * Objeto para definir y
  * representar la lista vacia
  */
case object Nil extends Lista[Nothing]

/**
  * Clase para definir la lista como compuesta por elemento inicial
  * (cabeza) y resto (cola)
  * @param cabeza elemento inicial de la lista
  * @param cola resto de la lista
  * @tparam A tipo de dato de la lista
  */
case class Cons[+A](cabeza : A, cola : Lista[A]) extends Lista[A]

/**
  * Objeto para desarrollar las funciones pedidas
  */
object Lista {

   /**
     * Metodo para permitir crear listas sin usar new
     * @param elementos secuencia de elementos a incluir en la lista
     * @tparam A tipo de dato asociado
     * @return Lista creada con elementos
     */
   def apply[A](elementos : A*) : Lista[A] = {

     //Si la lista está vacía, devolvemos Nil
     //en caso contrario construimos el objeto Lista
     //con el elemento cabeza y el resto de los elementos
     if(elementos.isEmpty) Nil
     else Cons(elementos.head,apply(elementos.tail : _*))

   }

   /**
     * Obtiene la longitud de una lista en funcion
     * de cual sea el elemento lista
     * @param lista elementos de la lista
     * @tparam A tipo de dato asociado
     * @return longitud de la lista
     */
   def longitud[A](lista : Lista[A]) : Int = {

     lista match {
       case Nil => 0
       case Cons(cabeza,cola) => 1 + longitud(cola)
     }

   }

   /**
     * Metodo para sumar los valores de una lista de enteros
     * en funcion de si es Nil o no
     * @param enteros Lista de enteros a sumar
     * @return suma de los valores en enteros
     */
   def sumaEnteros(enteros : Lista[Int]) : Double = {

     enteros match {
       case Nil => 0.0
       case Cons(cabeza, cola) => cabeza.toDouble + sumaEnteros(cola)
     }

   }

   /**
     * Metodo para multiplicar los valores de una lista de enteros
     * @param enteros Lista de enteros a multiplicar
     * @return producto de los valores en enteros
     */
   def productoEnteros(enteros : Lista[Int]) : Double = {

     enteros match {
       case Nil => 0.0
       case Cons(cabeza, cola) => cabeza.toDouble * sumaEnteros(cola)
     }

   }

   /**
     * Metodo para agregar el contenido de dos listas
     * @param lista1 lista a la que concatenar lista2
     * @param lista2 lista que se va a concatenar
     * @tparam A tipo de dato asociado
     * @return listas concatenadas
     */
   def concatenar[A](lista1: Lista[A], lista2: Lista[A]): Lista[A] = ???

   /**
     * Funcion de utilidad para aplicar una funcion de forma sucesiva a los
     * elementos de la lista
     * @param lista
     * @param neutro
     * @param funcion
     * @tparam A
     * @tparam B
     * @return
     */
   def foldRight[A, B](lista : Lista[A], neutro : B)
                      (funcion : (A, B) => B): B = ???

   /**
     * Suma mediante foldRight
     * @param listaEnteros
     * @return
     */
   def sumaFoldRight(listaEnteros : Lista[Int]) : Double = ???

   /**
     * Producto mediante foldRight
     * @param listaEnteros
     * @return
     */
   def productoFoldRight(listaEnteros : Lista[Int]) : Double = ???

   /**
     * Reemplaza la cabeza por nuevo valor. Se asume que si la lista esta vacia
     * se devuelve una lista con el nuevo elemento
     *
     * @param lista
     * @param cabezaNueva
     * @tparam A
     * @return
     */
   def asignarCabeza[A](lista : Lista[A], cabezaNueva : A) : Lista[A] = ???

   /**
     * Elimina el elemento cabeza de la lista
     * @param lista
     * @tparam A
     * @return
     */
   def tail[A](lista : Lista[A]): Lista[A] = ???

   /**
     * Elimina los n primeros elementos de una lista
     * @param lista lista con la que trabajar
     * @param n numero de elementos a eliminar
     * @tparam A tipo de datos
     * @return
     */
   def eliminar[A](lista : Lista[A], n: Int) : Lista[A] = ???

   /**
     * Elimina elementos mientra se cumple la condicion pasada como
     * argumento
     * @param lista lista con la que trabajar
     * @param criterio predicado a considerar para continuar con el borrado
     * @tparam A tipo de datos a usar
     * @return
     */
   def eliminarMientras[A](lista : Lista[A], criterio: A => Boolean) : Lista[A] = ???

   /**
     * Elimina el ultimo elemento de la lista. Aqui no se pueden compartir
     * datos en los objetos y hay que generar una nueva lista copiando
     * datos
     * @param lista lista con la que trabajar
     * @tparam A tipo de datos de la lista
     * @return
     */
   def eliminarUltimo[A](lista : Lista[A]) : Lista[A] = ???

   /**
     * foldLeft con recursividad tipo tail
     * @param lista lista con la que trabajar
     * @param neutro elemento neutro
     * @param funcion funcion a aplicar
     * @tparam A parametros de tipo de elementos de la lista
     * @tparam B parametro de tipo del elemento neutro
     * @return
     */
   //@annotation.tailrec
   def foldLeft[A, B](lista : Lista[A], neutro: B)(funcion : (B, A) => B): B = ???
}
