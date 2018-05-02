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
case class Constructor[+A](cabeza : A, cola : Lista[A]) extends Lista[A]

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
     else Constructor(elementos.head,apply(elementos.tail : _*))

   }

  def toList[A](lista : Lista[A]):List[A] = {
    lista match {
      case Nil => List()
      case Constructor(cabeza, cola) => cabeza::toList(cola)
    }
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
       case Constructor(cabeza,cola) => 1 + longitud(cola)
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
       case Constructor(cabeza, cola) => cabeza.toDouble + sumaEnteros(cola)
     }

   }

   /**
     * Metodo para multiplicar los valores de una lista de enteros
     * @param enteros Lista de enteros a multiplicar
     * @return producto de los valores en enteros
     */
   def productoEnteros(enteros : Lista[Int]) : Double = {

     enteros match {
       case Nil => 1.0
       case Constructor(cabeza, cola) => cabeza.toDouble * productoEnteros(cola)
     }

   }

   /**
     * Metodo para agregar el contenido de dos listas
     * @param lista1 lista a la que concatenar lista2
     * @param lista2 lista que se va a concatenar
     * @tparam A tipo de dato asociado
     * @return listas concatenadas
     */
   def concatenar[A](lista1: Lista[A], lista2: Lista[A]): Lista[A] = {

     lista1 match {
       case Nil => lista2
       case Constructor(cabeza,cola) => Constructor(cabeza, concatenar(cola,lista2))

     }

   }
   /**
     * Funcion de utilidad para aplicar una funcion de forma sucesiva a los
     * elementos de la lista
     * @param lista elementos a aplicar la funcion
     * @param neutro elemento neutro
     * @param funcion funcion a aplicar
     * @tparam A tipo de dato A
     * @tparam B tipo de dato B
     * @return aplicacion de una funcion a una lista
     */
   def foldRight[A, B](lista : Lista[A], neutro : B)
                      (funcion : (A, B) => B): B = {

     lista match {
       case Nil => neutro
       case Constructor(cabeza,cola) => foldRight(cola,funcion(cabeza,neutro))(funcion)
     }

   }

   /**
     * Suma mediante foldRight
     * @param listaEnteros enteros a aplicar fold right
     * @return elementos sumados
     */
   def sumaFoldRight(listaEnteros : Lista[Int]) : Double = {

     def suma(x:Int, y:Int) = x+y
     foldRight(listaEnteros,0)(suma)

   }

   /**
     * Producto mediante foldRight
     * @param listaEnteros
     * @return elementos multiplicados
     */
   def productoFoldRight(listaEnteros : Lista[Int]) : Double = {

     def producto(x:Int, y:Int) = x*y
     foldRight(listaEnteros,0)(producto)

   }

   /**
     * Reemplaza la cabeza por nuevo valor. Se asume que si la lista esta vacia
     * se devuelve una lista con el nuevo elemento
     *
     * @param lista a reemplazar la cabeza
     * @param cabezaNueva nuevo valor en cabeza
     * @tparam A tipo de dato asociado
     * @return Nueva lista con nueva cabeza
     */
   def asignarCabeza[A](lista : Lista[A], cabezaNueva : A) : Lista[A] = {

     lista match {
       case Nil => Lista(cabezaNueva)
       case Constructor(cabeza,cola) => Constructor(cabezaNueva,cola)
     }

   }

   /**
     * Elimina el elemento cabeza de la lista
     * @param lista a eliminar el elemento cola
     * @tparam A tipo de dato asociado
     * @return Lista sin el elemento de la cola
     */
   def tail[A](lista : Lista[A]): Lista[A] = {

     lista match {
       case Nil => lista
       case Constructor(cabeza,cola) => cola
     }

   }

   /**
     * Elimina los n primeros elementos de una lista
     * @param lista lista con la que trabajar
     * @param n numero de elementos a eliminar
     * @tparam A tipo de datos
     * @return lista nueva sin elementos
     */
   def eliminar[A](lista : Lista[A], n: Int) : Lista[A] = {

     if(n==0)
       lista
     else{
       lista match {
         case Nil => lista
         case Constructor(cabeza,cola) => eliminar(cola, n-1)
       }
     }

   }

   /**
     * Elimina elementos mientra se cumple la condicion pasada como
     * argumento
     * @param lista lista con la que trabajar
     * @param criterio predicado a considerar para continuar con el borrado
     * @tparam A tipo de datos a usar
     * @return
     */
   def eliminarMientras[A](lista : Lista[A], criterio: A => Boolean) : Lista[A] = {

     lista match {
       case Nil => lista
       case Constructor(cabeza,cola) => {
         if(criterio(cabeza))
           eliminarMientras(cola, criterio)
         else
           lista
       }
     }

   }

   /**
     * Elimina el ultimo elemento de la lista. Aqui no se pueden compartir
     * datos en los objetos y hay que generar una nueva lista copiando
     * datos
     * @param lista lista con la que trabajar
     * @tparam A tipo de datos de la lista
     * @return lista sin el ultimo elemento
     */
   def eliminarUltimo[A](lista : Lista[A]) : Lista[A] = {

     lista match{
       case Nil => lista
       case Constructor(cabeza,cola) => {
         if(longitud(cola) == 1)
           Nil
         else
           concatenar(Lista(cabeza),eliminarUltimo(cola))

       }
     }
   }

   /**
     * foldLeft con recursividad tipo tail
     * @param lista lista con la que trabajar
     * @param neutro elemento neutro
     * @param funcion funcion a aplicar
     * @tparam A parametros de tipo de elementos de la lista
     * @tparam B parametro de tipo del elemento neutro
     * @return
     */
   @annotation.tailrec
   def foldLeft[A, B](lista : Lista[A], neutro: B)(funcion : (B, A) => B): B = {

     lista match {
       case Nil => neutro
       case Constructor(cabeza, cola) => foldLeft(cola,funcion(cabeza,neutro))(funcion)
     }

   }
}
