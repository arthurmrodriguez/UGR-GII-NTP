/**
  * Clase genérica Nodo para almacenar los nodos
  * de un arbol
  */
abstract class Nodo {

  /**
    * Metodo para obtener los valores del Potencial
    * @return valores del Potencial
    */
  def obtenerValores : List[Double]

  /**
    * Metodo para obtener el hijo de la lista
    * de nodos del nodo actual
    * @param indice del nodo hijo
    * @return Nodo hijo
    */
  def obtenerHijo(indice : Int) : Nodo

  /**
    * Metodo para obtener el valor del Potencial
    * segun una asignacion
    * @param asignacion a buscar
    * @param inicio el nodo raiz
    * @return Valor correspondiente a la asignacion
    */
  def obtenerValor(asignacion : Asignacion, inicio : Int = 0) : Double

  /**
    * Metodo para restringir un nodo en funcion de una
    * variable y un estado determinado
    * @param variable a restringir
    * @param estado de la variable
    * @return Nodo restringido
    */
  def restringir(variable : Variable, estado : Int) : Nodo

  /**
    * Metodo para combinar dos nodos segun
    * diversas condiciones
    * @param raiz del nodo como argumento
    * @return Nodo combinado
    */
  def combinar(raiz : Nodo) : Nodo

  /**
    * Metodo para imprimir por pantalla
    * @param nivel del arboo
    * @return Arbol con una estructura
    */
  def toString(nivel : Int): String

}



case class NodoHoja(valor : Double) extends Nodo{

  /**
    * Metodo para obtener valores del Potencial
    * @return valores del Potencial
    */
  override def obtenerValores: List[Double] = List(valor)

  /**
    * Metodo para obtener el hijo de la lista
    * de nodos del nodo actual
    * @param indice del nodo hijo
    * @return Nodo hijo
    */
  override def obtenerHijo(indice : Int) : Nodo = this

  /**
    * Metodo para obtener el valor del Potencial
    * segun una asignacion
    * @param asignacion a buscar
    * @param inicio el nodo raiz
    * @return Valor correspondiente a la asignacion
    */
  override def obtenerValor(asignacion: Asignacion, inicio : Int = 0): Double = valor

  /**
    * Metodo para restringir un nodo en funcion de una
    * variable y un estado determinado
    * @param variable a restringir
    * @param estado de la variable
    * @return Nodo restringido
    */
  override def restringir(variable: Variable, estado: Int): Nodo =
    this

  /**
    * Metodo para combinar dos nodos segun
    * diversas condiciones
    * @param raiz del nodo como argumento
    * @return Nodo combinado
    */
  override def combinar(raiz: Nodo): Nodo = {

    raiz match {
      case otro : NodoHoja =>
        NodoHoja(this.valor * otro.valor)

      case otro : NodoVariable =>
        otro.combinar(this)
    }
  }

  /**
    * Metodo para imprimir por pantalla
    * @param nivel del arboo
    * @return Arbol con una estructura
    */
  override def toString(nivel: Int): String = ("  "*(nivel+1)) + " = " + valor

}

case class NodoVariable(variable : Variable, listaHijos : List[Nodo]) extends Nodo{

  /**
    * Metodo para obtener valores del Potencial
    * @return valores del Potencial
    */
  override def obtenerValores: List[Double] =
    listaHijos.map(hijo => hijo.obtenerValores).reduce(_:::_)

  /**
    * Metodo para obtener el hijo de la lista
    * de nodos del nodo actual
    * @param indice del nodo hijo
    * @return Nodo hijo
    */
  override def obtenerHijo(indice : Int ): Nodo = listaHijos(indice)

  /**
    * Metodo para obtener el valor del Potencial
    * segun una asignacion
    * @param asignacion a buscar
    * @param inicio el nodo raiz
    * @return Valor correspondiente a la asignacion
    */
  override def obtenerValor(asignacion : Asignacion, inicio : Int = 0) : Double =
    listaHijos(asignacion.valores(inicio)).obtenerValor(asignacion,inicio+1)

  /**
    * Metodo para restringir un nodo en funcion de una
    * variable y un estado determinado
    * @param variable a restringir
    * @param estado de la variable
    * @return Nodo restringido
    */
  override def restringir(variable: Variable, estado: Int): Nodo = {

    if(this.variable == variable)
      obtenerHijo(estado)

    else{
      val hijosAux = listaHijos.map(nodoHijo => nodoHijo.restringir(variable,estado))
      NodoVariable(this.variable,hijosAux)
    }

  }

  /**
    * Metodo para combinar dos nodos segun
    * diversas condiciones
    * @param raiz del nodo como argumento
    * @return Nodo combinado
    */
  override def combinar(raiz: Nodo): Nodo = {

    raiz match{
      case otro : NodoHoja =>
        var hijosAux = this.listaHijos.map(hijo => hijo.combinar(otro))
        NodoVariable(variable,hijosAux)

      case otro : NodoVariable =>

        var listaNodos = listaHijos.indices.map(estado =>
          this.obtenerHijo(estado).combinar(otro.restringir(variable, estado))).toList
        NodoVariable(variable,listaNodos)
    }

  }

  /**
    * Metodo para imprimir por pantalla
    * @param nivel del arboo
    * @return Arbol con una estructura
    */
  override def toString(nivel: Int): String = {

    (0 until variable.numEstados).map(estado =>

      ("  "*(nivel)) + variable.nombre + " : "+ estado +
        "\n"+ obtenerHijo(estado).toString(nivel+1)) mkString "\n"

  }


}
