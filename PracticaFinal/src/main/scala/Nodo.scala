/**
  * Clase genérica Nodo para almacenar los nodos
  * de un arbol
  */
abstract class Nodo {

  def obtenerValores : List[Double]

  def obtenerHijo(indice : Int) : Nodo

  def obtenerValor(asignacion : Asignacion, inicio : Int = 0) : Double

  def restringir(variable : Variable, estado : Int) : Nodo

  def toString(nivel : Int): String

}



case class NodoHoja(valor : Double) extends Nodo{

  override def obtenerValores: List[Double] = List(valor)

  override def obtenerHijo(indice : Int) : Nodo = this

  override def obtenerValor(asignacion: Asignacion, inicio : Int = 0): Double = valor

  override def restringir(variable: Variable, estado: Int): Nodo =
    this

  override def toString(nivel: Int): String = ("  "*(nivel+1)) + " = " + valor

}

case class NodoVariable(variable : Variable, listaHijos : List[Nodo]) extends Nodo{

  override def obtenerValores: List[Double] =
    listaHijos.map(hijo => hijo.obtenerValores).reduce(_:::_)

  override def obtenerHijo(indice : Int ): Nodo = listaHijos(indice)

  override def obtenerValor(asignacion : Asignacion, inicio : Int = 0) : Double =
    listaHijos(asignacion.valores(inicio)).obtenerValor(asignacion,inicio+1)

  override def restringir(variable: Variable, estado: Int): Nodo = {

    if(this.variable == variable)
      obtenerHijo(estado)

    else{
      val hijosAux = listaHijos.map(nodoHijo => nodoHijo.restringir(variable,estado))
      NodoVariable(this.variable,hijosAux)
    }


  }

  override def toString(nivel: Int): String = {

    (0 until variable.numEstados).map(estado =>

      ("  "*(nivel)) + variable.nombre + " : "+ estado +
        "\n"+ obtenerHijo(estado).toString(nivel+1)) mkString "\n"

  }


}
