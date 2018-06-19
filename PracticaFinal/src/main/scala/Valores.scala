/**
  * Clase abstracta para definir Valores
  * @param dominio sobre el que se define
  */
abstract class Valores(val dominio : Dominio){

  /**
    *  Metodo abstracto para obtener el valor
    * de una asignacion
    * @param asignacion
    * @return valor de la asignacion
    */
  def obtenerValor(asignacion: Asignacion) : Double

  /**
    * Metodo para obtener todos los valores del potencial
    * @return Lista de valores
    */
  def obtenerValores : List[Double]

  /**
    * Metodo para obtener el conjunto de variables del potencial
    * @return Lista de variables del dominio
    */
  def obtenerVariables : List[Variable] = dominio.variables

  /**
    * Metodo para imprimir por pantalla
    * @return cadena que representa los Valores
    */
  override def toString: String

  /**
    * Metodo para combinar objetos de tipo Valores
    * independientemente de si son ValoresArray o
    * ValoresArbol
    * @param otro objeto de la clase Valores a combinar
    * @return nuevo objeto Valores combinado
    */
  def combinar(otro : Valores) : Valores = {

    this match {
      case potencial1 : ValoresArray => {

        val otroValor = otro match {
          case potencial2 : ValoresArbol => potencial2.convertir
          case potencial2 : ValoresArray => potencial2
        }

        potencial1.combinarArrayArray(otroValor)

      }
      case potencial1 : ValoresArbol =>{

        val otroValor = otro match {
          case potencial2 : ValoresArray => potencial2.convertir
          case potencial2 : ValoresArbol => potencial2
        }

        potencial1.combinarArbolArbol(otroValor)
      }

    }

  }

  def restringir(variable : Variable, valor : Double) : List[Double] = ???

  def convertir : Valores = ???


}

/**
  * Clase ValoresArray. Hereda de valores
  * @param dominio sobre el que se define
  * @param datos valores asociados al Potencial
  */
case class ValoresArray(override val dominio: Dominio, datos : List[Double]) extends Valores(dominio){

  /**
    * Implementacion del metodo abstracto obtener valor
    * @param asignacion sobre la que se quiere obtener el valor
    * @return valor de la asignacion
    */
  override def obtenerValor(asignacion: Asignacion): Double = {

    if(asignacion.dominio.variables == dominio.variables)
      datos(asignacion.calcularIndice)

    else
      -1.0
  }


  /**
    * Implementacion del metodo para obtener la lista de valores
    * @return Lista de valores
    */
  override def obtenerValores: List[Double] = datos


  /**
    * Implementacion del metodo para imprimir por pantalla
    * @return cadena que representa los Valores
    */
  override def toString: String = {

    (0 until dominio.maximoIndice).map(indice => {
      Asignacion(dominio, indice).toString + " = " +datos(indice)
    }) mkString "\n"


  }

  /**
    * Metodo para combinar dos objetos
    * de la clase ValoresArray
    * @param otro objeto ValoresArray
    * @return nuevo ValoresArray combinado
    */
  def combinarArrayArray(otro : ValoresArray) : ValoresArray = {

    // Generamos el nuevo dominio sumando los dominios
    // de this y de otro
    val dominioFinal = dominio + otro.dominio

    // Recorremos todos los indices validos del nuevo dominio
    // realizamos distintas operaciones para combinar los ValoresArray
    val listaValores = (0 until dominioFinal.maximoIndice).map(indice => {

      // Creamos la nueva asignacion sobre el dominio
      val asignacionFinal = Asignacion(dominioFinal,indice)

      // Proyectar asignacionFinal sobre this.dominio
      val asignacionThis = asignacionFinal.proyectar(dominio)

      // Proyectar asignacionFinal sobre otro.dominio
      val asignacionOtro = asignacionFinal.proyectar(otro.dominio)

      // Obtenemos el valor como el producto de los dos valores
      obtenerValor(asignacionThis)*otro.obtenerValor(asignacionOtro)
    }).toList

    ValoresArray(dominioFinal,listaValores)

  }


  override def restringir(variable: Variable, valor: Double): List[Double] = 

  override def convertir: ValoresArbol = ???

}


/**
  * Clase ValoresArbol para la gestion de Potenciales
  * @param dominio sobre el que se define
  */
case class ValoresArbol(override val dominio : Dominio) extends Valores(dominio) {


  override def obtenerValor(asignacion: Asignacion): Double = ???

  override def obtenerValores: List[Double] = ???

  override def toString: String = ???

  override def convertir: ValoresArray = ???

  def combinarArbolArbol(otro : ValoresArbol) : ValoresArbol = ???



}
