/**
  * Clase abstracta para definir Valores
  * @param dominio sobre el que se define
  */
abstract class Valores(dominio : Dominio){

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
    null
  }


}

/**
  * Clase ValoresArray. Hereda de valores
  * @param dominio sobre el que se define
  * @param datos valores asociados al Potencial
  */
class ValoresArray(val dominio: Dominio, val datos : List[Double]) extends Valores(dominio){

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

}

object ValoresArray{

  def apply(dominio: Dominio, datos : List[Double]): ValoresArray = {
    new ValoresArray(dominio, datos)
  }

}
