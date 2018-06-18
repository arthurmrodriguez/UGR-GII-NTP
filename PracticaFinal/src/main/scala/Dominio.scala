/**
  * Clase Dominio
  * @param variables
  */
class Dominio(val variables : List[Variable]) {

  /**
    * Dato miembro para guardar el indice correspondiente
    * a cada variable en la lista
    */
  val indiceVariables : Map[Variable,Int] = (variables zipWithIndex).toMap

  /**
    * Dato miembro para asociar a cada variable
    * su peso correspondiente
    */
  val pesosVariables : Map[Variable, Int] = (variables zip calcularPesosVariables).toMap

  /**
    * Metodo para comprobar si el dominio es vacio
    * @return si el dominio es vacio
    */
  def vacio : Boolean = variables.isEmpty

  /**
    * Metodo para obtener la cantidad de variables del dominio
    * @return Numero de variables del dominio
    */
  def longitud : Int = variables.length

  /**
    * Metodo para obtener la lista de pesos de las variables
    * del dominio
    * @return lista de pesos de las variables del dominio
    */
  def pesos : List[Int] = pesosVariables.values.toList

  /**
    * Metodo para obtener el maximo indice del dominio
    * @return Entero que indica el maximo indice del dominio
    */
  def maximoIndice : Int = pesosVariables(variables(0))*variables(0).numEstados

  /**
    * Metodo para obtener la variable que ocupa una posicion
    * en el dominio
    * @param posicion de la variable a buscar
    * @return Variable que ocupa la posicion si existe, -1
    *         en caso contrario
    */
  def apply(posicion : Int) : Variable =
    if (posicion < longitud) variables(posicion) else null

  /**
    * Metodo toString para obtener la salida del Dominio
    * @return Salida del Dominio
    */
  override def toString: String = {

    variables.map(variable =>
      variable.nombre+"(s: " + variable.numEstados +
        " w: " + pesosVariables(variable)+ ")") mkString " "

  }

  /**
    * Metodo para añadir una Variable al Dominio
    * Devuelve un Dominio nuevo
    * @param variable nueva variable
    * @return dominio con la nueva variable
    */
  def +(variable : Variable) : Dominio = {
    if(variables.contains(variable))
      this
    else
      new Dominio(variables :+ variable)
  }

  /**
    * Metodo para sumar dos dominios teniendo en cuenta
    * las variables iguales
    * @param dominio dominio a unir
    * @return nuevo Dominio
    */
  def +(dominio : Dominio) : Dominio =
    new Dominio((variables ++ dominio.variables).distinct)

  /**
    * Metodo para eliminar una Variable del Dominio
    * Devuelve un Dominio nuevo
    * @param variable variable a eliminar
    * @return dominio con la variable eliminada
    */
  def -(variable : Variable) : Dominio = {
    if(!variables.contains(variable))
      this
    else
      new Dominio(variables.filter(mVar => mVar != variable))
  }

  /**
    * Metodo para eliminar las variables de un dominio
    * en funcion de las variables de otro dominio
    * @param dominio dominio cuyas variables hay que eliminar
    *                del dominio THIS
    * @return nuevo Dominio
    */
  def -(dominio : Dominio) : Dominio =
    new Dominio(variables.diff(dominio.variables))


  /**
    * Metodo para calcular los pesos de las variables
    * Hace uso de una funcion auxiliar para calcularlos
    * de forma recursiva
    * @return Lista de pesos de variables
    */
  private def calcularPesosVariables : List[Int] = {

    var pesos = List[Int]()

    def calcularPeso(indiceVariable : Int) : Int = {

      if(indiceVariable == variables.length-1){
        pesos = 1 :: pesos
        1
      }
      else{
        val pesoActual = variables(indiceVariable+1).numEstados * calcularPeso(indiceVariable+1)
          pesos = pesoActual :: pesos
        pesoActual
      }

    }
    if(!vacio)
      calcularPeso(0)
    pesos
  }

}

object Dominio {
  def apply(variables : List[Variable]) : Dominio = new Dominio(variables)
}
