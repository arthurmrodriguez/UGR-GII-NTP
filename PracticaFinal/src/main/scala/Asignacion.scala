/**
  * Clase Asignacion, compuesta por un dominio
  * y una lista de valores para las variables
  * @param dominio del Potencial
  * @param valores del Potencial
  */
class Asignacion(val dominio : Dominio, val valores : List[Int]) {

  val datos :Map[Variable,Int] =  asignarValoresADominio

  /**
    * Metodo para comprobar si se define una
    * Asignacion sobre un dominio vacio
    * @return Asignacion vacia
    */
  def vacia : Boolean = dominio.vacio

  /**
    * Metodo para obtener el numero de variables
    * que define la asignacion
    * @return numero de variables del dominio
    */
  def obtenerNumeroVariables : Int = dominio.longitud


  /**
    * Metodo para obtener el valor de una variable
    * en la asignacion.
    * @param variable para obtener su valor
    * @return valor de la variable, -1 en caso de que
    *         esa variable no este en esa asignacion
    */
  def obtenerValorVariable(variable : Variable) : Int = datos.getOrElse(variable, -1)


  /**
    * Metodo para añadir un par variable-valor a la
    * asignacion
    * @param variable nueva
    * @param valor de la variable
    * @return Asignacion nueva
    */
  def +(variable : Variable, valor : Int) : Asignacion = {
    if (dominio.variables.contains(variable))
      this
    else
      Asignacion(dominio + variable, valores :+ valor)
  }

  /**
    * Metodo para calcular el indice de la asignacion
    * @return indice de la Asignacion
    */
  def calcularIndice : Int = {
    (dominio.pesos zip datos.values).map{case (peso,valor) => peso*valor}.sum
  }

  /**
    * Metodo que devuelve una Asignacion con las variables
    * del dominio que se encuntran en SU dominio
    * @param unDominio que contiene las variables interseccion
    * @return nueva Asignacion
    */
  def proyectar(unDominio : Dominio) : Asignacion = {

    val mDominio = Dominio(unDominio.variables.intersect(dominio.variables))
    val mValores = mDominio.variables.map(variable => obtenerValorVariable(variable))

    Asignacion(mDominio,mValores)

  }

  /**
    * Metodo para mostrar por pantalla una asignacion
    * @return Asignacion con formato adecuado
    */
  override def toString: String = {
    if(datos != null)
      datos.map{case (variable,valor) => "["+variable.nombre + " - "+valor +"]"} mkString " "
    else
      null
  }

  /**
    * Metodo privado para asignar valores a un Dominio
    * @return Valores asignados a cada variable
    */
  private def asignarValoresADominio : Map[Variable,Int] = {

    if(valores.length == dominio.longitud){

      dominio.variables.zip(valores).map{
        case (variable,valor) =>

          if(valor < 0 || valor > variable.numEstados)
            return null
          else
            variable->valor

      }.toMap

    }

    else
      null

  }

}

object Asignacion{
  /**
    * Metodo para crear una Asignacion a partir de un
    * dominio y un conjunto de valores
    * @param dominio correspondiente
    * @param valores correspondientes
    * @return nueva Asignacion
    */
  def apply(dominio : Dominio, valores : List[Int]) : Asignacion = {
    new Asignacion(dominio,valores)
  }

  /**
    * Metodo para crear una Asignacion unicamente
    * a partir de un dominio, con valores 0 para cada
    * una de las variables
    * @param dominio correspondientes
    * @return nueva Asignacion
    */
  def apply(dominio : Dominio) : Asignacion= {
    new Asignacion(dominio, List.fill(dominio.longitud)(0))
  }

  /**
    * Metodo para crear una Asignacion a partir de un
    * dominio y un indice
    * @param dominio correspondiente
    * @param indice correspondiente
    * @return nueva Asignacion
    */
  def apply(dominio : Dominio, indice : Int) : Asignacion = {

    if(indice >= dominio.maximoIndice)
      Asignacion(dominio)

    else{
      new Asignacion(dominio, dominio.variables.map(variable =>
        (indice/dominio.pesosVariables(variable)) % variable.numEstados
      ))
    }
  }
}
