/**
  * Clase Variable - Elemento basico del sistema
  * @param nombre de la variable
  * @param numEstados numero de estados de la variable
  */
class Variable(val nombre : String, val numEstados : Int){

  override def toString : String =
    "Nombre "+ nombre + " " + "Numero de estados " + numEstados
}

/**
  * Companion Object de la clase variable
  */
object Variable{
  def apply(nombre : String, numEstados : Int) : Variable =
    new Variable(nombre,numEstados)
}

