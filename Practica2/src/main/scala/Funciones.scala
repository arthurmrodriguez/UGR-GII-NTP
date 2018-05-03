
/**
  * Objeto singleton para probar la funcionalidad del triangulo
  * de Pascal
  */
object Funciones {
  /**
    * Metodo main: en realidad no es necesario porque el desarrollo
    * deberia guiarse por los tests de prueba
    *
    * @param args
    */
  def main(args: Array[String]) {
    println("................... Triangulo de Pascal ...................")

    // Se muestran 10 filas del trinagulo de Pascal
    for (row <- 0 to 10) {
      // Se muestran 10 10 columnas
      for (col <- 0 to row)
        print(calcularValorTrianguloPascal(row, col) + " ")

      // Salto de linea final para mejorar la presentacion
      println()
    }

    println()
    //Comprobacion del primer ejercicio: triangulo Pascal
    println(calcularValorTrianguloPascal(15,10))
    println(calcularValorTrianguloPascal(0, 0))

    //Comprobacion del segundo ejercicio: balanceo de cadenas
    println(chequearBalance(List('(', '(','(',')',')',')'))) //True
    println(chequearBalance(List('(',')',')','('))) //False
    println(contarCambiosPosibles(4, List(1,2,3)))


  }

  /**
    * Ejercicio 1: funcion para generar el triangulo de Pascal
    *
    * @param fila Fila en el triangulo de Pascal
    * @param columna Columna en el triangulo de Pascal
    * @return Valor del triangulo de Pascal segun fila y columna
    */
  def calcularValorTrianguloPascal(fila: Int, columna: Int): Int = {

    if(fila == columna || columna == 0) 1
    else calcularValorTrianguloPascal(fila-1,columna-1) +
              calcularValorTrianguloPascal(fila-1,columna)
  }

  /**
    * Ejercicio 2: funcion para chequear el balance de parentesis
    * Se hace uso de una funcion auxiliar para permitir que sea tailrec.
    * Esta funcion aumenta un contador cuando se encuentra con un (
    * y disminuye cuando lo hace con un ), de forma que si al final
    * del conteo el acumulador != 0 o en algun momento
    * se ha encontrado un desbalance (acum < )0, la cadena no estará
    * balanceada.
    * @param cadena cadena a analizar
    * @return valor booleano con el resultado de la operacion
    */
  def chequearBalance(cadena: List[Char]): Boolean = {

    @annotation.tailrec
    def go(cadena: List[Char], acum: Int = 0): Boolean = {

      if(cadena.isEmpty) acum == 0
      else if(acum < 0) false
      else if(cadena.head == '(') go(cadena.tail,acum+1)
      else if(cadena.head == ')') go(cadena.tail,acum-1)
      else go(cadena.tail,acum)

    }

    go(cadena,0)
  }

  /**
    * Ejercicio 3: funcion para determinar las posibles formas de devolver el
    * cambio de una determinada cantidad con un conjunto de monedas establecido
    *
    * @param cantidad Cantidad a devolver
    * @param monedas valores de monedas posibles
    * @return contador de numero de vueltas posibles
    */
  def contarCambiosPosibles(cantidad: Int, monedas: List[Int]): Int = {

    def cantidadCambios(cantidad:Int, monedas:List[Int]): Int = {

      if(cantidad == 0) 1
      else if(monedas.isEmpty) 0
      else if(cantidad < monedas.head) 0
      else{

        //Recorremos todas las posibiles combinaciones y en cada
        //nivel lo que hacemos es quitar uno de los tipos de monedas
        var acum = 0
        for(i <- 0 to cantidad by monedas.head)
          acum+=cantidadCambios(cantidad-i,monedas.tail)
        acum

      }

    }

    val monedasOrdenadas = monedas.sorted
    if(cantidad > 0) cantidadCambios(cantidad, monedasOrdenadas)
    else 0

  }

  /**
   * Metodo generico para busqueda binaria
   * @param coleccion conjunto de datos sobre los que buscar
   * @param aBuscar elemento a buscar
   * @param criterio para comparar dos elementos de tipo A
   * @tparam A parametro de tipo
   * @return posicion del valor buscado o -1 en caso de no hallarlo
   */
  def busquedaBinaria[A](coleccion : Array[A], aBuscar: A, 
                       criterio : (A,A) => Boolean) : Int = {

    @annotation.tailrec
    def go(coleccion: Array[A], acum: Int = 0): Int = {

      val mitad = coleccion.length / 2
      val valorMitad = coleccion(mitad)

      if (valorMitad == aBuscar) acum + mitad
      else if (coleccion.length == 1) -1
      else {
        if (criterio(valorMitad, aBuscar)) {

          if (mitad + 1 == coleccion.length) -1
          else go(coleccion.slice(mitad + 1, coleccion.length), acum + mitad + 1)

        }
        else {
          if (mitad == 0) -1
          else go(coleccion.slice(0, mitad), acum)
        }
      }
    }

    if(coleccion.length == 0) -1
    else go(coleccion)
  }
}
