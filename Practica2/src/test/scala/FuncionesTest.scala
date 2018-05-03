import org.scalacheck.{Gen,Properties}
import org.scalacheck.Prop.{forAll, all, AnyOperators}
import org.scalacheck.Gen._

object FuncionesTest extends Properties("FuncionesTest"){

  val MAXIMO = 20

  //______________________________Triangulo de Pascal_________________________

  //Se generan los valores de fila y columna para los extremos
  val coordenadasExtremos = for {
    fila <- Gen.choose(0, MAXIMO)
    columna <- Gen.oneOf(0, fila)
  } yield (fila, columna)

  property("Triangulo de Pascal: Elementos en lados del triangulo valen 1") = {
    forAll(coordenadasExtremos) {
      (i) => {
        val resultado=Funciones.calcularValorTrianguloPascal(i._1, i._2)
        resultado == 1
      }
    }
  }


  //Los valores internos distintos de 1 estarán
  //a partir de la segunda fila y de la columna 1
  //hasta la fila-1
  val coordenadasInternas = for {
    fila <- Gen.choose(2, MAXIMO)
    columna <- Gen.oneOf(1, fila-1)
  } yield (fila, columna)

  property("Triangulo de Pascal: Los valores internos son la suma de los 2 superiores") = {
    forAll(coordenadasInternas) {
      (i) => {
        val resultado=Funciones.calcularValorTrianguloPascal(i._1, i._2)
        resultado == Funciones.calcularValorTrianguloPascal(i._1-1, i._2-1) +
          Funciones.calcularValorTrianguloPascal(i._1-1, i._2)
      }
    }

  }
  //______________________________Triangulo de Pascal_________________________

  //______________________________Balanceo de parentesis______________________
  // Generacion de cadenas de longitud n: forma de uso strGen(10) para cadenas
  // de 10 caracteres
  val strGen = (n: Int) =>
    Gen.listOfN(n, Gen.oneOf('(',')',Gen.alphaChar.sample.get)).map(_.mkString)

  //Funcion para comprobar que cada subcadena de la cadena generada
  //de forma aleatoria tomada desde el principio hasta su tamaño -1
  //la cantidad de parentesis ( es siempre mayor o igual a los parentesis )
  def comprobarCantidadParentesis(lista: List[Char]):Boolean = {

    for(i <- 1 to lista.length){
      val subCadena = lista.slice(0,i)
      if(subCadena.count("("==) < subCadena.count(")"==)) false
    }

    true
  }

  property("Balanceo de parentesis: balanceo correcto") = {
    forAll(strGen(10)) {
      (cadena) => {
        //La condicion que comprueba comprobarCantidadParentesis se tiene
        //que cumplir para que la cadena esté balanceada, además de cumplir
        //nuestra funcion chequearBalance. En caso de que ninguna de las dos se cumpla
        //al hacer el AND con la comprobacion de parentesis obtenemos valor false
        val comprobarParentesis = comprobarCantidadParentesis(cadena.toList)
        val chequearBalance = Funciones.chequearBalance(cadena.toList)
        (!comprobarParentesis == !chequearBalance) || comprobarParentesis
      }
    }

  }

  //______________________________Balanceo de parentesis______________________

  //_________________________________Cambios de moneda________________________
  property("Cambios de moneda: distintas cantidades con distintos tipos de moneda") = {

    val cambio1 = Funciones.contarCambiosPosibles(0, List(1,2,5,10)) == 0
    val cambio2 = Funciones.contarCambiosPosibles(4, List(1,2)) == 3
    val cambio3 = Funciones.contarCambiosPosibles(4, List(1,2,3)) == 4
    val cambio4 = Funciones.contarCambiosPosibles(15, List(20,50)) == 0
    val cambio5 = Funciones.contarCambiosPosibles(5, List()) == 0
    val cambio6 = Funciones.contarCambiosPosibles(15, List(1,3,5,10)) == 16
    val cambio7 = Funciones.contarCambiosPosibles(15, List(2,3,5,10)) == 9

    all(cambio1, cambio2, cambio3, cambio4,cambio5, cambio6,cambio7)

  }

  //_________________________________Cambios de moneda________________________

  //___________________________Busqueda binaria generica______________________

  val enteros = listOf(Gen.choose(-50, 50))

  property("Busqueda binaria generica") = {
    forAll(enteros) { (numeros) =>

      //Ordenamos en orden creciente y buscamos el indice
      //de un valor aleatorio del array de enteros
      val enterosOrdenados = numeros.sorted
      val valorABuscar = Gen.choose(-50, 50).sample.getOrElse(0)
      val indice = enterosOrdenados.indexOf(valorABuscar)

      //Comprobamos que el metodo de Funciones
      //devuelve el mismo indice que la busqueda
      //con indexOf o que en caso contrario, que apunten
      //al mismo valor (-1 para no encontrado)
      val indiceBusquedaBinaria = Funciones.busquedaBinaria[Int](enterosOrdenados.toArray, valorABuscar, _ < _)

      (indice == indiceBusquedaBinaria) || (enterosOrdenados(indice) == enterosOrdenados(indiceBusquedaBinaria))

    }
  }


}
