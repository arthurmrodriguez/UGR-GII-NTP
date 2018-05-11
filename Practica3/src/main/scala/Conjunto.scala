
/**
  * Clase para representar conjuntos definidos mediante una funcion
  * caracteristica (un predicado). De esta forma, se declara el tipo
  * conjunto como un predicado que recibe un entero (elemento) como
  * argumento y dvuelve un valor booleano que indica si pertenece o no
  * al conjunto
  *
  * @param funcionCaracteristica
  */
class Conjunto(val funcionCaracteristica: Int => Boolean) {
/**
    * Crea una cadena con el contenido completo del conjunto
    *
    * @return
    */
  override def toString(): String = {
    // El uso de this(i) implica la el uso del metodo apply
    val elementos = for (i <- -Conjunto.LIMITE to Conjunto.LIMITE 
                         if this(i)) yield i
    elementos.mkString("{", ",", "}")
  }
  
  /**
    * Metodo para determinar la pertenencia
    * de un elemento al conjunto
    * @param elemento
    * @return valor booleano indicando si elemento cumple
    *         la funcion caracteristica o no
    */
  def apply(elemento: Int): Boolean = {
    funcionCaracteristica(elemento)
  }
}



/**
  * Objeto companion que ofrece metodos para trabajar con
  * conjuntos
  */
object Conjunto {
   /**
    * Limite para la iteracion necesaria algunas operaciones,
    * entre -1000 y 1000
    */
  private final val LIMITE = 1000
  
  /**
    * Metodo que permite crear objetos de la clase Conjunto
    * de forma sencilla
    * @param f
    * @return
    */
  def apply(f: Int => Boolean): Conjunto = {
    new Conjunto(f)
  }

  /**
    * Metodo para crear el Conjunto con un unico elemento
    * @param elemento Unico elemento del conjunto
    * @return
    */
  def conjuntoUnElemento(elemento : Int) : Conjunto = {

    new Conjunto((x:Int) => x==elemento)

  }

  /**
    * Metodo para crear el Conjunto union de dos
    * conjuntos que se pasan como argumentos
    * @param c1 Conjunto uno
    * @param c2 Conjunto dos
    * @return nuevo Conjunto union
    */
  def union(c1 : Conjunto, c2 : Conjunto) : Conjunto = {
    new Conjunto((x:Int) => (c1.funcionCaracteristica(x) || c2.funcionCaracteristica(x)))
  }

  /**
    * Metodo para crear el Conjunto interseccion de dos
    * conjuntos que se pasan como argumentos
    * @param c1 Conjunto uno
    * @param c2 Conjunto dos
    * @return nuevo Conjunto interseccion
    */
  def interseccion(c1 : Conjunto, c2 : Conjunto) : Conjunto = {
    new Conjunto((x:Int) => (c1.funcionCaracteristica(x) && c2.funcionCaracteristica(x)))
  }

  /**
    * Metodo para crear el Conjunto diferencia de dos
    * conjuntos que se pasan como argumentos
    * @param c1 Conjunto uno
    * @param c2 Conjunto dos
    * @return nuevo Conjunto diferencia
    */
  def diferencia(c1 : Conjunto, c2 : Conjunto) : Conjunto = {
    new Conjunto((x:Int) => (c1.funcionCaracteristica(x) && !c2.funcionCaracteristica(x)))
  }

  /**
    * Metodo para crear un nuevo Conjunto filtrando
    * los elementos segun un predicado
    * @param c Conjunto de partida
    * @param predicado condicion a cumplir
    * @return nuevo Conjunto con elementos filtrados
    */
  def filtrar(c : Conjunto, predicado : Int => Boolean) : Conjunto = {
    new Conjunto((x:Int) => (c.funcionCaracteristica(x) && predicado(x)))

  }

  // FUNCIONES AVANZADAS SOBRE CONJUNTOS

  /**
    * Metodo para comprobar que todos los elementos
    * del conjunto cumplen una determinada condicion
    * @param conjunto a comprobar
    * @param predicado a cumplir por todos los elementos
    * @return si se cumple el predicado para todos los elementos
    */
  def paraTodo(conjunto : Conjunto, predicado : Int => Boolean) : Boolean = {

    // Funcion auxiliar para implementacion tailrec
    def iterar(elemento: Int): Boolean = {

      if(elemento == LIMITE+1) true
      else if(!conjunto.funcionCaracteristica(elemento)) iterar(LIMITE+1)
      else{
        predicado(elemento) && iterar(elemento + 1)
      }

    }

    iterar(-LIMITE)
  }

  /**
    * Metodo para comprobar que existe al menos un elemento
    * en el conjunto c que cumple el predicado indicado
    * @param c conjunto a explorar
    * @param predicado que tiene que cumplir al menos un elemento
    * @return si existe al menos un elemento en el conjunto que cumpla la
    *         condicion
    */
  def existe(c : Conjunto, predicado : Int => Boolean) : Boolean = {

    // Utilizando la Logica de predicados:
    // Existe x en C que cumple predicado implica que
    // No para todos los elementos de C se cumple !predicado
    !paraTodo(c, (x: Int) => !(predicado(x)))
  }

  /**
    * Metodo map: transforma un conjunto en otro aplicando una
    * cierta funcion determinada
    * @param c conjunto a transformar
    * @param funcion a aplicar para transformar
    * @return nuevo Conjunto transformado
    */
  def map(c : Conjunto, funcion : Int => Int) : Conjunto = {

    // Tengo que comprobar si en c existe algun
    // elemento tal que al aplicarle funcion, sea igual
    // que el elemento transformado
    new Conjunto((x:Int) => existe(c,(valorC:Int) => x == funcion(valorC)))
  }

}