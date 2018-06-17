object MainPrueba extends App{

  // Se crea dominio vacio
  val dominioVacio = Dominio(List())

  // Se comprueba que funciona el metodo asociado a comprobar la condicion
  // de dominio vacio
  println("Comprobacion de vacio sobre dominio vacio: "+ dominioVacio.vacio)

  // Se crean 4 variables
  val X1 = Variable("X1",3)
  val X2 = Variable("X2",4)
  val X3 = Variable("X3",2)
  val X4 = Variable("X4",2)

  // Se crea un dominio con las variables creadas antes
  val dominioNoVacio=Dominio(List(X1,X2,X3,X4))

  // Este dominio ya no esta vacio
  println("Comprobacion de vacio sobre dominio no vacio: "+dominioNoVacio.vacio)

  // Se obtiene la longitud del dominio
  val longitud=dominioNoVacio.longitud
  println("Longitud del dominio no vacio (debe ser 4): "+longitud)

  // Se muestra el objeto usando toString
  println(dominioNoVacio)

  // Se suma una variable al dominioNoVacio
  val X5 = new Variable("X5", 5)
  val dominioSuma=dominioNoVacio+X5
  println("Dominio suma (+X5): "+dominioSuma)

  // Se crea un dominio sobre X4, X5 y X6
  val X6 = new Variable("X6", 3)
  val dominioNoVacio2=new Dominio(List(X1, X2, X5, X6))

  // Se genera ahora la suma de los dos dominios no vacios
  val dominioSuma2=dominioNoVacio + dominioNoVacio2
  println("Suma de dominios: "+dominioSuma2)

  val maximoIndice=dominioSuma2.maximoIndice
  println("Maximo indice de dominio de suma: "+ maximoIndice)

  // Comprobar el operador resta con una variable
  val dominioResta1 = dominioNoVacio - X1
  println("Dominio resta (-X2) " + dominioResta1)

  // Comprobar el operador resta con un dominio
  val dominioResta2 = dominioSuma2 - dominioNoVacio
  println("Dominio resta dominioNoVacio2 " + dominioResta2)


  // Se crea asignacion vacia y se comprueba su chequeo
  val asignacionVacia = Asignacion(Dominio(List()), List())
  println("Comprobacion vacio asignacion vacia: "+asignacionVacia.vacia)

  // Se crea asignacion , dando valores 2, 3, 1 y 0 a las variables
  val asignacion1 = Asignacion(Dominio(List(X1, X2,X3, X4)),List(2,3,1,0))
  println("Comprobacion vacio sobre asignacion no vacia: " + asignacion1.vacia)
  println("Se muestra la asignacion: ")
  println(asignacion1)

  // Obtener valor de la variable
  val valorVariable = asignacion1.obtenerValorVariable(X4)
  val valorVariableNE = asignacion1.obtenerValorVariable(X2)

  println("Valor de la variable var4 en la asignacion1: " + valorVariable)
  println("Valor de la variable X2 en la asignacion1: " + valorVariableNE)

  // Añadir un par variable valor a la asignacion
  val asignacion2 = asignacion1 + (X6,0)
  println("Asignacion2 con par (X6,2) añadido: " + asignacion2)

  // Calculo del indice asociado a la asignacion
  val indice1=asignacion1.calcularIndice
  println("indice1 (debe ser 46): " + indice1)

  // Comprobar proyeccion
  val proyeccion1 = asignacion2.proyectar(Dominio(List(X3,X4)))
  println("Comprobar proyeccion sobre X3 y X4: "+ proyeccion1)

  // Comprobar creacion con solo el dominio
  val asignacion3 = Asignacion(dominioNoVacio)
  println("Asignacion con valores 0: " + asignacion3)

  // A partir del indice obtenemos la asignacion
  val asignacionDeIndice = Asignacion(asignacion1.dominio, indice1)

  // Se muestra la asignacion obtenida: debe ser X1=2, X2=3, X3=1, X4=0
  println("Asignacion resultante a traves de un indice: " + asignacionDeIndice)

}