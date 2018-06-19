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


  // __________________________________________________________________________
  // PRUEBA VALORES ARRAY
  // __________________________________________________________________________
  println("__________________________________________________________________________")
  val variable1 = Variable("X3",2)
  val variable2 = Variable("X4",2)
  val valoresArray1 = ValoresArray(Dominio(List(variable1,variable2)),List(0.2, 0.8, 0.6, 0.4))
  println("ValoresArray:")
  println(valoresArray1 + "\n")
  println("__________________________________________________________________________")


  // PRUEBA COMBINAR ARRAY ARRAY
  val var1 = Variable("X1",2)
  val var2 = Variable("X2",2)
  val var3 = Variable("X3",2)

  val valores1 = ValoresArray(Dominio(List(var1,var2)),List(0.3,0.7,0.6,0.4))
  val valores2 = ValoresArray(Dominio(List(var2,var3)),List(0.9,0.1,1.0,0.0))
  println("__________________________________________________________________________")
  println("ValoresArray: \n")
  println("valores1:")
  println(valores1 + "\n")
  println("valores2")
  println(valores2 + "\n")
  println("ValoresArray combinado: ")
  val valoresArrayCombinado = valores1.combinar(valores2)
  println(valoresArrayCombinado)
  println("__________________________________________________________________________")


  // PRUEBA RESTRINGIR VALORES ARRAY
  println("__________________________________________________________________________")
  println("ValoresArray: \n")
  println("valores1:")
  println(valores1 + "\n")
  println("Valores restringidos a (X1,0) sobre valores1: ")
  val valoresRestringidos = valores1.restringir(var1,0)
  println(valoresRestringidos)
  println("Valores restringidos a (X1,0) sobre valoresArrayCombinado: ")
  val valoresRestComb = valoresArrayCombinado.restringir(var1,0)
  println(valoresRestComb)
  println("__________________________________________________________________________")


  // PRUEBA VALORES ARBOL
  println("__________________________________________________________________________")
  println("ValoresArbol: \n")
  println("valoresArbol:")
  val valoresArbol = ValoresArbol.apply(Dominio(List(var1,var2,var3)),
    List(0.27,0.03,0.63,0.07,0.6,0.0,0.4,0.0))
  println(valoresArbol)
  println("Comprobacion de obtener valor")
  val asignacionValoresArbol = Asignacion(Dominio(List(var1,var2,var3)),List(1,1,0))
  println("Valor de X1,X2,X3 con valores (1,1,0) debe ser 0.4 : " +
    valoresArbol.obtenerValor(asignacionValoresArbol))
  println("__________________________________________________________________________")


  // PRUEBA RESTRINGIR VALORES ARBOL
  println("__________________________________________________________________________")
  println("ValoresArbol: \n")
  println("Restringir Valores Arbol (X2 y valor 0)")
  val arbolRestringido = valoresArbol.restringir(var2,0)
  println(arbolRestringido)
  println("__________________________________________________________________________")


  // COMPROBACION COMBINAR
  val valoresArbol2 = ValoresArbol(Dominio(List(var1,var2)), List(0.3,0.7,0.6,0.4))
  val arbolCombinado1 = valoresArbol2.combinar(ValoresArbol(Dominio(List()), List(0.3)))
  println("__________________________________________________________________________")
  println("ValoresArbol Combinado (Variable-NodoHoja): ")
  println(arbolCombinado1)
  println("__________________________________________________________________________")

  val valoresArbol3 = ValoresArbol(Dominio(List(var1,var3)), List(0.1,0.1,1.0,0.0))
  val arbolCombinado2 = valoresArbol2.combinar(valoresArbol3)
  println("__________________________________________________________________________")
  println("ValoresArbol Combinado (Variable-Variable): ")
  println(arbolCombinado2)
  println("__________________________________________________________________________")


}
