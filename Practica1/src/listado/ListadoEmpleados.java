package listado;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ListadoEmpleados {

    /**
     * Dato miembro estático para la separacion de cadenas por espacios
     */
    private static Pattern patronEspacios=Pattern.compile("\\s+");

    /**
     * Dato miembro para almacenar a los empleados tal y como se encuentran
     * en el archivo de datos.txt
     */
    private List<Empleado> listadoArchivo;

    /**
     * Dato miembro para almacenar a los empleados como mapa con pares
     * (una vez reparados los datos leidos del archivo)
     * <dni - empleado>
     */
    private Map<String, Empleado> listado;

    /**
     * Metodo auxiliar que crea un objeto de la clase Empleado a partir
     * de una linea del archivo de datos
     * @param linea cadena conteniendo la informacion del empleado
     */
    private Empleado crearEmpleado(String linea) {

        //Creamos un patron para considerar la coma como separador
        Pattern patron = Pattern.compile(",");

        //Creamos la lista de cadenas en funcion de la linea seperada
        List<String> informacion = patron.splitAsStream(linea).collect(Collectors.toList());

        //Creamos el nuevo empleado
        return new Empleado(informacion.get(0),
                informacion.get(1),
                informacion.get(2),
                informacion.get(3));
    }

    /**
     * Constructor de la clase
     * @param ruta cadena conteniendo la ruta del archivo de datos
     */
    public ListadoEmpleados(String ruta) throws IOException {

        //Inicializacion de datos miembro
        listado = new HashMap<>();

        //Obtenemos las lineas del fichero de datos indicado por ruta,
        //creando el flujo para procesar la lineas del archivo
        Stream<String> lineasEmpleados = Files.lines(Paths.get(ruta),
                StandardCharsets.ISO_8859_1);

        //Ahora para cada linea del flujo, llamamos al metodo empleado que se guardará
        //en la lista de empleados. Por medio del map, aplicamos a cada elemento del flujo
        //la llamada a la funcion crearEmpleado que devuelve un Empleado. Finalmente se recoge
        //el resultado transformandolo a una lista que sera nuestro listadoArchivo
        listadoArchivo = lineasEmpleados.map(this::crearEmpleado).collect(Collectors.toList());

    }

    /**
     * Metodo que devuelve el numero total de empleados que hay
     * antes de hacer la correccion de DNIs y correos
     * @return numero de empleados del dato miembro listadoArchivo
     */
    public int obtenerNumeroEmpleadosArchivo(){

        //Devuelve el tamaño de la lista de archivos, ha de ser 5000.
        return listadoArchivo.size();
    }

    /**
     * Metodo que devuelve si existe algun DNI repetido en la
     * lista de empleados
     * @return si existe repeticion de DNIs en la lista de empleados
     */
    public boolean hayDnisRepetidosArchivo(){

        //Basta con aislar los DNIs en otro flujo y aplicar la funcion distinct
        //para ver cuantos diferentes hay. Si no hubiese ninguno repetido, tendrían
        //que haber 5000 entradas.
        List<String> sinRepeticion = listadoArchivo.stream().map(Empleado::obtenerDni).
                distinct().collect(Collectors.toList());

        return sinRepeticion.size() != obtenerNumeroEmpleadosArchivo();

    }

    /**
     * Metodo que devuelve los DNIs repetidos de la lista
     * de empleados afectados por la repeticion de DNIs
     * @return Diccionario en donde para cada DNI repetido, se indican
     * los empleados afectados
     */
    public Map<String, List<Empleado>> obtenerDnisRepetidosArchivo(){

        //Agrupamos los Empleados por DNI
        Map<String, List<Empleado>> dnisRepetidos =
                listadoArchivo.stream().collect(Collectors.groupingBy(Empleado::obtenerDni));

        //Creamos el flujo con las entradas de este map obtenido para filtrar
        //aquellas entradas que tengan mas de un nombre en la lista
        dnisRepetidos = dnisRepetidos.entrySet().stream().
                filter(entrada -> entrada.getValue().size() > 1).
                collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        return dnisRepetidos;

    }

    /**
     * Metodo para contar el numero de empleados con DNIs repetidos
     * Toma como entrada la salida del metodo obtenerDnisRepetidosArchivo
     * @return Entero con el numero de empleados con DNIs repetidos
     */
    public int contarEmpleadosDnisRepetidos(){

        //Contamos el numero de valores de cada entrada del mapa
        //que devuelve la funcion obtenerDnisRepetidosArchivo
        return obtenerDnisRepetidosArchivo().
                entrySet().stream().
                mapToInt(entry -> entry.getValue().size()).sum();
    }

    /**
     * Metodo para reparar los DNIs repetidos que devuelve la funcion
     * obtenerDnisRepetidosArchivo. Cuando se realice la reparacion, se ha
     * de modificar el dato miembro listadoArchivo para que contemple el cambio
     * @param listaRepeticion lista de empleados cuyos DNIs estan repetidos
     */
    public void repararDnisRepetidos(Map<String, List<Empleado>> listaRepeticion){

        //Para cada Empleado que tenga un DNI repetido con otro/s
        //hay que buscarlo en el listadoArchivo y reparar su DNI
        listaRepeticion.values().stream().
                flatMap(Collection::stream).
                forEach(empleado -> {
                    int i = listadoArchivo.indexOf(empleado);
                    listadoArchivo.get(i).asignarDniAleatorio();

                });
    }

    /**
     * Metodo para comprobar si existen correos repetidos entre Empleados
     * @return si existe repeticion de correos o no
     */
    public boolean hayCorreosRepetidosArchivo(){

        List<String> sinRepeticion = listadoArchivo.stream().map(Empleado::obtenerCorreo).
                distinct().collect(Collectors.toList());

        return sinRepeticion.size() != obtenerNumeroEmpleadosArchivo();
    }

    /**
     * Metodo que devuelve los correos repetidos de la lista
     * de empleados afectados por la repeticion de correos
     * @return Diccionario en donde para cada correo repetido, se indican
     * los empleados afectados
     */
    public Map<String, List<Empleado>> obtenerCorreosRepetidosArchivo(){

        //Agrupamos los Empleados por correo
        Map<String, List<Empleado>> correosRepetidos =
                listadoArchivo.stream().collect(Collectors.groupingBy(Empleado::obtenerCorreo));

        //Creamos el flujo con las entradas de este map obtenido para filtrar
        //aquellas entradas que tengan mas de un nombre en la lista
        correosRepetidos = correosRepetidos.entrySet().stream().
                filter(entrada -> entrada.getValue().size() > 1).
                collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        return correosRepetidos;

    }


    /**
     * Metodo para contar el numero de empleados con correos repetidos
     * Toma como entrada la salida del metodo obtenerCorreosRepetidosArchivo
     * @return Entero con el numero de empleados con correos repetidos
     */
    public int contarCorreosRepetidos(){

        //Contamos el numero de valores de cada entrada del mapa
        //que devuelve la funcion obtenerCorreosRepetidosArchivo
        return obtenerCorreosRepetidosArchivo().
                entrySet().stream().
                mapToInt(entry -> entry.getValue().size()).sum();

    }

    /**
     * Metodo para reparar los correos repetidos que devuelve la funcion
     * obtenerDnisRepetidosArchivo. Cuando se realice la reparacion, se ha
     * de modificar el dato miembro listadoArchivo para que contemple el cambio
     * @param listaRepeticiones lista de los correos repetidos y los empleados afectados
     */
    public void repararCorreosRepetidos(Map<String, List<Empleado>> listaRepeticiones){

        //Para cada Empleado que tenga un correo repetido con otro/s
        //hay que buscarlo en el listadoArchivo y reparar su correo
        listaRepeticiones.values().stream().
                flatMap(Collection::stream).
                forEach(empleado -> {
                    int i = listadoArchivo.indexOf(empleado);
                    listadoArchivo.get(i).generarCorreoCompleto();

                });
    }

    /**
     * Metodo que una vez realizada la reparacion de todos los problemas
     * en cuanto a repeticion de DNIs y correos, inicializa el dato miembro
     * listado
     */
    public void validarListaArchivo(){

        listado= listadoArchivo.stream().
                collect(Collectors.toMap(Empleado::obtenerDni,empleado -> empleado));

    }

    /**
     * Metodo que comprueba si existe algun Empleado en listado con DNI dni
     * @param dni La clave a buscar en el mapa listad
     * @param unSector Sector al que será asignado
     * @return si el empleado está en el listado o no
     */
    public boolean procesarAsignacionSector(String dni, Sector unSector){

        //Aislamos el DNI
        List<String> infos = patronEspacios.splitAsStream(dni).collect(Collectors.toList());

        //Buscamos el empleado y si existe le asignamos el sector
        Empleado empleado = listado.get(infos.get(0));

        boolean existeEmpleado = false;

        if(empleado != null) {
            empleado.asignarSector(unSector);
            existeEmpleado = true;
        }
        return existeEmpleado;

    }

    /**
     * Metodo que comprueba si existe algun Empleado en listado con DNI dni
     * @param dni La clave a buscar en el mapa listado
     * @param unaRuta Ruta al la que será asignado
     * @return si el empleado está en el listado o no
     */
    public boolean procesarAsignacionRuta(String dni, Ruta unaRuta){

        //Aislamos el DNI
        List<String> infos = patronEspacios.splitAsStream(dni).collect(Collectors.toList());

        //Buscamos el empleado y si existe le asignamos el sector
        Empleado empleado = listado.get(infos.get(0));

        boolean existeEmpleado = false;

        if(empleado != null) {
            empleado.asignarRuta(unaRuta);
            existeEmpleado = true;
        }
        return existeEmpleado;
    }

    /**
     * Metodo que procesa una cadena que de coincidir con el nombre
     * de un Sector, devuelve un Sector con el valor indicado
     * @param unSector cadena a comprobar
     * @return Sector cuyo valor es unSector
     */
    public Sector procesarNombreSector(String unSector){

        //Troceamos la cadena por espacios en blanco y obtenemos un unico
        //objeto de tipo cadena
        List<String> infos = patronEspacios.splitAsStream(unSector).collect(Collectors.toList());

        //Predicado para filtrar los nombres
        Predicate<Sector> condicion = sector -> sector.name().equals(infos.get(0));

        //Devolvemos el resultado si existe
        return Arrays.stream(Sector.values()).filter(condicion).findFirst().get();
    }

    /**
     * Metodo que procesa una cadena que de coincidir con el nombre
     * de una Ruta, devuelve una Ruta con el valor indicado
     * @param unaRuta cadena a comprobar
     * @return Ruta cuyo valor es unaRuta
     */
    public Ruta procesarNombreRuta(String unaRuta){

        //Troceamos la cadena por espacios en blanco y obtenemos un unico
        //objeto de tipo cadena
        List<String> infos = patronEspacios.splitAsStream(unaRuta).collect(Collectors.toList());

        //Predicado para filtrar los nombres
        Predicate<Ruta> condicion = ruta -> ruta.name().equals(infos.get(0));

        //Devolvemos el resultado si existe
        return Arrays.stream(Ruta.values()).filter(condicion).findFirst().get();
    }

    /**
     * Metodo que devuelve el numero de errores en la asignacion de
     * sectores a Empleados
     * @param archivo Fichero con DNIs de los empleados a asignar a un Sector
     * @return Numero de errores al asignar un Sector a un Empleado inexistente
     */
    public long cargarArchivoAsignacionSector(String archivo) throws IOException {

        //Cargamos el fichero de asignacion de Sector determinado y creamos el sector
        List<String> lineas = Files.lines(Paths.get(archivo)).collect(Collectors.toList());
        Sector sector = procesarNombreSector(lineas.get(0));

        //Para cada DNI en la lista de sectores, comprobamos su existencia y contamos
        //los errores en las asignaciones
        return lineas.stream().skip(2).
                map(linea->procesarAsignacionSector(linea, sector)).
                filter(flag->flag==false).count();
    }

    /**
     * Metodo que devuelve el numero de errores en la asignacion de
     * Rutas a Empleados
     * @param archivo Fichero con DNIs de los empleados a asignar a una Ruta
     * @return Numero de errores al asignar una Ruta a un Empleado inexistente
     */
    public long cargarArchivoAsignacionRuta(String archivo) throws IOException {

        //Cargamos el fichero de asignacion de Sector determinado y creamos el sector
        List<String> lineas = Files.lines(Paths.get(archivo)).collect(Collectors.toList());
        Ruta unaRuta = procesarNombreRuta(lineas.get(0));

        //Para cada DNI en la lista de rutas, comprobamos su existencia y contamos
        //los errores en las asignaciones
        return lineas.stream().skip(2).
                map(linea->procesarAsignacionRuta(linea, unaRuta)).
                filter(flag->flag==false).count();
    }

    /**
     * Metodo auxiliar para contar el número de empleados
     * asignados a cada ruta dentro de un determinado sector
     * @param sector Sector a consultar
     * @return Número de empleados por cada ruta dentro de un sector
     */
    public Map<Ruta, Long> obtenerContadoresRuta(Sector sector){

        //Filtramos aquellos empleados que pertenecen al Sector determinado,
        //ordenamos y  agrupamos por rutas, contando los elementos en cada lista
        //Hace falta utilizar un LinkedHashMap para mantener el orden despues
        //de que se ejecute el sorted. Dado que este hereda de HashMap, que a su vez
        //implementa la interfaz Map, no hace falta conversion de tipos
        return listado.values().stream().
                filter(empleado -> empleado.obtenerSector() == sector).
                sorted(Comparator.comparing(Empleado::obtenerRuta)).
                collect(Collectors.groupingBy (Empleado::obtenerRuta,
                        LinkedHashMap::new,
                        Collectors.mapping(Empleado::obtenerRuta, Collectors.counting())));
    }

    /**
     * Metodo que devuelve cuantos empleados hay en una ruta determinada
     * dentro de un sector determinado
     * @return Numero de empleados por Ruta dentro de un Sector
     */
    public Map<Sector, Map<Ruta, Long>> obtenerContadoresSectorRuta(){

        //Para cada sector obtenemos los contadores de ruta y recogemos el resultado
        //en nuestro diccionario de salida
        return Arrays.stream(Sector.values()).
                sorted(Comparator.comparing(Sector::ordinal)).
                collect(Collectors.toMap(sector -> sector, this::obtenerContadoresRuta));

    }

    /**
     * Metodo que recoge el numero de Empleados en cada sector.
     * Parte de la informacion que proporciona obtenerContadoresSectorRuta
     * @return Array con la cantidad de empleados por sector
     */
    public List<Long> obtenerContadoresSectores(){

        //Obtengo un flujo en el que para cada sector tengo la lista
        //de la cantidad de empleados por cada ruta
        return obtenerContadoresSectorRuta().values().stream().
                map(Map::values).
                map(valor -> valor.stream().reduce((long) 0, Long::sum)).
                collect(Collectors.toList());
    }

    /**
     * @return Lista de Empleados sin Sector y sin Ruta
     */
    public List<Empleado> buscarEmpleadosSinSectorSinRuta(){

        //Filtramos aquellos empleados sin Sector ni Ruta
        return listado.values().stream().
                filter(empleado -> empleado.obtenerSector()==Sector.NOSECTOR &&
                        empleado.obtenerRuta() == Ruta.NORUTA).
                collect(Collectors.toList());
    }

    /**
     * Metodo que devuelve la lista de Empleados en unSector y NORUTA
     * @param unSector Sector al que pertenecen los Empleados
     * @return Empleados en unSector y sin Ruta
     */
    public List<Empleado> buscarEmpleadosSinRuta(Sector unSector){

        return listado.values().stream().
                filter(empleado -> empleado.obtenerSector()==unSector &&
                        empleado.obtenerRuta() == Ruta.NORUTA).
                collect(Collectors.toList());

    }

    /**
     * Metodo que devuelve la lista de Empleados asignados a algun
     * Sector pero que no tienen Ruta asignada.
     * @return Empleados en algun Sector pero sin Ruta
     */
    public List<Empleado> buscarEmpleadosConSectorSinRuta(){

        //Eliminamos el Sector NOSECTOR y posteriormente obtenemos los empleados
        //sin ruta pero dentro de algun Sector
        return Arrays.stream(Sector.values()).
                filter(sector -> sector != Sector.NOSECTOR).
                map(this::buscarEmpleadosSinRuta).
                flatMap(Collection::stream).
                collect(Collectors.toList());
    }

    /**
     * Metodo que devuelve los Empleados asignados a una ruta
     * pero sin Sector
     * @param ruta Ruta a la que pertenecen los Empleados
     * @return Empleados en ruta pero sin Sector
     */
    public List<Empleado> buscarEmpleadosSinSector(Ruta ruta) {

        return listado.values().stream().
                filter(empleado -> empleado.obtenerSector() == Sector.NOSECTOR &&
                        empleado.obtenerRuta() == ruta).
                collect(Collectors.toList());
    }

    /**
     * Metodo que devuelve la lista de Empleados asignados a alguna
     * Ruta pero que no tienen Sector asignado.
     * @return Empleados en algun Sector pero sin Ruta
     */
    public List<Empleado> buscarEmpleadosSinSectorConRuta(){

        //Eliminamos la Ruta NORUTA y posteriormente obtenemos los empleados
        //sin ruta pero dentro de algun Sector
        return Arrays.stream(Ruta.values()).
                filter(ruta -> ruta != Ruta.NORUTA).
                map(this::buscarEmpleadosSinSector).
                flatMap(Collection::stream).
                collect(Collectors.toList());
    }



    public static void main(String[] args) throws IOException {

        ListadoEmpleados prueba = new ListadoEmpleados("./data/datos.txt");
        prueba.cargarArchivoAsignacionSector("./data/asignacionSECTOR1.txt");
    }

}
