import listado.*;
import org.junit.BeforeClass;
import org.junit.Test;
import java.util.List;

import java.io.IOException;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Práctica 1 NTP
 */
public class ListadoTestP1 {
    private static ListadoEmpleados listado;

    /**
     * Codigo a ejecutar antes de realizar las llamadas a los métodos
     * de la clase; incluso antes de la propia instanciación de la
     * clase. Por eso el método debe ser estatico
     */
    @BeforeClass
    public static void inicializacion() {
        System.out.println("Metodo inicializacion conjunto pruebas");
        // Se genera el listado de empleados
        try {
            listado = new ListadoEmpleados("./data/datos.txt");
        } catch (IOException e) {
            System.out.println("Error en lectura de archivo de datos");
        };
    }

    /**
     * Test para comprobar que se ha leido de forma correcta la
     * informacion de los empleados
     *
     * @throws Exception
     */
    @Test
    public void testConstruccionListado() throws Exception {
        assert (listado.obtenerNumeroEmpleadosArchivo() == 5000);
    }


    /**
     * Test para comprobar que se ha reparado correctamente
     * la repeticion de DNIs
     */
    @Test
    public void testComprobarReparacionEfectivaDnis(){
        //Comprobamos que antes habia repeticiones
        boolean condicionPrevia = listado.hayDnisRepetidosArchivo();
        int cantidadRepetidos = listado.contarEmpleadosDnisRepetidos();
        Map<String, List<Empleado>> empleadosDnisRepes = listado.obtenerDnisRepetidosArchivo();

        //Reparamos los dnis repetidos
        listado.repararDnisRepetidos(empleadosDnisRepes);

        //Comprobamos que se ha reparado correctamente
        boolean condicionPosterior = listado.hayDnisRepetidosArchivo();

        assert(condicionPrevia == true && cantidadRepetidos == 4 && condicionPosterior == false);

    }


    /**
     * Test para comprobar el numero de empleados con correos
     * repetidos
     */
    @Test
    public void testComprobarContadoresCorreosRepetidosArchivo() {

        assert (listado.contarCorreosRepetidos() == 315);
    }

    /**
     * Test para comprobar que se ha reparado correctamente
     * los correos repetidos
     */
    @Test
    public void testComprobarReparacionEfectivaCorreos(){
        //Comprobamos que antes habia repeticiones
        boolean condicionPrevia = listado.hayCorreosRepetidosArchivo();
        int cantidadRepetidos = listado.contarCorreosRepetidos();
        Map<String, List<Empleado>> empleadosCorreosRepes = listado.obtenerCorreosRepetidosArchivo();

        //Reparamos los dnis repetidos
        listado.repararCorreosRepetidos(empleadosCorreosRepes);

        //Comprobamos que se ha reparado correctamente
        boolean condicionPosterior = listado.hayCorreosRepetidosArchivo();

        assert(condicionPrevia == true &&cantidadRepetidos == 315 && condicionPosterior == false );

    }
}
