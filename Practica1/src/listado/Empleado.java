package listado;

import java.util.HashMap;
import java.util.List;
import java.util.Random;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Clase para gestionar empleados, dnis y grupos de practicas
 * @author mgomez
 */
public class Empleado {
    /**
     * Dato miembro para almacenar dni
     */
    private String dni;

    /**
     * Datos miembro para almacenar apellido1, apellido2 y nombre
     */
    private String apellidos, nombre;

    /**
     * Dato miembro para almacenar el correo
     */
    private String correo;

    /**
     * Dato miembro para almacenar el sector asignado
     */
    private Sector sector;

    /**
     * Dato miembro para almacenar la asignacion a ruta
     */
    private Ruta ruta;

    /**
     * Dato miembro para almacenar un generador de numeros aleatorios
     */
    private static Random generador = new Random();

    /**
     * Constructor de la clase
     * @param dni
     * @param apellidos
     * @param nombre
     * @param correo
     */
    public Empleado(String dni, String nombre,
                    String apellidos, String correo) {
        this.dni = dni;
        this.nombre = nombre;
        this.apellidos = apellidos;
        this.correo = correo;

        // Por defecto no sde asigna ni sector ni ruta (representado
        // con el valor NA en cada enumerado)
        sector = Sector.NOSECTOR;
        ruta = Ruta.NORUTA;
    }

    /**
     * Asigna el dni
     * @param dni
     */
    public void asignarDni(String dni){
        this.dni=dni;
    }

    /**
     * Metodo para asignar el sector a un empleado
     * @param sector
     */
    public void asignarSector(Sector sector){
        this.sector =sector;
    }

    /**
     * Metodo para asignar la ruta a un empleado
     * @param ruta
     */
    public void asignarRuta(Ruta ruta){
        this.ruta =ruta;
    }

    /**
     * Modifica el dni actual generando un numero aleatorio
     * entre 0 y 99
     */
    public void asignarDniAleatorio(){
        System.out.println("A asignar: "+dni+generador.nextInt(100));
        dni=dni+generador.nextInt(100);
    }

    /**
     * Metodo que permite asignar el sector de forma aleatoria
     */
    public void asignarSectorAleatorio() {
        // Se genera un numero aleatorio para el sector
        int sectorAleatorio = generador.nextInt(Sector.values().length);

        // Se asigna
        sector =(Sector.values())[sectorAleatorio];
    }

    /**
     * Metodo para asignar la ruta de forma aleatoria
     */
    public void asignarRutaAleatorio() {
        int rutaAleatorio = generador.nextInt(Ruta.values().length);
        // Se asigna el ruta
        ruta =(Ruta.values())[rutaAleatorio];
    }

    public void generarCorreoCompleto(){
        correo=nombre.toLowerCase()+apellidos.toLowerCase()+"@acme.com";
        // Se eliminan espacios en blanco
        correo=correo.replaceAll("\\s+","");
    }

    /**
     * Metodo para obtener el dni
     *
     * @return
     */
    public String obtenerDni() {
        return dni;
    }

    /**
     * Metodo para obtener el correo
     */
    public String obtenerCorreo(){
        return correo;
    }

    /**
     * Recupera la dvision
     * @return
     */
    public Sector obtenerSector(){
        return sector;
    }
    /**
     * Metodo que devuelve el ruta
     * @return
     */
    public Ruta obtenerRuta() {
        return ruta;
    }

    /**
     * Metodo toString
     * @return
     */
    public String toString() {
        String info = "------------------------------------------------\n";
        info = info + "DNI: " + dni + "  " + nombre + "  " + apellidos +
                "  " + correo + " ";
        // Se muestra tambien informacion sobre sector y ruta
        info = info + " " + sector.toString() + " : " + ruta.toString();
        info = info + "\n------------------------------------------------\n";
        return info;
    }

    /**
     * Metodo toLine genera una linea con la informacion de los empleados,
     * separada por comas
     *
     * @return
     */
    public String generarLineaSimple() {
        String info = dni + ",  " + nombre + ",  " + apellidos + ",  " + correo;
        return info;
    }

    /**
     * Metodo para generar los datos de un empleado, con dni y
     * ruta
     * @return
     */
    public String generarLineaDniRuta() {
        String info = dni + " " + ruta.toString();
        return info;
    }

    /**
     * Metodo para generar los datos de un empleado, con dni y
     * sector
     * @return
     */
    public String generarLineaDniSector() {
        String info = dni + " " + sector.toString();
        return info;
    }


    /**
     * Metodo que devuelve un valor booleano indicando si un empleado
     * pertenece a un sector
     * @param sector
     * @return
     */
    public boolean perteneceSector(Sector sector) {
        // Se devuelve el resultado de la comparacion
        return(this.sector == sector);
    }
}
