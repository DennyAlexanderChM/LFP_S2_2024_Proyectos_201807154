# Lenguajes Formales de programación 
## Manual Técnico Proyecto 1 (índice)
* ⚪ Introducción
* 🟡 Información técnica
	- 🟨  Frontend
	- 🟨  Backend
    - 🟨  Dependencias (Frontend)
    - 🟨  IDE
* 🟠 Descripción de la aplicación
* 🔴 Tabla descriptiva de archivos
* 🟢 Descripción de modulos y funciones
	- 🟩 Backend
	- 🟩 Frontend
* 🔵 Tabla de tokens y patrones
* ⚫ Obtención de automata
	- ⬛ Agregar estado de aceptación
	- ⬛ Dibujar árbol de expresión
	- ⬛ Tabla de siguientes
	- ⬛ Reducción del autómata
	- ⬛ Autómata
## ⚪ Introducción
El presente documento describe los aspectos técnicos informáticos del sistema desarrollado, teniendo un control e información oportuna de los requerimientos que se solicitan.

## 🟡 Información Técnica
### 🟨 Frontend
Lenguaje de programación: Python

Versión: 3.12.4

### 🟨 Frontend
Lenguaje de programación: Fortran 90

Versión compilador: Gfortran 12.3.0

### 🟨 Dependencias o librerias (Frontend)
Libreria: Pillow

Lenguaje de programación: Python

Versión: Gfortran 10.4.0

### 🟨 IDE utlilizado
IDE: Visual Studio Code

Versión: 1.93.1

## 🟠 Descripción de la aplicación
Se poseé un archivo de texto plano con extención **.ORG,** el cual se visualiza por medio de una aplicación desarrollada en Python utilizando la biblioteca Tkinter.

El archivo .ORG posee su propio lenguaje definido en el que se pueden visualizar los lugares propuestos, su ubicación geográfica y un porcentaje de qué tan saturado está el mercado en dicho lugar. Para el análisis del archivo se desarrollo un **analizador léxico** aplicando el método del árbol para obtener un **AFD**, el cual fue empleado utilizando Fortran 90.

Además la aplicación permite a los usuarios las siguientes funciones:

* Editor de texto
* Abrir un nuevo archivo
* Guardar
* Guardar Como
* Analizar
* Mostrar gráfica
* Acerca de
* Salir
* Generar reporte de Tokens
* Generar Reporte de Errores Léxicos

## 🔴 Tabla descriptiva de archivos

La estructura de archivos del proyecto es la siguiente
| Directorio/ Archivo                  | Descripción                                  |
| ------------------------------------ | -------------------------------------------- |
| 'main.py'                            | Ejecutable del Frontend.                     |
| 'main.exe'                           | Ejecutable del Backend (Análizador).         |
| 'fortran_files/'                     | Contiene el codigo del Backend.              |
| 'fortran_files/analizador.f90'       | Modulo analizador y token.                   |
| 'fortran_files/continente.f90'       | Modulo continente.                           |
| 'fortran_files/grafica.f90'          | Modulo grafica.                              |
| 'fortran_files/main.f90'             | Programa principal que ejecuta el proyecto.  |
| 'fortran_files/moduloanalizador.mod' | Archivo de módulo para el Módulo Analizador. |
| 'fortran_files/modulocontinente.mod' | Archivo de módulo para el Módulo Continente. |
| 'fortran_files/modulografica'        | Archivo de módulo para el Módulo Grafica.    |
| 'docs/'                              | Documentos del proyecto.                     |
| 'docs/images/'                       | Imagenes del Proyecto.                       |
| 'test_files/'                        | Archivos de prueba para el sistema.          |

## 🟢 Descripción de modulos y funciones
### 🟩 Frontend
#### Programa principal main
Archivo: '/main.py'

Descripción: Utiliza la libreria Tkinter para crear y mostrar una ventana junto con sus elementos gráficos.'

Variables Globales

* app: Variable Clase MyAplication creada con Tkinter, contiene la ventana y sus elementos gráficos.

Procedientos y funciones de la Clase MyAplication

* initUI
	- Descripción: Inicializa los elemenetos de la ventana (labels, cuadros detexto, menú y bótones) para ubicarlos en sus posiciones correspondientes.

	- Entradas: Ninguna
	
	- Salidas: Ninguna

* menuBar
	- Descripción: Crea un menú con las funcionalidades del programa.
	
	- Entradas: Ninguna
	
	- Salidas:

		tk.Menu(self)

* aboutUs
	- Descripción: Despliega una alerta con los datos del desarrollador.
	
	- Entradas: Ninguna
	
	- Salidas: Ninguna

* enviarDatos
	- Descripción: Verifica si los datos del campo de texto han sido guardados para luego hacer la conexion con el backend. Tambén se encarga de recibir el mensaje y mostrar en pantalla los datos recibidos.

	- Entradas: Ninguna
	
	- Salidas: Ninguna

* new_file
	- Descripción: Verifica si los datos del campo de texto han sido guardados para luego limpiar el campo de texto.
	
	- Entradas: Ninguna
	
	- Salidas: Ninguna

* open_file
	- Descripción: Verifica si los datos del campo de texto han sido guardados para luego abrir una ventana emergente para cargar el nuevo archivo de entrada.
	
	- Entradas: Ninguna
	
	- Salidas: Ninguna

* save_file
	- Descripción: guarda los cambios efectuados en el archivo abierto.
	
	- Entradas: Ninguna
	
	- Salidas: Ninguna

* save_as_file
	- Descripción: Guarda los cambios efectuados en el campo de texto, abre una ventana emergente para verificar el sitio en el que se guardara el nuevo documento '.ORG'.
	
	- Entradas: Ninguna
	
	- Salidas: Ninguna

* on_text_change
	- Descripción: Verifica si el contenido del campo de texto a sido modificado.
	
	- Entradas: Ninguna
	
	- Salidas: Ninguna

* check_for_save
	- Descripción: Verifica si el contenido del archivo de texto a sido guardado.
	
	- Entradas: Ninguna
	
	- Salidas: Ninguna

### 🟩 Backend
#### Programa principal: main
Archivo: 'fortran_files/main.f90'

Descripción: Recibe el texto desde el frontend para luego analizar y generar los reportes y gráficas'

Variables Globales

* miAnalizador: variable de tipo Analizador

* miGrafica: variable de tipo Grafica

Subrutinas y funciones

* readFile
	- Descripción: Lee y analiza el texto línea por línea.
	- Entradas: ninguna
	- Salida: ninguna

#### Módulo: ModuloAnalizador

Archivo: 'fortran_Files/analizador.f90'

Descripción: Almacena los type token y analizador, cada una una con sus propias carateristícas y funciones.

Subrutinas y funciones (Analizador contains)

* analizarCadena
	- Descripción: Se base en AFD, se encarga de clasificar los tokens y lexemas.
	
	- Entradas:

		class(Analizador), intent(inout) :: this

		character(len=200), intent(in) :: cadena
        
		integer, intent(in) :: fila

	- Salida: ninguna

* palabraReservada
	- Descripción: Verifica si el lexema pertenece a la lista de palabras reservadas.

	- Entradas:

		class(Analizador), intent(inout) :: this

	- Salida: ninguna

* agregarToken
	- Descripción: Agrega el token encontrado a la lista perteneciente al analizador.

	- Entradas:
	
		class(Analizador), intent(inout) :: this

	- Salida: ninguna

* guardarDatos 
	- Descripción: Se encarga de analizar los tokens para su clasificación y almacenamiento en el Type Grafica.
	- Entradas:

		class(Analizador), intent(inout) :: this

		type(Grafica), intent(inout) :: miGrafica

	- Salida: ninguna

* reporteTokens
	- Descripción: Crear el archivo HTML del reporte de tokens.

	- Entradas:

		class(Analizador), intent(inout) :: this

	- Salida: ninguna

* reporteError
	- Descripción: Crear el archivo HTML del reporte de errores lexicos encontrados.

	- Entradas:

		class(Analizador), intent(inout) :: this

	- Salida: ninguna

#### Módulo: ModuloContinente

Archivo: 'fortran_Files/continente.f90'

Descripción: Almacena los type continente y pais, cada una una con sus propias carateristícas y funciones.

Subrutinas y funciones (Continente contains)

* addPais
	- Descripción: Añade un elemento pais a la lista del continente.
	
	- Entradas:

		class(Continente), intent(inout) :: this

        character(len=100), intent(in) :: nombrePais, bandera
		
        integer, intent(in) :: poblacion, saturacion
        
		class(Pais), allocatable :: temp(:)

	- Salida: ninguna

* densidadMedia
	- Descripción: Obtiene la densidad media del continente.

	- Entradas:

		class(Continente), intent(inout) :: this

	- Salida: ninguna

#### Módulo: ModuloGrafica

Archivo: 'fortran_Files/grafica.f90'

Descripción: Almacena los type Grafica con sus carateristícas y funciones.

Subrutinas y funciones

* addContinente
	- Descripción: Añade un elemento Continente a la lista.

	- Entradas:

		class(Grafica), intent(inout) :: this

        character(len = 100), intent(in) :: nombreContinente

	- Salida: ninguna

* addPaisContinente
	- Descripción: Llama al ultimo elemento continente para agregar un elemento pais a su lista.
	
	- Entradas:

		class(Grafica), intent(inout) :: this

        character(len=100), intent(in) :: nombrePais, bandera

        integer, intent(in) :: poblacion, saturacion

	- Salida: ninguna

* buscarPaisOptimo
	- Descripción: Recorre el las lista continente y pais para buscar e imprimir el pais con la menor densidad poblacional, estos datos son recibidos por el frontend.
	
	- Entradas:

		class(Grafica), intent(inout) :: this

	- Salida: ninguna

* generarGrafo
	- Descripción: Recorre el las lista continente y pais para generar el grafo utilizando Gra.
	
	- Entradas:

		class(Grafica), intent(inout) :: this

	- Salida: ninguna

## 🔵 Tabla de tokens y patrones

| Token            | Patrón                                              |
| ---------------- | --------------------------------------------------- |
| PALABRA_RESERADA | grafica                                             |
| PALABRA_RESERADA | nombre                                              |
| PALABRA_RESERADA | pais                                                |
| PALABRA_RESERADA | continente                                          |
| PALABRA_RESERADA | poblacion                                           |
| PALABRA_RESERADA | saturacion                                          |
| PALABRA_RESERADA | bandera                                             |
| CADENA           | Cualquier cadena de carácteres entre comillas.      |
| ENTERO           | [0-9]+                                              |
| PORCENTAJE       | %                                                   |
| PUNTO_COMA       | ;                                                   |
| DOS_PUNTOS       | :                                                   |
| LLAVE_ABRE       | {                                                   |
| LLAVE_CIERRA     | }                                                   |
| ERROR            | Carácter o palabra no reconocida por el análizador. |

## ⚫ Obtención DFA mínimo
La expresión regular que acepta los carácteres del lenguaje es: S+ | L+ | N+ | "C+"

Donde:

S: Separadores o simbolos : ; , { } %

L: Letras [a-z]

N: Números enteros [0-9]

C: Cualquier caracter excepto "
### ⬛ Agregar estado de aceptación
S+ | L+ | N+ | "C+"#

### ⬛ Dibujar árbol de expresión
* Numeración de hojas

* Aplicar funciones anulables a cada nodo

<image src='Proyecto1/docs/images/ARBOL1.jpg' alt='ábol de expresión'  caption='Árbol de la expresión S+ | L+ | N+ | "C+"# '>

* Calculo de anulables

* Calculo de primeros y últimos

<image src='Proyecto1/docs/images/ARBOL2.jpg' alt='ábol de expresión'  caption='Árbol de la expresión S+ | L+ | N+ | "C+"# '>

### ⬛ Tabla de siguientes

| i | Simbolo | sig(i) |
| - | ------- | ------ |
| 1 | S       | 1.7    |
| 2 | L       | 2.7    |
| 3 | N       | 3,7    |
| 4 | "       | 5      |
| 5 | C       | 5,6    |
| 6 | "       | 7      |
| 7 | #       |   --   |

### ⬛ Reducción del autómata
**S0 = {1,2, 3, 4}**

Sig(1) = {1, 7} = S1

Sig(2) = {2, 7} = S2

Sig(3) = {3, 7} = S3

Sig(4) = {5} = S4

**S4 = {5}**

Sig(5) = {5, 6} = S5

**S5 = {5, 6}**

Sig(6) = {7} = S6

**S6 = {7}**

Sig(7)= #

### ⬛ Autómata

<image src="Proyecto1/docs/images/AFD.jpg" alt="árbol de expresión" caption="Autómata mínimo">
