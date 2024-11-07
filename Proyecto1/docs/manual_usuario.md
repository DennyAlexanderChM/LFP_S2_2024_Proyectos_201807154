# Lenguajes Formales de programación 

## Manual de Usuario Proyecto 1 (índice)
* ⚪ Introducción
* 🟡 Requerimientos del sistema
	- 🟨  Interprete
    - 🟨  Dependencias
* 🟢 Interfaz gráfica
	- 🟩 Pantalla Principal
	- 🟩 Mensajes Emergentes
* ⚫ Reportes de errores y tokens encontrados
	- ⬛ Reportes de tokens
	- ⬛ Reportes de errores


## ⚪ Introducción
Se desarrollo una aplicación que visualiza el contenido de un archivo con extención **.ORG**, el archivo contiene una serie de lugares, su ubicaion geográfica y un porcentaje de saturacion en el mercado de dicho lugar. La principal función de la aplicacion es la de encontrar el lugar con el menor porcentaje de saturacion y visualizar los datos contenidos en el archivo **.ORG**.

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

## 🟡 Requerimientos del sistema
Para el funcionamiento del programa se recomienda tener instalado en el equipo las siguientes dependencias:

### 🟨 Interprete
Interprete: Python

Versión: 3.12.4

### 🟨 Dependencias o librerias (Frontend)
Libreria: Pillow

Versión: Gfortran 10.4.0

Software: Graphviz

Versión: 12.1.1

## 🟢 Interfaz Gráfica
### 🟩 Pantalla Principal
Al iniciar el sistema se muestra la siguiente ventana mostrando los elementos gráficos del sistema, cuenta con un area de edición de texto, un botón de análisis y la barra de mavegación. Ademas de contar con una area que permitirá visualizar los datos obtenidos de archivo de entrada.

<image src="images/INICIO.png" alt="Incio" caption="Pantalla de inicio">

#### Editor de texto
Área de texto que permite el ingreso de instrucciones de entrada o bien la visualización de archivos cargados al sistema.

#### Menú
* Abrir: abre una venta emergente que permite la busqueda de los archivos de entrada con extención **.ORG**.
* Guardar: guarda los cambios realizados en el campo de texto, si el archio no ha sido guardado en proporciona de guardar el archivo de forma local.
* Guardar como: permite guardar el archivo en una ruta y nombre específicas.
* Acerca de: muestras los datos del desarrollador.
* Salir: finaliza la ejecución del programa.

#### Botón Analizar
Realiza el análisis del contenido en el área de texto. Debe de guardar primero los cambios efectuados en el campo de texto.

#### Area gráfica
Mustra la gráfica generada a partir de la entrada obtenida en el campo de texto.

#### Área de selección
Muestra el país seleccionado y su bandera.

**En la siguiente imagen se muestra un ejemplo de la aplicación luego de realizar correctamente el análisis del contendido del campo de texto.**

<image src="images/VENTANA.png" alt="Ventana" caption="Pantalla de inicio">

### 🟩 Mensajes emergentes
Se muestran al realizar alguna acción.
#### Mensaje de Error
Se muestra al encotrar un error al análisas la entrada de texto

<image src="images/ERR.png" alt="error" caption="Mensaje Error">

### Mensaje de Aviso
Se muestra al finalizar el análisis de la entrada de texto.

<image src="images/INFO.png" alt="Info" caption="Mensaje Info">

## ⚫ Reportes de errores y tokens encontrados
### ⬛ Reportes de tokens
Si el análisis de la entrada se ralizó correctamente la plicación genera un archivo en fortato HTML, el archivo contiene la información de cada uno de los elementos que conforman el texto análizado.

**Ejemplo del reporte de tokens**

<image src="images/TOKENS.png" alt="tokens" caption="Reporte de tokens">

### ⬛ Reportes de errores
En caso de encotrar errores léxicos la aplicación generará un reporte en formato HTML el cual muestra el error y su posición.

**Ejemplo del reporte de errores**

<image src="images/ERROR.png" alt="tokens" caption="Reporte de tokens">

---

*Los reportes puden ser abiertos en cualquier navegador*

