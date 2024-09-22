module ModuloAnalizador
    use ModuloGrafica
    implicit none
    private :: isalpha, isdigit, Token
    public :: Analizador, analizarCadena
    
    type ::Token ! Type Token
        character(len = 100) :: lexema
        character(len = 100) :: type
        integer :: position_x
        integer :: position_y

    end type Token

    type :: Analizador
        ! Declaramos las variables del type Analizador
        type(Token), dimension(:), allocatable :: listaTokens
        integer :: columna, fila, estado, columnaLexema! Posiciön "X" | "Y"
        character(len=:), allocatable :: lexema
        character(len=100) :: typeLexema
        logical :: correcto = .TRUE.

    contains ! Procedimientos del type Analizador 
        procedure :: analizarCadena ! Recibe la cadena de caracteres para análizar su contenido
        ! procedure :: printTokens (Solo si se ejecuta el main.f90 directamente)
        procedure :: guardarDatos ! Recibe guarda los tokens en sub types
        procedure :: reporteTokens ! Genera el archivo HTML (Solo si no se encontraron errores)
        procedure :: reporteError ! Genera el archivo HTML de errores (Solo si se encontraron errores)
        ! Procedimientos privades
        procedure, private :: agregarToken ! Agrega el token a la lista
        procedure, private :: palabraReservada ! Verifica si el Lexema es una palabra reservada
    
    end type Analizador
contains
    subroutine analizarCadena(this, cadena, fila)
        ! Declaracion de variables
        class(Analizador), intent(inout) :: this
        character(len=200), intent(in) :: cadena
        integer, intent(in) :: fila
        character(len=:), allocatable  :: cadenaNueva
        ! Inicializamos variables
        character :: actual = ''
        this%columna = 1
        this%columnaLexema = 1
        this%estado = 0
        this%fila = fila
        this%lexema = ''
        cadenaNueva = TRIM(cadena) // '#' ! Fin de cadena

        do while (this%columna <= LEN_TRIM(cadenaNueva)) ! Recorremos la cadena recibida
            actual = cadenaNueva(this%columna:this%columna) ! Caracter actual

            if ( this%estado == 0 ) then ! Si el estado es cero
                if ( isalpha(actual) ) then ! Verifica si el caracter es alfabetico [A-Za-z]
                    this%estado = 2 ! Estado 2 cadenas
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna
                    this%columna = this%columna + 1
                
                else if ( isdigit(actual) ) then ! Verifica si el caracter es numerico [0-9]
                    this%estado = 3 ! Estado 3 enteros
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna
                    this%columna = this%columna + 1

                else if (actual == '"') then ! Verifica si es inicio de una cadena
                    this%estado = 4 ! Estado 4 incio de cadena
                    this%lexema = this%lexema //actual
                    this%columnaLexema = this%columna
                    this%columna = this%columna + 1

                else if (actual == ':') then ! Dos puntos
                    this%estado = 1 ! Estado 1 simbolos o separadores
                    this%typeLexema = 'DOS_PUNTOS'
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna
                    call this%agregarToken()
                    this%columna =  this%columna + 1

                else if (actual == '{') then ! Llave abre
                    this%estado = 1 ! Estado 1 simbolos o separadores
                    this%typeLexema = 'LLAVE_ABRE'
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna
                    call this%agregarToken()
                    this%columna =  this%columna + 1
                
                else if (actual == '}') then ! Llave cierra
                    this%estado = 1 ! Estado 1 simbolos o separadores
                    this%typeLexema = 'LLAVE_CIERRA'
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna
                    call this%agregarToken()
                    this%columna = this%columna + 1
                
                else if (actual == ';') then ! Punto y coma
                    this%estado = 1 ! Estado 1 simbolos o separadores
                    this%typeLexema = 'PUNTO_COMA'
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna
                    call this%agregarToken()
                    this%columna =  this%columna + 1

                else if (actual == '%') then ! Punto y coma
                    this%estado = 1 ! Estado 1 simbolos o separadores
                    this%typeLexema = 'PORCENTAJE'
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna
                    call this%agregarToken()
                    this%columna =  this%columna + 1
                
                else if (actual == '#' .AND. this%columna == LEN_TRIM(cadenaNueva) ) then ! Fin de cadena
                    exit
                    ! char(9) tabulaciones, char(12) salto de pagina, char(13) retorno de carro
                else if (actual == ' ' .OR. actual == CHAR(9) .OR. actual == CHAR(12) .OR. actual == CHAR(13)) then 
                    this%estado = 0
                    this%lexema = ''
                    this%columna =  this%columna + 1

                else ! Caracter sin reconocer
                    this%typeLexema = 'ERROR' 
                    this%lexema = this%lexema // actual
                    this%correcto = .FALSE.
                    this%columnaLexema = this%columna
                    call this%agregarToken()
                    this%columna =  this%columna + 1
                    
                end if
            ! -------------------------------------------------
            ! Maneja el estado 2 (Letras)
            else if ( this%estado == 2 ) then 
                if ( isalpha(actual) ) then ! Verifica sin son letras
                    this%estado = 2 ! Estado 2 letras
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1    
                
                else
                    ! Si el caracter actual no es una letra se verifica si el lexema es una palabra reservada
                    call this%palabraReservada()
                end if
            ! -------------------------------------------------
            ! Maneja el estado 3 (Enteros)
            else if ( this%estado == 3 ) then
                if ( isdigit(actual) ) then
                    this%estado = 3 ! Estado 3 (Enteros)
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
            
                else
                    this%typeLexema = 'ENTERO'
                    call this%agregarToken()

                end if
            ! -------------------------------------------------
            ! Maneja el estado 4 (Cadena de caracteres)
            else if ( this%estado == 4 ) then
                if ( actual /= '"' ) then ! Verifica si el caracter actual es diferente de "
                    this%estado = 5 ! Estado 5 (caracter diferente de ")
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    
                else ! En caso de cadena vacia guardar error
                    this%typeLexema = 'ERROR'
                    this%lexema = this%lexema // actual
                    this%correcto = .FALSE.
                    this%columnaLexema = this%columna
                    call this%agregarToken()
                    this%columna =  this%columna + 1
                end if
            ! -------------------------------------------------
            ! Maneja el estado 5 (Cualquier caracter excepto ")
            else if ( this%estado == 5 ) then
                if ( actual /= '"' ) then
                    this%estado = 5 ! Estado 5 (Cualquier caracter excepto ")
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    
                else
                    this%estado = 6 ! Estado 6 (Aceptación de cadena)
                    this%typeLexema = 'CADENA'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%columna = this%columna + 1
                end if

            end if

        end do
        
    end subroutine analizarCadena

    function isalpha(str) result(is_alpha) ! Verifica si el caracter recibido es una letra
        character, intent(in) :: str
        logical :: is_alpha

        is_alpha = .FALSE.

        if ((ICHAR(str) >= ICHAR('A')) .AND. (ICHAR(str) <= ICHAR('Z')) .OR. &
            (ICHAR(str) >= ICHAR('a') .AND. ICHAR(str) <= ICHAR('z'))) then
            is_alpha = .TRUE.

        end if
    
    end function isalpha

    function isdigit(str) result(is_digit) ! Verifica si el caracter recibido es un numero
        character, intent(in) :: str
        logical :: is_digit

        is_digit = .FALSE.

        if (ICHAR(str) >= ICHAR('0') .AND. ICHAR(str) <= ICHAR('9')) then
            is_digit = .TRUE.

        end if
    
    end function isdigit

    subroutine palabraReservada(this) ! Verifica si la cadena es una palabra reservada
        class(Analizador), intent(inout) :: this
        this%typeLexema = 'PALABRA_RESERVADA'
        if ( TRIM(this%lexema) == "grafica" ) then
            call this%agregarToken()
        else if ( TRIM(this%lexema) == "nombre" ) then
            call this%agregarToken()
        else if ( TRIM(this%lexema) == "continente" ) then
            call this%agregarToken()
        else if ( TRIM(this%lexema) == "pais" ) then
            call this%agregarToken()
        else if ( TRIM(this%lexema) == "poblacion" ) then
            call this%agregarToken()
        else if ( TRIM(this%lexema) == "saturacion" ) then
            call this%agregarToken()
        else if ( TRIM(this%lexema) == "bandera" ) then
            call this%agregarToken()
        else
            this%typeLexema = "ERROR"
            this%correcto = .FALSE.
            call this%agregarToken()
        end if
        
    end subroutine

    subroutine agregarToken(this) ! Agrega el Token a la lista
        class(Analizador), intent(inout) :: this
        type(Token), allocatable :: temp(:) ! Lista temporal de dimensión indefinida
        integer :: i

        if ( allocated(this%listaTokens) ) then ! Verifica si la lista tiene dimension definida
            i = SIZE(this%listaTokens) + 1 ! Obtenemos su dimension + 1
            allocate(temp(i-1)) ! Definimos la dimension de la lista temporal
            temp = this%listaTokens ! Copiamos la lista de tokens a la lista temporal
            deallocate(this%listaTokens) ! Liberamos el espacio en memoria de la lista de tokens
            allocate(this%listaTokens(i)) ! Asignamos la nueva dimensión de la lista de tokens
            this%listaTokens(1:i-1) = temp ! Copiamos los datos almacenados en lista temporal a la lista de tokens
            deallocate(temp) ! Liberamos el espacio en memoria de la lista temporal
        else
            i = 1 ! Si la lista no tiene ningun elemento
            allocate(this%listaTokens(i))
        end if
        ! Asignamos calores al elemento de la lista
        this%listaTokens(i)%lexema = this%lexema
        this%listaTokens(i)%type = this%typeLexema
        this%listaTokens(i)%position_x = this%columnaLexema
        this%listaTokens(i)%position_y = this%fila
        ! Reiniciamos los datos del lexema y el estado
        this%lexema = ''
        this%estado = 0
        
    end subroutine

    subroutine printTokens(this) ! Imprime la los tokens
        class(Analizador), intent(inout) :: this
        integer :: i

        do i = 1, SIZE(this%listaTokens)
            print *, 'Token: ' , this%listaTokens(i)%type , ' Lexema: ' , this%listaTokens(i)%lexema
        end do

    end subroutine 

    ! Guarda los elementos en el objeto grafica
    ! El objeto grafica almacenara los datos de los continente (Objetos continentes)
    ! Los Objetos Continentes contendran las listas de paises de cada continente
    subroutine guardarDatos(this, miGrafica) 
        class(Analizador), intent(inout) :: this
        type(Grafica), intent(inout) :: miGrafica
        character(len=100) :: bloque, elemento, nombreElemento, banderaPais
        integer :: i, poblacion, saturacion
        i = 1
        poblacion = 0
        saturacion = 0
        bloque = ''
        elemento = ''
        banderaPais = ''
        nombreElemento = ''

        do while (i <= SIZE(this%listaTokens))
            if ( TRIM(this%listaTokens(i)%type) == 'PALABRA_RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'grafica') then
                bloque = 'grafica'
            else if ( TRIM(this%listaTokens(i)%type) == 'PALABRA_RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'continente' ) then
                bloque = 'continente'
            else if ( TRIM(this%listaTokens(i)%type) == 'PALABRA_RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'pais' ) then
                bloque = 'pais'
            else if ( TRIM(this%listaTokens(i)%type) == 'PALABRA_RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'nombre' ) then
                elemento = 'nombre'
            else if ( TRIM(this%listaTokens(i)%type) == 'PALABRA_RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'poblacion' ) then
                elemento = 'poblacion'
            else if ( TRIM(this%listaTokens(i)%type) == 'PALABRA_RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'saturacion' ) then
                elemento = 'saturacion'
            else if ( TRIM(this%listaTokens(i)%type) == 'PALABRA_RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'bandera' ) then
                elemento = 'bandera'
            end if

            if ( TRIM(this%listaTokens(i)%type) == 'CADENA' .AND. elemento == 'nombre' ) then
                nombreElemento = this%listaTokens(i)%lexema
                elemento = ''
            else if ( TRIM(this%listaTokens(i)%type) == 'ENTERO' .AND. elemento == 'poblacion' ) then
                read(this%listaTokens(i)%lexema, *)  poblacion
                elemento = ''
            else if ( TRIM(this%listaTokens(i)%type) == 'ENTERO' .AND. elemento == 'saturacion' ) then
                read(this%listaTokens(i)%lexema, *)  saturacion
                elemento = ''
            else if ( TRIM(this%listaTokens(i)%type) == 'CADENA' .AND. elemento == 'bandera' ) then
                banderaPais = this%listaTokens(i)%lexema
                elemento = ''
            end if

            if ( TRIM(bloque) == 'grafica' .AND. nombreElemento /= '') then 
                miGrafica%nombreGrafica = nombreElemento ! Asigna el nombre de nuestra grafica
                nombreElemento = ''
            
            else if (TRIM(bloque) == 'continente' .AND. nombreElemento /= '') then
                call miGrafica%addContinente(nombreElemento) ! Añade el continente a la grafica
                nombreElemento = ''

            else if (TRIM(bloque) == 'pais' .AND. nombreElemento /= '' .AND. poblacion /= 0 &
                .AND. saturacion /= 0 .AND. banderaPais /= '') then
                    call miGrafica%addPaisContinente(nombreElemento, poblacion, saturacion, banderaPais) ! Añade el continente a la grafica
                    nombreElemento = ''
                    poblacion = 0
                    saturacion = 0
                    banderaPais = ''
            end if
            
            i = i + 1
        end do
        
    end subroutine guardarDatos
    
    subroutine reporteTokens(this) ! Crea el archivo html con los datos de los tokens
        class(Analizador), intent(inout) :: this
        integer :: i
        
        if ( SIZE(this%listaTokens) > 0 ) then

            ! Abrir el archivo para escribir (crea el archivo si no existe)
            open(unit=10, file="TOKENS.html", status="replace", action="write")
            write(10, *) "<!DOCTYPE html><html><head><style>"
            write(10, *) "#container{display: flex;}"
            write(10, *) "#customers {font-family: Arial, Helvetica, sans-serif; border-collapse: collapse; width: 90%;margin: auto; }"
            write(10, *) "#customers td,"
            write(10, *) "#customers th { border: 1px solid #ddd;padding: 8px; }"
            write(10, *) "#customers tr:nth-child(even) { background-color: #f2f2f2; }"
            write(10, *) "#customers tr:hover { background-color: #ddd; }"
            write(10, *) "#customers th { padding-top: 12px; padding-bottom: 12px; text-align: left; background-color: #04AA6D;color: white; }"
            write(10, *) "</style></head><body>"
            write(10, *) "<h1 style='text-align: center;'>Listado de Tokens</h1>"
            write(10, *) "<div id='container'><table id='customers'>"
            write(10, *) "<tr><th>No</th><th>Lexema</th><th>Tipo</th><th>Columna</th><th>Fila</th></tr>"

            do i = 1, SIZE(this%listaTokens)

               write(10, *) "<tr>"
               write(10, *) "<th>", i ,"</th>"
               write(10, *) "<th>", TRIM(this%listaTokens(i)%lexema) ,"</th>"
               write(10, *) "<th>", TRIM(this%listaTokens(i)%type) ,"</th>"
               write(10, *) "<th>", this%listaTokens(i)%position_x ,"</th>"
               write(10, *) "<th>", this%listaTokens(i)%position_y ,"</th>"
               write(10, *) "</tr>"
                        
            end do

            write(10, *) " </table></div></body></html>"
            
            close(10)
        end if
        
        
    end subroutine reporteTokens

    subroutine reporteError(this) ! Crea el archivo html con los datos de los tokens
        class(Analizador), intent(inout) :: this
        integer :: i, numTokens
        numTokens = 1
        
        if ( SIZE(this%listaTokens) > 0 ) then

            ! Abrir el archivo para escribir (crea el archivo si no existe)
            open(unit=10, file="ERRORES.html", status="replace", action="write")
            write(10, *) "<!DOCTYPE html><html><head><style>"
            write(10, *) "#container{display: flex;}"
            write(10, *) "#customers {font-family: Arial, Helvetica, sans-serif; border-collapse: collapse; width: 90%;margin: auto; }"
            write(10, *) "#customers td,"
            write(10, *) "#customers th { border: 1px solid #ddd;padding: 8px; }"
            write(10, *) "#customers tr:nth-child(even) { background-color: #f2f2f2; }"
            write(10, *) "#customers tr:hover { background-color: #ddd; }"
            write(10, *) "#customers th { padding-top: 12px; padding-bottom: 12px; text-align: left; background-color: #04AA6D;color: white; }"
            write(10, *) "</style></head><body>"
            write(10, *) "<h1 style='text-align: center;'>Lista de errores</h1>"
            write(10, *) "<div id='container'><table id='customers'>"
            write(10, *) "<tr><th>No</th><th>Lexema</th><th>Tipo</th><th>Fila</th><th>Columna</th></tr>"

            do i = 1, SIZE(this%listaTokens)

                if ( TRIM(this%listaTokens(i)%type) == 'ERROR' ) then
                    
                    write(10, *) "<tr>"
                    write(10, *) "<th>", numTokens ,"</th>"
                    write(10, *) "<th>", TRIM(this%listaTokens(i)%lexema) ,"</th>"
                    write(10, *) "<th>", TRIM(this%listaTokens(i)%type) ,"</th>"
                    write(10, *) "<th>", this%listaTokens(i)%position_y ,"</th>"
                    write(10, *) "<th>", this%listaTokens(i)%position_x ,"</th>"
                    write(10, *) "</tr>"

                    numTokens = numTokens + 1 
                        
                end if

            end do

            write(10, *) " </table></div></body></html>"
            
            close(10)
        end if
        
    end subroutine reporteError

end module ModuloAnalizador