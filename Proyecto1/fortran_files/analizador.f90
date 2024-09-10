module AnalizadorModulo
    use TokenModule
    use GraficaModulo
    implicit none
    private :: isalpha, isdigit
    public :: Analizador, analizarCadena
    type :: Analizador
        type(Token), dimension(:), allocatable :: listaTokens
        integer :: columna, fila, estado! Posiciön "X" | "Y"
        character(len=:), allocatable :: lexema
        character(len=100) :: typeLexema

    contains
        procedure :: analizarCadena
        procedure :: printTokens
        procedure :: guardarDatos
        procedure :: reporteTokens
        procedure, private :: agregarToken
        procedure, private :: palabraReservada
    
    end type Analizador
contains
    subroutine analizarCadena(this, cadena, fila)
        class(Analizador), intent(inout) :: this
        character(len=200), intent(in) :: cadena
        integer, intent(in) :: fila
        character(len=:), allocatable  :: cadenaNueva
        ! Inicializamos variables
        character :: actual = ''
        this%columna = 1
        this%estado = 0
        this%fila = fila
        this%lexema = ''
        cadenaNueva = TRIM(cadena) // '#' ! Fin de cadena

        do while (this%columna <= LEN(cadenaNueva))
            actual = cadena(this%columna:this%columna)

            if ( this%estado == 0 ) then ! Si el estado es cero
                if ( isalpha(actual) ) then ! Verifica si el caracter es un caracter alfabetico
                    this%estado = 1
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                
                else if ( isdigit(actual) ) then ! Verifica si el caracter es numerico
                    this%estado = 3
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1

                else if (actual == '"') then ! Verifica si es inicio de una cadena
                    this%estado = 2
                    this%lexema = this%lexema //actual
                    this%columna = this%columna + 1

                else if (actual == ':') then ! Dos puntos
                    this%typeLexema = 'DOS PUNTOS'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%columna =  this%columna + 1

                else if (actual == '{') then ! Llave abre
                    this%typeLexema = 'LLAVE ABRE'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%columna =  this%columna + 1
                
                else if (actual == '}') then ! Llave cierra
                    this%typeLexema = 'LLAVE CIERRA'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%columna = this%columna + 1
                
                else if (actual == ';') then ! Punto y coma
                    this%typeLexema = 'PUNTO COMA'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%columna =  this%columna + 1

                else if (actual == '#') then ! Fin de cadena
                    this%estado = 0
                    exit

                else if (actual == ' ' .OR. actual == '\n' .OR. actual == '\r') then 
                    this%estado = 0
                    this%columna =  this%columna + 1
                
                else ! Caracter sin reconocer
                    this%typeLexema = 'ERROR'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%columna =  this%columna + 1
                    
                end if
            else if ( this%estado == 1 ) then ! Maneja el estado 1 (Palabras)
                if ( isalpha(actual) ) then ! Verifica sin son letras
                    this%estado = 1
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                
                else
                    call this%palabraReservada()
                    
                end if
            else if ( this%estado == 2 ) then ! Maneja el  estado 2 (Cadenas)
                if ( actual /= '"' ) then
                    this%estado = 2
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    
                else
                    this%typeLexema = 'CADENA'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%columna = this%columna + 1
                end if
                
            else if ( this%estado == 3 ) then !Maneja el estado 3 (Numeros)
                if ( isdigit(actual) ) then
                    this%estado = 3
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1

                else if ( actual == '%') then
                    this%typeLexema = 'PORCENTAJE'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%columna = this%columna + 1
                
                else
                    this%typeLexema = 'ENTERO'
                    call this%agregarToken()
                    
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
        this%typeLexema = 'PALABRA RESERVADA'
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
            call this%agregarToken()
        end if
        
    end subroutine

    subroutine agregarToken(this) ! Agrega el Token a la lista
        class(Analizador), intent(inout) :: this
        class(Token), allocatable :: temp(:)
        integer :: i

        if ( allocated(this%listaTokens) ) then
            i = SIZE(this%listaTokens) + 1
            allocate(temp(i-1))
            temp = this%listaTokens
            deallocate(this%listaTokens)
            allocate(this%listaTokens(i))
            this%listaTokens(1:i-1) = temp
            deallocate(temp)
        else
            i = 1
            allocate(this%listaTokens(i))
        end if

        this%listaTokens(i)%lexema = this%lexema
        this%listaTokens(i)%type = this%typeLexema
        this%listaTokens(i)%position_x = this%columna
        this%listaTokens(i)%position_y = this%fila

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
        class(Grafica), intent(inout) :: miGrafica
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
            if ( TRIM(this%listaTokens(i)%type) == 'PALABRA RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'grafica') then
                bloque = 'grafica'
            else if ( TRIM(this%listaTokens(i)%type) == 'PALABRA RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'continente' ) then
                bloque = 'continente'
            else if ( TRIM(this%listaTokens(i)%type) == 'PALABRA RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'pais' ) then
                bloque = 'pais'
            else if ( TRIM(this%listaTokens(i)%type) == 'PALABRA RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'nombre' ) then
                elemento = 'nombre'
            else if ( TRIM(this%listaTokens(i)%type) == 'PALABRA RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'poblacion' ) then
                elemento = 'poblacion'
            else if ( TRIM(this%listaTokens(i)%type) == 'PALABRA RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'saturacion' ) then
                elemento = 'saturacion'
            else if ( TRIM(this%listaTokens(i)%type) == 'PALABRA RESERVADA' .AND. TRIM(this%listaTokens(i)%lexema) == 'bandera' ) then
                elemento = 'bandera'
            end if

            if ( TRIM(this%listaTokens(i)%type) == 'CADENA' .AND. elemento == 'nombre' ) then
                nombreElemento = this%listaTokens(i)%lexema
                elemento = ''
            else if ( TRIM(this%listaTokens(i)%type) == 'ENTERO' .AND. elemento == 'poblacion' ) then
                read(this%listaTokens(i)%lexema, *)  poblacion
                elemento = ''
            else if ( TRIM(this%listaTokens(i)%type) == 'PORCENTAJE' .AND. elemento == 'saturacion' ) then
                elemento = obtenerSaturacion(this%listaTokens(i)%lexema)
                read(elemento, *)  saturacion
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

    function obtenerSaturacion(saturacion) result(porcentajeSaturacion)
        character(len = 100), intent(in) :: saturacion
        character(len = :), allocatable :: porcentajeSaturacion
        character :: actual
        integer :: i
        actual = ''

        ! Recorremos la cadena original carácter por carácter
        do i = 1, len_trim(saturacion)
            actual = saturacion(i:i)
      
            ! Si el carácter no es uno de los que queremos eliminar, lo añadimos a la cadena limpia
            if (isdigit(actual)) then
                porcentajeSaturacion = trim(porcentajeSaturacion) // actual
            end if
        end do
        
    end function obtenerSaturacion
    
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
            write(10, *) "<tr><th>No</th><th>Lexema</th><th>Tipo</th><th>Fila</th><th>Columna</th></tr>"

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

end module AnalizadorModulo