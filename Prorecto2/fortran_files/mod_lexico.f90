module ModuloAnalizador
    !use ModuloGrafica
    use ModuloToken
    implicit none
    private :: isalpha, isdigit
    public :: Analizador, analizarCadena

    type :: Analizador
        ! Declaramos las variables del type Analizador
        type(Token), dimension(:), allocatable :: listaTokens
        integer :: columna, fila, estado, columnaLexema, typeLexema
        character(len=:), allocatable :: lexema
    contains
        procedure :: analizarCadena ! Recibe la cadena de caracteres para análizar su contenido
        procedure :: printTokens
        procedure :: reserver_word
        procedure, private :: agregarToken ! Agrega el token a la lista
    
    end type Analizador
contains
    subroutine analizarCadena(this, cadena)
        class(Analizador), intent(inout) :: this
        character(len=:), intent(in), allocatable :: cadena
        character(len=:), allocatable  :: cadenaNueva
        integer :: posicion
        character :: actual
        ! Inicializamos variables
        this%columna = 1
        this%columnaLexema = 1
        this%estado = 0
        this%fila = 1
        this%lexema = ''
        posicion = 1
        actual = ''
        cadenaNueva = TRIM(cadena) // '&' ! Fin de cadena

        do while ( posicion <= LEN_TRIM(cadenaNueva)) ! Recorremos la cadena recibida

            actual = cadenaNueva(posicion:posicion) ! Caracter actual

            if ( this%estado == 0 ) then
                if ( isalpha(actual) ) then ! Verifica si el caracter es alfabetico [a-zA-Z]
                    this%estado = 2 ! Estado 2 caracter alfabetico
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna
                    this%columna = this%columna + 1
                    posicion = posicion + 1

                else if ( isdigit(actual) ) then ! Verifica si el caracter es numerico [0-9]
                    this%estado = 3 ! Estado 3 enteros
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna
                    this%columna = this%columna + 1
                    posicion = posicion + 1

                else if (actual == '"') then ! Verifica si es inicio de una cadena
                    this%estado = 4 ! Estado 4 incio de cadena
                    this%lexema = this%lexema //actual
                    this%columnaLexema = this%columna
                    this%columna = this%columna + 1
                    posicion = posicion + 1

                else if (actual == '/') then ! Verifica si es inicio de un Comentario
                    this%estado = 5 ! Estado 5 incio de comentario
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna
                    this%columna = this%columna + 1
                    posicion = posicion + 1

                else if (actual == '<' .OR. &
                    actual == '>' .OR. &
                    actual == '!' .OR. &
                    actual == '-' .OR. &
                    actual == ';' .OR. &
                    actual == '.' .OR. &
                    actual == ',' .OR. &
                    actual == '(' .OR. &
                    actual ==')') then

                    this%estado = 1 ! Estado 1 simbolos o separadores
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna

                else if (actual == CHAR(10)) then ! Salto de línea
                    this%estado = 0 
                    this%fila = this%fila + 1
                    this%columna = 1
                    this%lexema = ''
                    posicion = posicion + 1
                ! char(9) tabulaciones char(13) retorno de carro
                else if (actual == ' ' .OR. actual == CHAR(9) .OR. actual == CHAR(13)) then 
                    this%estado = 0
                    this%lexema = ''
                    this%columna =  this%columna + 1
                    posicion = posicion + 1
                
                else if (actual == '&' .AND. this%columna == LEN_TRIM(cadenaNueva) ) then ! Fin de cadena
                    exit

                else ! Caracter sin reconocer 
                    this%typeLexema = ERROR
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna
                    call this%agregarToken()
                    this%columna =  this%columna + 1
                    posicion = posicion + 1
                    
                end if
            else if(this%estado == 1) then
                if ( actual == '!' ) then
                    this%typeLexema = SIGNO_EXCLAMACION
                else if ( actual == '<' ) then
                    this%typeLexema = SIGNO_MAYOR
                else if ( actual == '>' ) then
                    this%typeLexema = SIGNO_MENOR
                else if ( actual == ';' ) then
                    this%typeLexema = SIGNO_PUNTO_COMA
                else if ( actual == '-' ) then
                    this%typeLexema = SIGNO_GUION
                else if ( actual == '.' ) then
                    this%typeLexema = SIGNO_PUNTO
                else if ( actual == ',' ) then
                    this%typeLexema = SIGNO_COMA
                else if ( actual == '(' ) then
                    this%typeLexema = PARENTESIS_ABRE
                else if ( actual == ')' ) then
                    this%typeLexema = PARENTESIS_CIERRA
                end if
                call this%agregarToken()
                this%columna =  this%columna + 1
                posicion = posicion + 1
                
            ! -------------------------------------------------
            ! Maneja el estado 2 (Letras)
            else if ( this%estado == 2 ) then 
                if ( isalpha(actual) ) then ! Verifica sin son letras
                    this%estado = 2
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    posicion = posicion + 1
                else if ( isdigit(actual) .OR. actual == '_') then
                    this%estado = 6
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    posicion = posicion + 1
                else
                    call this%reserver_word()
                    call this%agregarToken()
                end if
            else if ( this%estado == 6 ) then
                if ( isalpha(actual) .OR. isdigit(actual) .OR. actual == '_') then
                    this%estado = 6 ! Estado 6
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    posicion = posicion + 1
                else                    
                    this%typeLexema = IDENTIFICADOR
                    call this%agregarToken()
                end if

            else if ( this%estado == 3 ) then ! Maneja numeros enteros
                if ( isdigit(actual) ) then
                    this%estado = 3
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    posicion = posicion + 1
                else
                    this%typeLexema = ENTERO
                    call this%agregarToken()
                end if

            else if ( this%estado == 4 ) then ! Maneja cadenas entre "Cualquier caracter excepto "
                if ( actual /= '"' ) then
                    this%estado = 4
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    posicion = posicion + 1
                else
                    this%estado = 7 ! Estado de aceptación
                    this%typeLexema = RESERVADA_CADENA
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%columna = this%columna + 1
                    posicion = posicion + 1
                end if

            else if ( this%estado == 5 ) then 
                if ( actual == '*') then
                    this%estado = 8
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    posicion = posicion + 1
                else if ( actual == '/' ) then
                    this%estado = 9
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    posicion = posicion + 1
                else
                    this%typeLexema = ERROR
                    call this%agregarToken()
                    
                end if

            else if ( this%estado == 8 ) then 
                if ( actual /= '*') then
                    if ( actual == CHAR(10) ) this%fila = this%fila + 1
                    this%estado = 8
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    posicion = posicion + 1
                else 
                    this%estado = 10
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    posicion = posicion + 1
                end if

            else if ( this%estado == 9 ) then 
                if ( actual /= CHAR(10)) then
                    this%estado = 9
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    posicion = posicion + 1
                else 
                    this%estado = 7 ! Estado de aceptación
                    this%typeLexema = COMENTARIO
                    call this%agregarToken()
                end if

            else if ( this%estado == 10 ) then 
                if ( actual == '/') then
                    this%estado = 7
                    this%lexema = this%lexema // actual
                    this%typeLexema = COMENTARIO
                    call this%agregarToken()
                    this%columna = this%columna + 1
                    posicion = posicion + 1
                else 
                    this%typeLexema = ERROR
                    this%lexema = this%lexema // actual
                    this%columnaLexema = this%columna
                    call this%agregarToken()
                    this%columna =  this%columna + 1
                    posicion = posicion + 1
                end if

            end if

        end do
        
    end subroutine analizarCadena

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
            print *, 'Token: ' , this%listaTokens(i)%type , ' Lexema: ' , TRIM(this%listaTokens(i)%lexema), this%listaTokens(i)%position_x
        end do

    end subroutine

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

    subroutine reserver_word(this)
        class(Analizador), intent(inout) :: this
        if (trim(this%lexema) == 'Controles') then
            this%typeLexema = RESERVADA_CONTROLES
        else if ( trim(this%lexema) == 'Propiedades' ) then
            this%typeLexema = RESERVADA_PROPIEDADES
        else if ( trim(this%lexema) == 'Colocacion' ) then
            this%typeLexema = RESERVADA_COLOCACION
        else if ( trim(this%lexema) == 'add' ) then
            this%typeLexema = RESERVADA_ADD
        else if ( trim(this%lexema) == 'this' ) then
            this%typeLexema = RESERVADA_THIS
        else if (trim(this%lexema) == 'Etiqueta') then
            this%typeLexema = RESERVADA_ETIQUETA
        else if ( trim(this%lexema) == 'Boton' ) then
            this%typeLexema = RESERVADA_BOTON
        else if ( trim(this%lexema) == 'Check' ) then
            this%typeLexema = RESERVADA_CHECK
        else if ( trim(this%lexema) == 'RadioBoton' ) then
            this%typeLexema = RESERVADA_RADIOBOTON
        else if ( trim(this%lexema) == 'Texto' ) then
            this%typeLexema = RESERVADA_TEXTO
        else if ( trim(this%lexema) == 'AreaTexto' ) then
            this%typeLexema = RESERVADA_AREATEXTO
        else if ( trim(this%lexema) == 'Clave' ) then
            this%typeLexema = RESERVADA_CLAVE
        else if ( trim(this%lexema) == 'Contenedor' ) then
            this%typeLexema = RESERVADA_CONTENEDOR
        else if (trim(this%lexema) == 'setColorLetra') then
            this%typeLexema = RESERVADA_PROPIEDAD_COLOR_LETRA
        else if ( trim(this%lexema) == 'setTexto' ) then
            this%typeLexema = RESERVADA_PROPIEDAD_TEXTO
        else if ( trim(this%lexema) == 'setAlineacion' ) then
            this%typeLexema = RESERVADA_PROPIEDAD_ALINEACION
        else if ( trim(this%lexema) == 'setColorFondo' ) then
            this%typeLexema = RESERVADA_PROPIEDAD_COLOR_FONDO
        else if ( trim(this%lexema) == 'setMarcada' ) then
            this%typeLexema = RESERVADA_PROPIEDAD_MARCADA
        else if ( trim(this%lexema) == 'setGrupo' ) then
            this%typeLexema = RESERVADA_PROPIEDAD_GRUPO
        else if ( trim(this%lexema) == 'setAncho' ) then
            this%typeLexema = RESERVADA_PROPIEDAD_ANCHO
        else if ( trim(this%lexema) == 'setAlto' ) then
            this%typeLexema = RESERVADA_PROPIEDAD_ALTO
        else if ( trim(this%lexema) == 'setPosicion' ) then
            this%typeLexema = RESERVADA_PROPIEDAD_POSICION
        else if ( trim(this%lexema) == 'true' ) then
            this%typeLexema = RESERVADA_TRUE
        else if ( trim(this%lexema) == 'false' ) then
            this%typeLexema = RESERVADA_FALSE
        else if ( trim(this%lexema) == 'izquierdo' ) then
            this%typeLexema = RESERVADA_IZQUIERDA
        else if ( trim(this%lexema) == 'centro' ) then
            this%typeLexema = RESERVADA_CENTRO
        else if ( trim(this%lexema) == 'derecho' ) then
            this%typeLexema = RESERVADA_DERECHA
        else
            this%typeLexema = IDENTIFICADOR
        end if
        
    end subroutine reserver_word

end module ModuloAnalizador