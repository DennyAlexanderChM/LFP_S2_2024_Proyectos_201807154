module ModuloAnalizador
    
    use ModuloToken
    use ModuloError
    implicit none
    private :: isalpha, isdigit, reserver_word, agregarToken
    public :: analizarCadena
    integer :: columna, fila, estado, columnaLexema, typeLexema
    character(len=:), allocatable :: lexema
    
contains
    subroutine analizarCadena(cadena, lista_tokens)
        type(Token), dimension(:), allocatable :: lista_tokens
        character(len=:), intent(in), allocatable :: cadena
        character(len=:), allocatable  :: cadenaNueva
        character(len = :), allocatable :: comentario_error
        integer :: posicion
        character :: actual
        ! Inicializamos variables
        columna = 1
        columnaLexema = 1
        estado = 0
        fila = 1
        lexema = ''
        posicion = 1
        actual = ''
        cadenaNueva = TRIM(cadena) // '&' ! Fin de cadena

        do while ( posicion <= LEN_TRIM(cadenaNueva)) ! Recorremos la cadena recibida

            actual = cadenaNueva(posicion:posicion) ! Caracter actual

            if ( estado == 0 ) then
                if ( isalpha(actual) ) then ! Verifica si el caracter es alfabetico [a-zA-Z]
                    estado = 2 ! Estado 2 caracter alfabetico
                    lexema = lexema // actual
                    columnaLexema = columna
                    columna = columna + 1
                    posicion = posicion + 1

                else if ( isdigit(actual) ) then ! Verifica si el caracter es numerico [0-9]
                    estado = 3 ! Estado 3 enteros
                    lexema = lexema // actual
                    columnaLexema = columna
                    columna = columna + 1
                    posicion = posicion + 1

                else if (actual == '"') then ! Verifica si es inicio de una cadena
                    estado = 4 ! Estado 4 incio de cadena
                    lexema = lexema //actual
                    columnaLexema = columna
                    columna = columna + 1
                    posicion = posicion + 1

                else if (actual == '/') then ! Verifica si es inicio de un Comentario
                    estado = 5 ! Estado 5 incio de comentario
                    lexema = lexema // actual
                    columnaLexema = columna
                    columna = columna + 1
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

                    estado = 1 ! Estado 1 simbolos o separadores
                    lexema = lexema // actual
                    columnaLexema = columna

                else if (actual == CHAR(10)) then ! Salto de línea
                    estado = 0 
                    fila = fila + 1
                    columna = 1
                    lexema = ''
                    posicion = posicion + 1
                ! char(9) tabulaciones char(13) retorno de carro
                else if (actual == ' ' .OR. actual == CHAR(9) .OR. actual == CHAR(13)) then 
                    estado = 0
                    lexema = ''
                    columna =  columna + 1
                    posicion = posicion + 1
                
                else if (actual == '&' .AND. posicion == LEN_TRIM(cadenaNueva) ) then ! Fin de cadena
                    exit

                else ! Caracter sin reconocer
                    comentario_error = 'Caracter no reconocido'
                    typeLexema = ERROR
                    lexema = lexema // actual
                    columnaLexema = columna
                    call errores%agregar_error_lexico(comentario_error, lexema, fila, columnaLexema)
                    call agregarToken(lista_tokens)
                    columna =  columna + 1
                    posicion = posicion + 1
                    
                end if
            else if(estado == 1) then
                if ( actual == '!' ) then
                    typeLexema = SIGNO_EXCLAMACION
                else if ( actual == '<' ) then
                    typeLexema = SIGNO_MENOR
                else if ( actual == '>' ) then
                    typeLexema = SIGNO_MAYOR
                else if ( actual == ';' ) then
                    typeLexema = SIGNO_PUNTO_COMA
                else if ( actual == '-' ) then
                    typeLexema = SIGNO_GUION
                else if ( actual == '.' ) then
                    typeLexema = SIGNO_PUNTO
                else if ( actual == ',' ) then
                    typeLexema = SIGNO_COMA
                else if ( actual == '(' ) then
                    typeLexema = PARENTESIS_ABRE
                else if ( actual == ')' ) then
                    typeLexema = PARENTESIS_CIERRA
                end if
                call agregarToken(lista_tokens)
                columna =  columna + 1
                posicion = posicion + 1
                
            ! -------------------------------------------------
            ! Maneja el estado 2 (Letras)
            else if ( estado == 2 ) then 
                if ( isalpha(actual) ) then ! Verifica sin son letras
                    estado = 2
                    lexema = lexema // actual
                    columna = columna + 1
                    posicion = posicion + 1
                else if ( isdigit(actual) .OR. actual == '_') then
                    estado = 6
                    lexema = lexema // actual
                    columna = columna + 1
                    posicion = posicion + 1
                else
                    call reserver_word()
                    call agregarToken(lista_tokens)
                end if
            else if ( estado == 6 ) then
                if ( isalpha(actual) .OR. isdigit(actual) .OR. actual == '_') then
                    estado = 6 ! Estado 6
                    lexema = lexema // actual
                    columna = columna + 1
                    posicion = posicion + 1
                else                    
                    typeLexema = IDENTIFICADOR
                    call agregarToken(lista_tokens)
                end if

            else if ( estado == 3 ) then ! Maneja numeros enteros
                if ( isdigit(actual) ) then
                    estado = 3
                    lexema = lexema // actual
                    columna = columna + 1
                    posicion = posicion + 1
                else
                    typeLexema = ENTERO
                    call agregarToken(lista_tokens)
                end if

            else if ( estado == 4 ) then ! Maneja cadenas entre "Cualquier caracter excepto "
                if ( actual /= '"' ) then
                    if ( actual /= CHAR(10) ) then
                        estado = 4
                        lexema = lexema // actual
                        columna = columna + 1
                        posicion = posicion + 1
                    else
                        comentario_error = 'CIERRE DE COMENTARIO'
                        typeLexema = ERROR
                        call errores%agregar_error_lexico(comentario_error, lexema, fila, columnaLexema)
                        call agregarToken(lista_tokens)
                        
                    end if
                    
                else
                    estado = 7 ! Estado de aceptación
                    typeLexema = RESERVADA_CADENA
                    lexema = lexema // actual
                    call agregarToken(lista_tokens)
                    columna = columna + 1
                    posicion = posicion + 1
                end if

            else if ( estado == 5 ) then 
                if ( actual == '*') then
                    estado = 8
                    lexema = lexema // actual
                    columna = columna + 1
                    posicion = posicion + 1
                else if ( actual == '/' ) then
                    estado = 9
                    lexema = lexema // actual
                    columna = columna + 1
                    posicion = posicion + 1
                else
                    comentario_error = 'CIERRE DE COMENTARIO'
                    typeLexema = ERROR
                    call errores%agregar_error_lexico(comentario_error, lexema, fila, columnaLexema)
                    call agregarToken(lista_tokens)
                    
                end if

            else if ( estado == 8 ) then 
                if ( actual /= '*') then
                    if ( actual == CHAR(10) ) then
                        estado = 8
                        fila = fila + 1
                        lexema = lexema // ' '
                        columna = columna + 1
                        posicion = posicion + 1

                    else
                        estado = 8
                        lexema = lexema // actual
                        columna = columna + 1
                        posicion = posicion + 1
                    end if
                    
                else 
                    estado = 10
                    lexema = lexema // actual
                    columna = columna + 1
                    posicion = posicion + 1
                end if

            else if ( estado == 9 ) then 
                if ( actual /= CHAR(10)) then
                    estado = 9
                    lexema = lexema // actual
                    columna = columna + 1
                    posicion = posicion + 1
                else 
                    estado = 7 ! Estado de aceptación
                    typeLexema = COMENTARIO
                    call agregarToken(lista_tokens)
                end if

            else if ( estado == 10 ) then 
                if ( actual == '/') then
                    estado = 7
                    lexema = lexema // actual
                    typeLexema = COMENTARIO
                    call agregarToken(lista_tokens)
                    columna = columna + 1
                    posicion = posicion + 1
                else 
                    comentario_error = 'Caracter no reconocido'
                    typeLexema = ERROR
                    lexema = lexema // actual
                    columnaLexema = columna
                    call errores%agregar_error_lexico(comentario_error, lexema, fila, columnaLexema)
                    call agregarToken(lista_tokens)
                    columna =  columna + 1
                    posicion = posicion + 1
                end if

            end if

        end do
        
    end subroutine analizarCadena

    subroutine agregarToken(lista_tokens) ! Agrega el Token a la lista
        type(Token), dimension(:), allocatable :: lista_tokens        
        type(Token), allocatable :: temp(:) ! Lista temporal de dimensión indefinida
        integer :: i

        if ( allocated(lista_tokens) ) then ! Verifica si la lista tiene dimension definida
            i = SIZE(lista_tokens) + 1 ! Obtenemos su dimension + 1
            allocate(temp(i-1)) ! Definimos la dimension de la lista temporal
            temp = lista_tokens  ! Copiamos la lista de tokens a la lista temporal
            deallocate(lista_tokens ) ! Liberamos el espacio en memoria de la lista de tokens
            allocate(lista_tokens(i)) ! Asignamos la nueva dimensión de la lista de tokens
            lista_tokens (1:i-1) = temp ! Copiamos los datos almacenados en lista temporal a la lista de tokens
            deallocate(temp) ! Liberamos el espacio en memoria de la lista temporal
        else
            i = 1 ! Si la lista no tiene ningun elemento
            allocate(lista_tokens (i))
        end if
        ! Asignamos calores al elemento de la lista
        lista_tokens(i)%lexema = lexema
        lista_tokens(i)%type = typeLexema
        lista_tokens(i)%position_x = columnaLexema
        lista_tokens(i)%position_y = fila
        ! Reiniciamos los datos del lexema y el estado
        lexema = ''
        estado = 0
        
    end subroutine agregarToken

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

    subroutine reserver_word()

        if (trim(lexema) == 'Controles') then
            typeLexema = RESERVADA_CONTROLES
        else if ( trim(lexema) == 'Propiedades' ) then
            typeLexema = RESERVADA_PROPIEDADES
        else if ( trim(lexema) == 'Colocacion' ) then
            typeLexema = RESERVADA_COLOCACION
        else if ( trim(lexema) == 'add' ) then
            typeLexema = RESERVADA_ADD
        else if ( trim(lexema) == 'this' ) then
            typeLexema = RESERVADA_THIS
        else if (trim(lexema) == 'Etiqueta') then
            typeLexema = RESERVADA_ETIQUETA
        else if ( trim(lexema) == 'Boton' ) then
            typeLexema = RESERVADA_BOTON
        else if ( trim(lexema) == 'Check' ) then
            typeLexema = RESERVADA_CHECK
        else if ( trim(lexema) == 'RadioBoton' ) then
            typeLexema = RESERVADA_RADIOBOTON
        else if ( trim(lexema) == 'Texto' ) then
            typeLexema = RESERVADA_TEXTO
        else if ( trim(lexema) == 'AreaTexto' ) then
            typeLexema = RESERVADA_AREATEXTO
        else if ( trim(lexema) == 'Clave' ) then
            typeLexema = RESERVADA_CLAVE
        else if ( trim(lexema) == 'Contenedor' ) then
            typeLexema = RESERVADA_CONTENEDOR
        else if (trim(lexema) == 'setColorLetra') then
            typeLexema = RESERVADA_PROPIEDAD_COLOR_LETRA
        else if ( trim(lexema) == 'setTexto' ) then
            typeLexema = RESERVADA_PROPIEDAD_TEXTO
        else if ( trim(lexema) == 'setAlineacion' ) then
            typeLexema = RESERVADA_PROPIEDAD_ALINEACION
        else if ( trim(lexema) == 'setColorFondo' ) then
            typeLexema = RESERVADA_PROPIEDAD_COLOR_FONDO
        else if ( trim(lexema) == 'setMarcada' ) then
            typeLexema = RESERVADA_PROPIEDAD_MARCADA
        else if ( trim(lexema) == 'setGrupo' ) then
            typeLexema = RESERVADA_PROPIEDAD_GRUPO
        else if ( trim(lexema) == 'setAncho' ) then
            typeLexema = RESERVADA_PROPIEDAD_ANCHO
        else if ( trim(lexema) == 'setAlto' ) then
            typeLexema = RESERVADA_PROPIEDAD_ALTO
        else if ( trim(lexema) == 'setPosicion' ) then
            typeLexema = RESERVADA_PROPIEDAD_POSICION
        else if ( trim(lexema) == 'true' ) then
            typeLexema = RESERVADA_TRUE
        else if ( trim(lexema) == 'false' ) then
            typeLexema = RESERVADA_FALSE
        else if ( trim(lexema) == 'izquierdo' ) then
            typeLexema = RESERVADA_IZQUIERDA
        else if ( trim(lexema) == 'centro' ) then
            typeLexema = RESERVADA_CENTRO
        else if ( trim(lexema) == 'derecho' ) then
            typeLexema = RESERVADA_DERECHA
        else
            typeLexema = IDENTIFICADOR
        end if
        
    end subroutine reserver_word

end module ModuloAnalizador