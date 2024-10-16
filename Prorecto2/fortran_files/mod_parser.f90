module ModuloAnalizadorSintactico
    use ModuloToken
    implicit none
    private
    public parser
    ! Variables del modulo
    type(Token), dimension(:), allocatable :: tokens
    integer :: num_pos
    type(Token) :: vistazo
    logical :: vacio
contains
    subroutine parser(listaTokens)
        type(Token), dimension(:), allocatable :: listaTokens
        tokens = listaTokens
        num_pos = 1
        vistazo = tokens(num_pos)
        vacio = .FALSE.
        call inicio()
        
    end subroutine parser

    subroutine inicio()
        call bloque_controles()
        call bloque_propiedades()
        call bloque_colocacion()
    end subroutine inicio

    subroutine bloque_controles()
        call match(SIGNO_MAYOR) ! <
        call match(SIGNO_EXCLAMACION) ! <
        call match(SIGNO_GUION) ! <
        call match(SIGNO_GUION)
        call match(RESERVADA_CONTROLES) ! CONTROLES
        call lista_controles()
        call match(RESERVADA_CONTROLES) ! CONTROLES
        call match(SIGNO_GUION) ! <
        call match(SIGNO_GUION) ! <
        call match(SIGNO_MENOR) ! <
    end subroutine bloque_controles

    recursive subroutine lista_controles()
        if ( .not. vacio ) then
            call elemento_control()
            call lista_controles()
        end if
        
    end subroutine lista_controles

    recursive subroutine elemento_control()
        if ( vistazo%type /= COMENTARIO )  then
            if ( vistazo%type /= RESERVADA_CONTROLES ) then
                call tipo_elemento_control()
                call match(IDENTIFICADOR)
                call match(SIGNO_PUNTO_COMA)
            else
                vacio = .TRUE.
            end if
            
        else
            num_pos = num_pos + 1
            vistazo = Tokens(num_pos)
        end if
    end subroutine elemento_control

    subroutine tipo_elemento_control()
        select case (vistazo%type)
            case (RESERVADA_ETIQUETA)
                call match(RESERVADA_ETIQUETA)
            case (RESERVADA_BOTON)
                call match(RESERVADA_BOTON)
            case (RESERVADA_CHECK)
                call match(RESERVADA_CHECK)
            case (RESERVADA_RADIOBOTON)
                call match(RESERVADA_RADIOBOTON)
            case (RESERVADA_TEXTO)
                call match(RESERVADA_TEXTO)
            case (RESERVADA_AREATEXTO)
                call match(RESERVADA_AREATEXTO)
            case (RESERVADA_CLAVE)
                call match(RESERVADA_CLAVE)
            case (RESERVADA_CONTENEDOR)
                call match(RESERVADA_CONTENEDOR)
            case default
                print *, "Error sintactico: Tipo de control no valido."
                call panicModeRecovery(RESERVADA_CONTENEDOR, IDENTIFICADOR)
            
        end select
        
    end subroutine tipo_elemento_control

    subroutine bloque_propiedades()
        call match(SIGNO_MAYOR) ! <
        call match(SIGNO_EXCLAMACION) ! <
        call match(SIGNO_GUION) ! <
        call match(SIGNO_GUION)
        call match(RESERVADA_PROPIEDADES) ! CONTROLES
        call lista_propiedades()
        call match(RESERVADA_PROPIEDADES) ! CONTROLES
        call match(SIGNO_GUION) ! <
        call match(SIGNO_GUION) ! <
        call match(SIGNO_MENOR) ! <
    end subroutine bloque_propiedades

    recursive subroutine lista_propiedades()
        if ( .not. vacio) then
            call elemento_propiedad()
            call lista_propiedades()
        end if
        
    end subroutine lista_propiedades

    recursive subroutine elemento_propiedad()
        if ( vistazo%type /= COMENTARIO )  then

            if ( vistazo%type /= RESERVADA_PROPIEDADES ) then
                call match(IDENTIFICADOR)
                call match(SIGNO_PUNTO)
                call tipo_elemento_propiedad()
                call match(SIGNO_PUNTO_COMA)
            else
                vacio = .TRUE.
            end if
            
        else
            num_pos = num_pos + 1
            vistazo = Tokens(num_pos)
        end if
    end subroutine elemento_propiedad

    subroutine tipo_elemento_propiedad()
        select case (vistazo%type)
            case (RESERVADA_PROPIEDAD_COLOR_LETRA)
                call match(RESERVADA_PROPIEDAD_COLOR_LETRA)
                call match(PARENTESIS_ABRE)
                call propiedad_color()
                call match(PARENTESIS_CIERRA)
            case (RESERVADA_PROPIEDAD_TEXTO)
                call match(RESERVADA_PROPIEDAD_TEXTO)
                call match(PARENTESIS_ABRE)
                call match(RESERVADA_CADENA)
                call match(PARENTESIS_CIERRA)
            case (RESERVADA_PROPIEDAD_ALINEACION)
                call match(RESERVADA_PROPIEDAD_ALINEACION)
                call match(PARENTESIS_ABRE)
                call propiedad_alineacion()
                call match(PARENTESIS_CIERRA)
            case (RESERVADA_PROPIEDAD_COLOR_FONDO)
                call match(RESERVADA_PROPIEDAD_COLOR_FONDO)
                call match(PARENTESIS_ABRE)
                call propiedad_color()
                call match(PARENTESIS_CIERRA)
            case (RESERVADA_PROPIEDAD_MARCADA)
                call match(RESERVADA_PROPIEDAD_MARCADA)
                call match(PARENTESIS_ABRE)
                call propiedad_valor()
                call match(PARENTESIS_CIERRA)
            case (RESERVADA_PROPIEDAD_GRUPO)
                call match(RESERVADA_PROPIEDAD_GRUPO)
                call match(PARENTESIS_ABRE)
                call match(IDENTIFICADOR)
                call match(PARENTESIS_CIERRA)
            case (RESERVADA_PROPIEDAD_ANCHO)
                call match(RESERVADA_PROPIEDAD_ANCHO)
                call match(PARENTESIS_ABRE)
                call match(ENTERO)
                call match(PARENTESIS_CIERRA)
            case (RESERVADA_PROPIEDAD_ALTO)
                call match(RESERVADA_PROPIEDAD_ALTO)
                call match(PARENTESIS_ABRE)
                call match(ENTERO)
                call match(PARENTESIS_CIERRA)
            case default
                print *, "Error sintactico: Tipo de control no valido."
                call panicModeRecovery(RESERVADA_PROPIEDADES, IDENTIFICADOR)
            
        end select
        
    end subroutine tipo_elemento_propiedad

    subroutine propiedad_color() ! ENTERO, ENTERO, ENTERO
        call match(ENTERO)
        CALL match(SIGNO_COMA)
        call match(ENTERO)
        CALL match(SIGNO_COMA)
        call match(ENTERO)
        
    end subroutine propiedad_color

    subroutine propiedad_alineacion() ! ENTERO, ENTERO, ENTERO
        select case (tokens(num_pos)%type)
            case (RESERVADA_IZQUIERDA)
                call match(RESERVADA_IZQUIERDA)
            case (RESERVADA_CENTRO)
                call match(RESERVADA_CENTRO)
            case (RESERVADA_DERECHA)
                call match(RESERVADA_DERECHA)
            case default
                print *, "Error sintactico: Tipo de control no valido."
            
        end select
        
    end subroutine propiedad_alineacion

    subroutine propiedad_valor() ! ENTERO, ENTERO, ENTERO
        select case (tokens(num_pos)%type)
            case (RESERVADA_TRUE)
                call match(RESERVADA_TRUE)
            case (RESERVADA_FALSE)
                call match(RESERVADA_FALSE)
            case default
                print *, "Error sintactico: Tipo de control no valido."
            
        end select
        
    end subroutine propiedad_valor

    subroutine bloque_colocacion()
        call match(SIGNO_MAYOR) ! <
        call match(SIGNO_EXCLAMACION) ! <
        call match(SIGNO_GUION) ! <
        call match(SIGNO_GUION)
        call match(RESERVADA_COLOCACION) ! CONTROLES
        call lista_posicionamiento()
        call match(RESERVADA_COLOCACION) ! CONTROLES
        call match(SIGNO_GUION) ! <
        call match(SIGNO_GUION) ! <
        call match(SIGNO_MENOR) ! <
    end subroutine bloque_colocacion

    recursive subroutine lista_posicionamiento()
        if ( .not. vacio) then
            call detalles_posicionamiento()
            call lista_posicionamiento()
        end if
        
    end subroutine lista_posicionamiento
    
    recursive subroutine detalles_posicionamiento()
        if ( vistazo%type /= COMENTARIO )  then

            if ( vistazo%type /= RESERVADA_COLOCACION ) then
                call elemento_identificador()
                call match(SIGNO_PUNTO)
                call propiedad_colocacion()
                call match(SIGNO_PUNTO_COMA)
            else
                vacio = .TRUE.
            end if
            
        else
            num_pos = num_pos + 1
            vistazo = Tokens(num_pos)
        end if
    end subroutine detalles_posicionamiento



    subroutine elemento_identificador()
        select case (tokens(num_pos)%type)
            case (RESERVADA_THIS)
                call match(RESERVADA_THIS)
            case (IDENTIFICADOR)
                call match(IDENTIFICADOR)
            case default
                print *, "Error sintactico: Tipo de control no valido."
                call panicModeRecovery(RESERVADA_COLOCACION, SIGNO_PUNTO)
        
        end select
    end subroutine elemento_identificador

    subroutine propiedad_colocacion()
        select case (tokens(num_pos)%type)
            case (RESERVADA_PROPIEDAD_POSICION)
                call match(RESERVADA_PROPIEDAD_POSICION)
                call match(PARENTESIS_ABRE)
                call elemento_posicion()
                call match(PARENTESIS_CIERRA)
            case (RESERVADA_ADD)
                call match(RESERVADA_ADD)
                call match(PARENTESIS_ABRE)
                call match(IDENTIFICADOR)
                call match(PARENTESIS_CIERRA)
            case default
                print *, "Error sintactico: Tipo de control no valido."
                call panicModeRecovery(RESERVADA_COLOCACION, SIGNO_PUNTO)
        
        end select
        
    end subroutine propiedad_colocacion

    subroutine elemento_posicion()
        call match(ENTERO)
        call match(SIGNO_COMA)
        call match(ENTERO)
        
    end subroutine elemento_posicion

    subroutine match(type_mach)
        integer, intent(in) :: type_mach
        if ( vistazo%type == type_mach) then
            ! print *, TRIM(vistazo%lexema)
            vacio = .FALSE.
            num_pos = num_pos + 1
            vistazo = Tokens(num_pos)
                
        else
            print *, 'Ocurrio un error, se esperaba ', TRIM(tipo_token(type_mach))

            call panicModeRecovery(type_mach, SIGNO_PUNTO_COMA)
            

        end if
        
    end subroutine match

    subroutine panicModeRecovery(token_esperado, punto_sincronizacion)
        integer, intent(in) :: token_esperado
        integer, intent(in) :: punto_sincronizacion
        do while (num_pos <= size(tokens) .and. vistazo%type/= punto_sincronizacion)
            num_pos = num_pos + 1
            vistazo = Tokens(num_pos)
        end do
    end subroutine panicModeRecovery

    function tipo_token(token_parametro) result(token_)
        integer, intent(in) :: token_parametro
        character(len=100) :: token_ 
        select case (token_parametro)
            case (RESERVADA_CONTROLES)
                token_ = 'RESERVADA_CONTROLES'
            case (RESERVADA_ETIQUETA)
                token_ = 'RESERVADA_ETIQUETA'
            case (RESERVADA_BOTON)
                token_ = 'RESERVADA_BOTON'
            case (RESERVADA_CHECK)
                token_ = 'RESERVADA_CHECK'
            case (RESERVADA_RADIOBOTON)
                token_ = 'RESERVADA_RADIOBOTON'
            case (RESERVADA_TEXTO)
                token_ = 'RESERVADA_TEXTO'
            case (RESERVADA_AREATEXTO)
                token_ = 'RESERVADA_AREATEXTO'
            case (RESERVADA_CLAVE)
                token_ = 'RESERVADA_CLAVE'
            case (RESERVADA_CONTENEDOR)
                token_ = 'RESERVADA_CONTENEDOR'
            case (RESERVADA_PROPIEDADES)
                token_ = 'RESERVADA_PROPIEDADES'
            case (RESERVADA_PROPIEDAD_COLOR_LETRA)
                token_ = 'RESERVADA_PROPIEDAD_COLOR_LETRA'
            case (RESERVADA_PROPIEDAD_TEXTO)
                token_ = 'RESERVADA_PROPIEDAD_TEXTO'
            case (RESERVADA_PROPIEDAD_ALINEACION)
                token_ = 'RESERVADA_PROPIEDAD_ALINEACION'
            case (RESERVADA_PROPIEDAD_COLOR_FONDO)
                token_ = 'RESERVADA_PROPIEDAD_COLOR_FONDO'
            case (RESERVADA_PROPIEDAD_MARCADA)
                token_ = 'RESERVADA_PROPIEDAD_MARCADA'
            case (RESERVADA_PROPIEDAD_GRUPO)
                token_ = 'RESERVADA_PROPIEDAD_GRUPO'
            case (RESERVADA_PROPIEDAD_ANCHO)
                token_ = 'RESERVADA_PROPIEDAD_ANCHO'
            case (RESERVADA_PROPIEDAD_ALTO)
                token_ = 'RESERVADA_PROPIEDAD_ALTO'
            case (RESERVADA_PROPIEDAD_POSICION)
                token_ = 'RESERVADA_PROPIEDAD_POSICION'
            case (RESERVADA_COLOCACION)
                token_ = 'RESERVADA_PROPIEDAD_COLOCACION'
            case (RESERVADA_THIS)
                token_ = 'RESERVADA_THIS'
            case (RESERVADA_ADD)
                token_ = 'RESERVADA_ADD'
            case (SIGNO_PUNTO)
                token_ = 'SIGNO_PUNTO'
            case (SIGNO_COMA)
                token_ = 'SIGNO_COMA'
            case (SIGNO_PUNTO_COMA)
                token_ = 'SIGNO_PUNTO_COMA'
            case (SIGNO_MAYOR)
                token_ = 'SIGNO_MAYOR'
            case (SIGNO_MENOR)
                token_ = 'SIGNO_MENOR'
            case (SIGNO_EXCLAMACION)
                token_ = 'SIGNO_EXCLAMACION'
            case (SIGNO_GUION)
                token_ = 'SIGNO_GUION'
            case (PARENTESIS_ABRE)
                token_ = 'PARENTESIS_ABRE'
            case (PARENTESIS_CIERRA)
                token_ = 'PARENTESIS_CIERRA'
            case (COMENTARIO)
                token_ = 'COMENTARIO'
            case (ENTERO)
                token_ = 'ENTERO'
            case (IDENTIFICADOR)
                token_ = 'IDENTIFICADOR'
            case (RESERVADA_CADENA)
                token_ = 'RESERVADA_CADENA'
            case (ERROR)
                token_ = 'ERROR'
            case (RESERVADA_TRUE)
                token_ = 'RESERVADA_TRUE'
            case (RESERVADA_FALSE)
                token_ = 'RESERVADA_FALSE'
            case (RESERVADA_IZQUIERDA)
                token_ = 'RESERVADA_IZQUIERDA'
            case (RESERVADA_CENTRO)
                token_ = 'RESERVADA_CENTRO'
            case (RESERVADA_DERECHA)
                token_ = 'RESERVADA_DERECHA'
            case default
                token_ = "Desconocido"
        end select
    
        
    end function tipo_token

end module ModuloAnalizadorSintactico