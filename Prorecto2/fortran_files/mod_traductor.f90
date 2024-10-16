module ModuloTraductor

    use ModuloToken
    use ModuloControl
    
    implicit none
    
    type(Token), dimension(:), allocatable :: tokens
    
    integer :: num_pos
    
contains
    
    subroutine Traducir(listaTokens,listaControles)
        type(Token), intent(in), dimension(:), allocatable :: listaTokens 
        type(Control), dimension(:), allocatable :: listaControles
        
        character(len=100) :: bloque, nombre_elemento, nombre_elemento_b
        character(len=:), allocatable  :: propiedad_valor, pos_x, pos_y
        integer :: tipo_elemento, tipo_actual
        tipo_elemento = 0
        tipo_actual = 0
        tokens = listaTokens
        bloque = ''
        nombre_elemento = ''
        nombre_elemento_b = ''
        propiedad_valor = ''
        pos_x = ''
        pos_y = ''
        num_pos = 1

        do while ( num_pos <= SIZE(tokens) )

            tipo_actual = tokens(num_pos)%type

            select case (tipo_actual)

                case (RESERVADA_CONTROLES)
                    bloque = 'CONTROLES'

                case (RESERVADA_PROPIEDADES)
                    bloque = 'PROPIEDADES'

                case (RESERVADA_COLOCACION)
                    bloque = 'COLOCACION'

                case default
                    continue
            end select
            
            if ( bloque == 'CONTROLES' ) then

                if ( tipo_elemento == 0 .AND. reservada_control(tipo_actual) ) tipo_elemento = tipo_actual
                
                if (tipo_elemento > 0 .AND. tipo_actual == IDENTIFICADOR) nombre_elemento = tokens(num_pos)%lexema

                if ( tipo_elemento > 0 .AND. nombre_elemento /= '') then

                    call nuevo_control(listaControles, tipo_elemento, nombre_elemento)
                    ! reinicionamos valores 
                    tipo_elemento = 0
                    nombre_elemento = ''

                end if
            end if

            if ( bloque == 'PROPIEDADES' ) then
                if (tipo_actual == IDENTIFICADOR) nombre_elemento = tokens(num_pos)%lexema
                
                if ( nombre_elemento /= '' .AND. reservada_propiedad(tipo_actual) ) tipo_elemento = tipo_actual
                
                if ( tipo_elemento > 0 .AND. nombre_elemento /= '') then

                    if (tipo_actual == PARENTESIS_ABRE) then
                        num_pos = num_pos + 1
                        tipo_actual = tokens(num_pos)%type

                        do while (tipo_actual /= PARENTESIS_CIERRA)

                            propiedad_valor = propiedad_valor // TRIM(tokens(num_pos)%lexema)

                            num_pos = num_pos + 1

                            tipo_actual = tokens(num_pos)%type

                        end do

                        call editar_propiedades(nombre_elemento, tipo_elemento, propiedad_valor, listaControles )

                        ! reinicionamos valores 
                        tipo_elemento = 0
                        nombre_elemento = ''
                        propiedad_valor = ''
                        
                    end if

                end if
            end if

            if ( bloque == 'COLOCACION' ) then

                if (tipo_actual == IDENTIFICADOR .OR. tipo_actual == RESERVADA_THIS) nombre_elemento = tokens(num_pos)%lexema
                
                if ( nombre_elemento /= '' .AND. reservada_colocar(tipo_actual) ) tipo_elemento = tipo_actual
                
                if ( tipo_elemento == RESERVADA_ADD .AND. nombre_elemento /= '') then
                    if (tipo_actual == PARENTESIS_ABRE) then
                        num_pos = num_pos + 1
                        tipo_actual = tokens(num_pos)%type

                        do while (tipo_actual /= PARENTESIS_CIERRA)

                            nombre_elemento_b = TRIM(tokens(num_pos)%lexema)

                            num_pos = num_pos + 1

                            tipo_actual = tokens(num_pos)%type

                        end do

                        call area_colocacion(nombre_elemento_b, nombre_elemento, listaControles)
                        ! reinicionamos valores 
                        tipo_elemento = 0
                        nombre_elemento = ''
                        nombre_elemento_b = ''
                        propiedad_valor = ''
                        
                    end if
                else if ( tipo_elemento == RESERVADA_PROPIEDAD_POSICION .AND. nombre_elemento /= '' ) then
                    
                    if (tipo_actual == PARENTESIS_ABRE) then
                        num_pos = num_pos + 1
                        tipo_actual = tokens(num_pos)%type

                        do while (tipo_actual /= PARENTESIS_CIERRA)

                            if (tipo_actual == ENTERO) then

                                if ( pos_x == '' ) then
                                    pos_x = TRIM(tokens(num_pos)%lexema)

                                    call editar_propiedades(nombre_elemento, POSICION_X, pos_x, listaControles )
                                    
                                else if (pos_y == '') then  
                                    pos_y = TRIM(tokens(num_pos)%lexema)

                                    call editar_propiedades(nombre_elemento, POSICION_Y, pos_y, listaControles )

                                end if

                            end if

                            num_pos = num_pos + 1

                            tipo_actual = tokens(num_pos)%type

                        end do
                        ! reinicionamos valores 
                        tipo_elemento = 0
                        nombre_elemento = ''
                        nombre_elemento_b = ''
                        propiedad_valor = ''
                        pos_x = ''
                        pos_y = ''
                        
                    end if

                end if
            end if
        
            num_pos = num_pos + 1
        end do

    end subroutine Traducir

    function reservada_control(tipo_token) result(es_reservada)
        integer, intent(in) :: tipo_token
        logical :: es_reservada

        select case (tipo_token)
            case (RESERVADA_ETIQUETA)
                es_reservada = .TRUE.
            case (RESERVADA_BOTON)
                es_reservada = .TRUE.
            case (RESERVADA_CHECK)
                es_reservada = .TRUE.
            case (RESERVADA_RADIOBOTON)
                es_reservada = .TRUE.
            case (RESERVADA_TEXTO)
                es_reservada = .TRUE.
            case (RESERVADA_AREATEXTO)
                es_reservada = .TRUE.
            case (RESERVADA_CLAVE)
                es_reservada = .TRUE.
            case (RESERVADA_CONTENEDOR)
                es_reservada = .TRUE.
            case default
                es_reservada = .FALSE.
        end select
    
        
    end function reservada_control

    function reservada_propiedad(tipo_token) result(es_reservada)
        integer, intent(in) :: tipo_token
        logical :: es_reservada

        select case (tipo_token)
            case (RESERVADA_PROPIEDAD_COLOR_LETRA)
                es_reservada = .TRUE.
            case (RESERVADA_PROPIEDAD_TEXTO)
                es_reservada = .TRUE.
            case (RESERVADA_PROPIEDAD_ALINEACION)
                es_reservada = .TRUE.
            case (RESERVADA_PROPIEDAD_COLOR_FONDO)
                es_reservada = .TRUE.
            case (RESERVADA_PROPIEDAD_MARCADA)
                es_reservada = .TRUE.
            case (RESERVADA_PROPIEDAD_GRUPO)
                es_reservada = .TRUE.
            case (RESERVADA_PROPIEDAD_ANCHO)
                es_reservada = .TRUE.
            case (RESERVADA_PROPIEDAD_ALTO)
                es_reservada = .TRUE.
            case default
                es_reservada = .FALSE.
        end select
    
        
    end function reservada_propiedad

    function reservada_colocar(tipo_token) result(es_reservada)
        integer, intent(in) :: tipo_token
        logical :: es_reservada

        select case (tipo_token)
            case (RESERVADA_PROPIEDAD_POSICION)
                es_reservada = .TRUE.
            case (RESERVADA_ADD)
                es_reservada = .TRUE.
            case default
                es_reservada = .FALSE.
        
        end select
        
    end function reservada_colocar

    subroutine nuevo_control(listaControles, tipo_control, ID_control)
        type(Control), intent(inout), dimension(:), allocatable :: listaControles
        integer, intent(in) :: tipo_control
        character(len = 100), intent(in) ::  ID_control
        type(Control), allocatable :: temp(:)
        integer i

        if ( allocated(listaControles) ) then
            i = SIZE(listaControles) + 1
            allocate(temp(i-1))
            temp = listaControles
            deallocate(listaControles)
            allocate(listaControles(i))
            listaControles(1:i-1) = temp
            deallocate(temp)
        else
            i = 1
            allocate(listaControles(i))
        end if
        call listaControles(i)%incializar_variables()
        
        listaControles(i)%tipo_elemento = tipo_control
        listaControles(i)%ID = ID_control
        
    end subroutine nuevo_control

    subroutine editar_propiedades(ID_control, tipo_propiedad, valor_propiedad, listaControles)
        character(len= 100), intent(in) :: ID_control
        integer, intent(in) :: tipo_propiedad
        character(len=:), intent(in), allocatable :: valor_propiedad
        type(Control), intent(inout), dimension(:), allocatable :: listaControles
        integer :: i

        i = 1
        do while (i <= SIZE(listaControles))

            if ( listaControles(i)%ID == ID_control) then

                select case (tipo_propiedad)
                    case (RESERVADA_PROPIEDAD_COLOR_LETRA)
                        listaControles(i)%RGB_texto = valor_propiedad
                        print*, valor_propiedad
                    case (RESERVADA_PROPIEDAD_TEXTO)
                        listaControles(i)%texto = valor_propiedad
                    case (RESERVADA_PROPIEDAD_ALINEACION)
                        listaControles(i)%tipo_alineacion = valor_propiedad
                    case (RESERVADA_PROPIEDAD_COLOR_FONDO)
                        listaControles(i)%RGB_fondo = valor_propiedad
                    case (RESERVADA_PROPIEDAD_MARCADA)
                        listaControles(i)%marcada = valor_propiedad
                    case (RESERVADA_PROPIEDAD_GRUPO)
                        listaControles(i)%grupo = valor_propiedad
                    case (RESERVADA_PROPIEDAD_ANCHO)
                        listaControles(i)%ancho = valor_propiedad
                    case (RESERVADA_PROPIEDAD_ALTO)
                        listaControles(i)%largo = valor_propiedad
                    case (POSICION_X)
                        listaControles(i)%pos_x = valor_propiedad
                    case (POSICION_Y)
                        listaControles(i)%pos_y = valor_propiedad
                    case default
                        print *, 'Propiedad no encontrada'
                end select

                exit
                
            end if

            i = i + 1
            
        end do
        
    end subroutine editar_propiedades

    subroutine area_colocacion(ID_control, ID_contenedor, listaControles)
        character(len= 100), intent(in) :: ID_control, ID_contenedor
        type(Control), intent(inout), dimension(:), allocatable :: listaControles
        integer :: i

        i = 1
        do while (i <= SIZE(listaControles))

            if ( listaControles(i)%ID == ID_control) then

                listaControles(i)%ID_contenedor = ID_contenedor

                exit
                
            end if

            i = i + 1
            
        end do
        
    end subroutine area_colocacion

    subroutine generar_pagina(listaControles) ! Crea el archivo html con los datos de los tokens
        type(Control), dimension(:), allocatable :: listaControles
        integer :: i
        i = 1
        
        if ( SIZE(listaControles) > 0 ) then

            ! Abrir el archivo para escribir (crea el archivo si no existe)
            open(unit=10, file='TOKENS.html', status='replace', action='write')
            write(10, *) '<!DOCTYPE html><html><head><link rel="stylesheet" href="style.css"><title>ESTA ES UNA PRUEBA</title></head><body>'

            do while ( i <= SIZE(listaControles))

                if ( listaControles(i)%ID_contenedor  == 'this' ) then

                    write(10, *) '<div id="',TRIM(listaControles(i)%ID),'">'

                    write(10, *) generar_elementos(listaControles(i)%ID, listaControles)
                    
                    write(10, *) '</div>'

                end if

                i = i + 1
                        
            end do

            write(10, *) '</body></html>'
            
            close(10)
        end if
        
        
    end subroutine generar_pagina

    recursive function generar_elementos(ID_elemento, listaControles) result(html_lines)
        character(len = 100), intent(in) :: ID_elemento
        type(Control), intent(inout), dimension(:), allocatable :: listaControles
        character(len = :), allocatable :: html_lines

        integer :: i
        html_lines = ''
        i = 1
        do while (i < SIZE(listaControles))

            if ( listaControles(i)%ID_contenedor == ID_elemento ) then

                if ( listaControles(i)%tipo_elemento == RESERVADA_CONTENEDOR ) then
                    
                    html_lines = html_lines // '<div id="'// TRIM(listaControles(i)%ID) // '">' // TRIM(generar_elementos(listaControles(i)%ID, listaControles)) // '</div>'
                else

                    html_lines = html_lines // TRIM(listaControles(i)%elemento_html())

                end if

            end if

            i = i + 1 
            
        end do    
        
    end function generar_elementos

    subroutine generar_estilos(listaControles) ! Crea el archivo html con los datos de los tokens
        type(Control), dimension(:), allocatable :: listaControles
        integer :: i
        i = 1
        
        if ( SIZE(listaControles) > 0 ) then

            ! Abrir el archivo para escribir (crea el archivo si no existe)
            open(unit=11, file='style.css', status='replace', action='write')
            do while ( i <= SIZE(listaControles))

                write(11, *) '#',TRIM(listaControles(i)%ID) ,'{' ! Ejemplo: #JTextField0 {
                write(11, *) 'position: absolute;'
                write(11, *) 'font-size: 12px;'

                if ( listaControles(i)%pos_y /= '' )  write(11, *) 'top: ' , TRIM(listaControles(i)%pos_y) , 'px;'
                if ( listaControles(i)%pos_x /= '' )  write(11, *) 'left: ' , TRIM(listaControles(i)%pos_x) , 'px;'
                if ( listaControles(i)%largo /= '' )  write(11, *) 'height: ' , TRIM(listaControles(i)%largo) , 'px;'
                if ( listaControles(i)%ancho /= '' )  write(11, *) 'width: ' , TRIM(listaControles(i)%ancho) , 'px;'
                if ( listaControles(i)%RGB_texto /= '' )  write(11, *) 'color: rgb(' , TRIM(listaControles(i)%RGB_texto) , ');'
                if ( listaControles(i)%RGB_fondo /= '' )  write(11, *) 'background-color: rgb(' , TRIM(listaControles(i)%RGB_fondo) , ');'
                write(11, *) '}'
                i = i + 1
                        
            end do
            
            close(11)
        end if
        
        
    end subroutine generar_estilos
    
end module ModuloTraductor