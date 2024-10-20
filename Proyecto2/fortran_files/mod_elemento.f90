module ModuloControl

    use ModuloToken
    implicit none
    
    type, public :: Control

        character(len = 200) :: ID, ID_contenedor, texto, tipo_alineacion, grupo, RGB_texto, RGB_fondo, marcada, ancho, largo, pos_x, pos_y
        integer :: tipo_elemento
        character(len=:), allocatable :: resultado_html
    
    contains
        procedure :: incializar_variables

        procedure :: elemento_html => elemento_html

        procedure :: obtener_alineacion  => obtener_alineacion

        procedure :: obtener_texto  => obtener_texto
        
    end type Control

    contains
        subroutine incializar_variables(this)
            class(Control), intent(inout) :: this
            this%ID = ''
            this%ID_contenedor = ''
            this%texto = ''
            this%tipo_alineacion = ''
            this%grupo = ''
            this%RGB_texto = ''
            this%RGB_fondo = ''
            this%marcada = ''
            this%ancho = ''
            this%largo = ''
            this%pos_x = ''
            this%pos_y = ''
            this%tipo_elemento = 0
            
        end subroutine incializar_variables

    function elemento_html(this) result(resultado_html)
        class(Control), intent(inout) :: this
        character(len=:), allocatable :: resultado_html
        resultado_html = ''

        select case (this%tipo_elemento)

            case (RESERVADA_ETIQUETA)

                resultado_html = '<label id="' // TRIM(this%ID) // '">' // TRIM(this%obtener_texto())  // '</label>'

            case (RESERVADA_BOTON)

                resultado_html = '<button type="submit" id="'// TRIM(this%ID)// '" style="text-align: '// TRIM(this%obtener_alineacion()) // ';">' //TRIM(this%obtener_texto()) // '</button>'
                
            case (RESERVADA_CHECK)

                resultado_html =  '<input type="checkbox" name="'// TRIM(this%grupo) //'" id="'// TRIM(this%ID) //'"'
                
                if ( this%marcada /= '' ) resultado_html = resultado_html // ' checked'

                resultado_html =  resultado_html // ' />'

            case (RESERVADA_RADIOBOTON)

                resultado_html = '<input type="radio" name="'// TRIM(this%grupo) //'" id="'// TRIM(this%ID) //'"'

                if ( this%marcada /= '' ) resultado_html = resultado_html // ' checked'

                resultado_html =  resultado_html // ' />'

            case (RESERVADA_TEXTO)

                if ( TRIM(this%texto) == '' ) this%texto = '""' 

                resultado_html = '<input type="text" id="'// TRIM(this%ID) //'" value=' // TRIM(this%texto) //' style="text-align: '// TRIM(this%obtener_alineacion()) // ';">'
            
            case (RESERVADA_AREATEXTO)

                resultado_html = ' <textarea id="'// TRIM(this%ID) //'>'// TRIM(this%obtener_texto()) //'</textarea>'
           
            case (RESERVADA_CLAVE)

                if ( TRIM(this%texto) == '' ) this%texto = '""'

                resultado_html = '<input type="password" id="'// TRIM(this%ID) //'" value=' // TRIM(this%texto) //' style="text-align: '// TRIM(this%obtener_alineacion()) // ';">'
            
            case default

                resultado_html = ''

        end select
        
    end function elemento_html

    function obtener_alineacion(this) result(alineacion)
        class(Control), intent(in) :: this
        character(len=20) :: alineacion

        if ( TRIM(this%tipo_alineacion) == 'izquierdo' .OR.  TRIM(this%tipo_alineacion) == '') then
            alineacion = 'left'
        else if ( TRIM(this%tipo_alineacion) == 'centro' ) then
            alineacion = 'center'
        else if ( TRIM(this%tipo_alineacion) == 'derecho' ) then  
            alineacion = 'right'  
        end if

        
    end function obtener_alineacion

    function obtener_texto(this) result(sin_comillas)
        class(Control), intent(in) :: this
        character(len=:), allocatable :: sin_comillas
        integer :: i

        sin_comillas = ''

        do i = 1, LEN_TRIM(this%texto)
            if ( this%texto(i:i) /= '"' ) sin_comillas = sin_comillas // this%texto(i:i)
        end do
        
    end function obtener_texto

    
end module ModuloControl