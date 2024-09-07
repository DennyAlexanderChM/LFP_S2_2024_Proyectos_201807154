module AnalizadorModulo
    use TokenModule
    implicit none
    private :: isalpha, isdigit
    public :: Analizador, analizarCadena
    type :: Analizador
        type(Token), dimension(:), allocatable :: listTokens
        integer :: columna, fila, estado! Posiciön "X" | "Y"
        character(len=:), allocatable :: lexema
        character(len=100) :: typeLexema

    contains
        procedure :: analizarCadena
        procedure :: printTokens
        procedure, private :: agregarToken
        procedure, private :: palabraReservada
    
    end type Analizador
contains
    subroutine analizarCadena(this, cadena, fila)
        class(Analizador), intent(inout) :: this
        character(len=200), intent(in) :: cadena
        integer, intent(in) :: fila
        character(len=:), allocatable  :: cadenaNueva
        character :: actual = ''

        this%columna = 1
        this%estado = 0
        this%fila = fila
        this%lexema = ''
        cadenaNueva = TRIM(cadena) // '#'

        do while (this%columna <= LEN(cadenaNueva))
            actual = cadena(this%columna:this%columna)

            if ( this%estado == 0 ) then

                if ( isalpha(actual) ) then
                    this%estado = 1
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                
                else if ( isdigit(actual) ) then
                    this%estado = 3
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1

                else if (actual == '"') then
                    this%estado = 2
                    this%lexema = this%lexema //actual
                    this%columna = this%columna + 1

                else if (actual == ':') then ! Dos puntos
                    this%typeLexema = 'DOS PUNTOS'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%estado = 0
                    this%columna =  this%columna + 1

                else if (actual == '{') then ! Llave abre
                    this%typeLexema = 'LLAVE ABRE'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%estado = 0
                    this%columna =  this%columna + 1
                
                else if (actual == '}') then ! Llave cierra
                    this%typeLexema = 'LLAVE CIERRA'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%estado = 0
                    this%columna = this%columna + 1
                
                else if (actual == ';') then ! Punto y coma
                    this%typeLexema = 'PUNTO COMA'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%estado = 0
                    this%columna =  this%columna + 1

                else if (actual == '#') then 
                    this%estado = 0
                    this%columna =  this%columna + 1

                else if (actual == ' ' .OR. actual == '\n' .OR. actual == '\r') then 
                    this%estado = 0
                    this%columna =  this%columna + 1
                
                else ! Caracter sin reconocer
                    this%typeLexema = 'ERROR'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%estado = 0
                    this%columna =  this%columna + 1
                    
                end if
            else if ( this%estado == 1 ) then
                if ( isalpha(actual) ) then
                    this%estado = 1
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                
                else
                    call this%palabraReservada()
                    this%estado = 0
                    
                end if
            else if ( this%estado == 2 ) then
                if ( actual /= '"' ) then
                    this%estado = 2
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    
                else
                    this%typeLexema = 'CADENA'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%estado = 0
                    this%columna = this%columna + 1
                end if
                
            else if ( this%estado == 3 ) then
                if ( isdigit(actual) ) then
                    this%estado = 3
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1

                else if ( actual == '%') then
                    this%typeLexema = 'PORCENTAJE'
                    this%lexema = this%lexema // actual
                    call this%agregarToken()
                    this%estado = 0
                    this%columna = this%columna + 1
                
                else
                    this%typeLexema = 'ENTERO'
                    call this%agregarToken()
                    this%estado = 0
                    
                end if
            
            end if

        end do

        
    end subroutine analizarCadena

    function isalpha(str) result(is_alpha)
        character, intent(in) :: str
        logical :: is_alpha

        is_alpha = .FALSE.

        if ((ICHAR(str) >= ICHAR('A')) .AND. (ICHAR(str) <= ICHAR('Z')) .OR. &
            (ICHAR(str) >= ICHAR('a') .AND. ICHAR(str) <= ICHAR('z'))) then
            is_alpha = .TRUE.

        end if
    
    end function isalpha

    function isdigit(str) result(is_digit)
        character, intent(in) :: str
        logical :: is_digit

        is_digit = .FALSE.

        if (ICHAR(str) >= ICHAR('0') .AND. ICHAR(str) <= ICHAR('9')) then
            is_digit = .TRUE.

        end if
    
    end function isdigit

    subroutine palabraReservada(this)
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
            this%typeLexema = "PALABRA DESCONOCIDA"
            call this%agregarToken()
        end if
        
    end subroutine

    subroutine agregarToken(this)
        class(Analizador), intent(inout) :: this
        class(Token), allocatable :: temp(:)
        integer :: i

        i = SIZE(this%listTokens) + 1

        if ( allocated(this%listTokens) ) then
            allocate(temp(i-1))
            temp = this%listTokens
            deallocate(this%listTokens)
            allocate(this%listTokens(i))
            this%listTokens(1:i-1) = temp
            deallocate(temp)
        else
            allocate(this%listTokens(i))
        end if

        this%listTokens(i)%lexema = this%lexema
        this%listTokens(i)%type = this%typeLexema
        this%listTokens(i)%position_x = this%columna
        this%listTokens(i)%posicion_y = this%fila

        this%lexema = ''
        
    end subroutine

    subroutine printTokens(this)
        class(Analizador), intent(inout) :: this
      
        integer :: i

        i = SIZE(this%listTokens)

        do i = 1, SIZE(this%listTokens)
            print *, 'Token: ' , this%listTokens(i)%type , ' Lexema: ' , this%listTokens(i)%lexema
            
        end do

    end subroutine 
    
end module AnalizadorModulo