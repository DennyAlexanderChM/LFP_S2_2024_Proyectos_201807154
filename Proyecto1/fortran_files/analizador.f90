module AnalizadorModulo
    use TokenModule
    implicit none
    private :: isalpha, isdigit
    public :: Analizador, analizarCadena
    type :: Analizador
        integer :: columna, fila, estado! Posiciön "X" | "Y"
        character(len=:), allocatable :: lexema

    contains
        procedure :: analizarCadena
        procedure :: agregarToken
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
                    this%estado = 0
                    this%lexema = ''
                    this%columna =  this%columna + 1

                else if (actual == '{') then ! Llave abre
                    this%estado = 0
                    this%lexema = ''
                    this%columna =  this%columna + 1
                
                else if (actual == '}') then ! Llave cierra
                    this%estado = 0
                    this%lexema = ''
                    
                    this%columna = this%columna + 1
                
                else if (actual == ';') then ! Punto y coma
                    this%estado = 0
                    this%lexema = ''
                    this%columna =  this%columna + 1

                else if (actual == '#') then 
                    this%estado = 0
                    this%columna =  this%columna + 1

                else if (actual == ' ' .OR. actual == '\n' .OR. actual == '\r') then 
                    this%estado = 0
                    this%columna =  this%columna + 1
                
                else ! Caracter sin reconocer
                    this%estado = 0
                    this%lexema = ''
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
                    this%lexema = ''

                end if
            else if ( this%estado == 2 ) then
                if ( actual /= '"' ) then
                    this%estado = 2
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1
                    
                else
                    this%estado = 0
                    this%lexema = ''
                    this%columna = this%columna + 1
                end if
                
            else if ( this%estado == 3 ) then
                if ( isdigit(actual) ) then
                    this%estado = 3
                    this%lexema = this%lexema // actual
                    this%columna = this%columna + 1

                else if ( actual == '%') then
                    this%estado = 0
                    this%lexema = this%lexema // actual
                    this%lexema = ''
                    this%columna = this%columna + 1
                
                else
                    this%estado = 0
                    this%lexema = ''
                    
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
        print *, this%lexema
        
    end subroutine

    subroutine agregarToken(this)
        class(Analizador), intent(inout) :: this
        
    end subroutine 
    
end module AnalizadorModulo



