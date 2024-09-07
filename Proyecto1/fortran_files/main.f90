program main

    use TokenModule
    use AnalizadorModulo

    implicit none
        ! Define variables
        type(Token), dimension(:), allocatable :: listTokens
        type(Analizador) :: miAnalizador
        call readFile(listTokens)
    contains

    subroutine readFile(listTokens)
        type(Token), dimension(:), allocatable, intent(inout) :: listTokens
        character(len=200) :: linea
        integer :: fila, estadoApertura
        OPEN(UNIT=2, FILE='../test_files/prueba.data', STATUS='OLD', ACTION='READ', IOSTAT= estadoApertura)
    
        ! Si la apertura del archivo no tiene errores
        if ( estadoApertura /= 0 ) then
    
            print *, "A ocurrido un error al abrir el archivo"
    
        else
    
            fila = 1
    
            DO
    
                READ(2, '(A)', IOSTAT=estadoApertura) linea
    
                if (estadoApertura /= 0) exit
    
                call miAnalizador%analizarCadena(linea, fila)
    
                fila = fila + 1
    
            END DO
    
        end if
    
        ! Cerramos el archivo
        close(2)
        
    end subroutine readFile
    
end program main