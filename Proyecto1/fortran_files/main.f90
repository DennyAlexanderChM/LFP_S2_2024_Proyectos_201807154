program main

    use TokenModule
    use AnalizadorModulo

    implicit none
        ! Define variables
        type(Analizador) :: miAnalizador
        call readFile()

    contains

    subroutine readFile()
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
        
        call miAnalizador%printTokens()
        
    end subroutine readFile
    
end program main