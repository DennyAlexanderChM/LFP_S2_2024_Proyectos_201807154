program main

    use ModuloAnalizador
    use ModuloAnalizadorSintactico
    use ModuloTraductor
    use ModuloControl

    implicit none
        type(Analizador) :: miAnalizador 
        type(control), dimension(:), allocatable :: listaControles
        CHARACTER(LEN=:), ALLOCATABLE :: contenido

        call readFile()
        call parser(miAnalizador%listaTokens)
        ! call Traducir(miAnalizador%listaTokens, listaControles)
        ! CALL generar_pagina(listaControles)
        ! call generar_estilos(listaControles)
        
    contains

    subroutine readFile()
        character(len=200) :: linea
        integer :: estadoApertura
        contenido = ''

        OPEN(UNIT= 2, FILE='../test_files/prueba.LFP', STATUS='OLD', ACTION='READ', IOSTAT= estadoApertura)

        ! Si la apertura del archivo no tiene errores
        if ( estadoApertura /= 0 ) then

            print *, "A ocurrido un error al abrir el archivo"
        
        else

            DO

                READ(2, '(A)', IOSTAT=estadoApertura) linea
    
                if (estadoApertura /= 0) exit
                
                contenido = contenido // TRIM(linea) // CHAR(10)
                
            END DO

            call miAnalizador%analizarCadena(contenido)

        end if

        ! Cerramos el archivo
        close(2)
        
    end subroutine readFile

    subroutine print_dates()
        integer:: i
        do i = 1, SIZE(listaControles)

            print *, TRIM(listaControles(i)%ID), TRIM(listaControles(i)%ID_contenedor), TRIM(listaControles(i)%pos_x), TRIM(listaControles(i)%pos_y)
            
        end do
    
        
    end subroutine print_dates
    
end program main