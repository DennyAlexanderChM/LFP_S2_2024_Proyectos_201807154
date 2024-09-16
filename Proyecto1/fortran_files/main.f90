program main

    use ModuloAnalizador

    implicit none
        ! Define variables
        type(Analizador) :: miAnalizador
        type(Grafica) :: miGrafica

        call readFile()

        if ( miAnalizador%correcto ) then
            call miAnalizador%reporteTokens()
            call miAnalizador%guardarDatos(miGrafica)
            call miGrafica%buscarPaisOptimo()
            call miGrafica%generarGrafo()

        else
            call miAnalizador%reporteError()
            print *, 'Error'
            
        end if

    contains

    subroutine readFile()
        character(len=200) :: linea
        integer :: fila, estadoApertura
        fila = 1

        do
            READ(*, '(A)', IOSTAT=estadoApertura) linea ! Lee el archivo

            if (estadoApertura /= 0) exit

            call miAnalizador%analizarCadena(linea, fila)

            fila = fila + 1
            
        end do
        
    end subroutine readFile
    
end program main