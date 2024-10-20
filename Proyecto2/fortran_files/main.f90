program main
    ! Importamos los modulos a utilizar
    use ModuloAnalizador
    use ModuloAnalizadorSintactico
    use ModuloTraductor
    use ModuloControl
    use ModuloError

    implicit none

        ! DEFINIR VARIABLES
        type(Token), dimension(:), allocatable :: lista_tokens
        type(Control), dimension(:), allocatable :: lista_controles
        CHARACTER(LEN=:), ALLOCATABLE :: contenido

        call readFile() ! Inicia la lectura del texto recibido
        call parser(lista_tokens) ! Inicia el analisis sintactico
        call text_token() ! Crea un archivo .txt de tokens

        if ( .NOT. allocated(errores%lista_errores) ) then ! Verifica si la lista de errores fue inicializada
            ! call Traducir
            ! Recorre la lista de tokens para crear los elementos de tipo Control
            ! Las cuales almacenan las propiedades detalladas en el archivo de entrada
            call Traducir(lista_tokens, lista_controles)
            ! Genera la pagina html con los elementos obtenidos de la funcion Traducir
            call generar_pagina(lista_controles)
            ! Genera el archivo CSS con las propiedades obtenidas de la funcion Traducir
            call generar_estilos(lista_controles)
            
            print *, 'CORRECTO' ! Mensaje que se envia al fronted

        else

            call errores%text_error()
            
            PRINT *, 'ERROR' ! Mensaje que se envia al fronted

        end if
        
    contains

    subroutine readFile()
        character(len=200) :: linea
        integer :: estadoApertura
        contenido = ''

        DO

            READ(*, '(A)', IOSTAT=estadoApertura) linea
    
            if (estadoApertura /= 0) exit
                
            contenido = contenido // TRIM(linea) // CHAR(10)
                
        END DO

        call analizarCadena(contenido, lista_tokens)

        close(2)
        
    end subroutine readFile

    subroutine text_token() ! Crea el archivo html con los datos de los tokens
        integer :: i, num_error
        num_error = 1

        ! Abrir el archivo para escribir (crea el archivo si no existe)
        open(unit=12, file="tokens.txt", status="replace", action="write")
          
        if ( allocated(lista_tokens) ) then ! Verifica si la lista de tokens fue inicializada
            
            do i = 1, SIZE(lista_tokens)

                    write(12, *) i, '&&',  TRIM(lista_tokens(i)%lexema) ,'&&', TRIM(tipo_token(lista_tokens(i)%type)) ,'&&', lista_tokens(i)%position_y ,'&&', lista_tokens(i)%position_x

                    num_error = num_error + 1 

            end do

        end if
        
        close(12)
        
    end subroutine text_token
        
end program main
!  gfortran main.f90 mod_elemento.f90 mod_lexico.f90 mod_parser.f90 mod_token.f90 mod_traductor.f90 -o main