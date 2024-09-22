module ModuloGrafica
    use ModuloContinente
    implicit none
    type, public :: Grafica
        character(len=100) :: nombreGrafica
        integer :: numContinentes
        type(Continente), dimension(:), allocatable :: listaContinentes
    
    contains
        procedure :: addContinente
        procedure :: addPaisContinente
        procedure :: buscarPaisOptimo
        procedure :: generarGrafo
        procedure :: printDates
        
    end type Grafica
    
contains
    subroutine addContinente(this,  nombreContinente)
        class(Grafica), intent(inout) :: this
        type(Continente), allocatable :: temp(:)
        character(len = 100), intent(in) :: nombreContinente
        integer :: i
        i = 0

        if ( allocated(this%listaContinentes) ) then
            i = SIZE(this%listaContinentes) + 1
            allocate(temp(i-1))
            temp = this%listaContinentes
            deallocate(this%listaContinentes)
            allocate(this%listaContinentes(i))
            this%listaContinentes(1:i-1) = temp
            deallocate(temp)
        else
            i = 1
            allocate(this%listaContinentes(i))
        end if
        
        this%numContinentes = i
        this%listaContinentes(i)%nombre = nombreContinente
        
    end subroutine addContinente

    subroutine addPaisContinente(this, nombrePais, poblacion, saturacion, bandera)
        class(Grafica), intent(inout) :: this
        character(len=100), intent(in) :: nombrePais, bandera
        integer, intent(in) :: poblacion, saturacion
        if ( this%numContinentes > 0 ) then

            call this%listaContinentes(this%numContinentes)%addPais(nombrePais, poblacion, saturacion, bandera)
            
        end if
        
    end subroutine addPaisContinente

    subroutine printDates(this)
        class(Grafica), intent(inout) :: this
        integer :: i, j
        i = 1
        j = 1

        print *, 'Grafica Nombre: ' , this%nombreGrafica

        do while (i <= SIZE(this%listaContinentes))

            print *, this%listaContinentes(i)%nombre
            print *, SIZE(this%listaContinentes(i)%paises)
            j = 1
            do while (j <= SIZE(this%listaContinentes(i)%paises))

                print *, this%listaContinentes(i)%paises(j)%nombre
                print *, this%listaContinentes(i)%paises(j)%poblacion
                print *, this%listaContinentes(i)%paises(j)%densidad
                print *, this%listaContinentes(i)%paises(j)%bandera
                
                j = j + 1
            end do
            i = i + 1
        end do
        
    end subroutine printDates

    subroutine buscarPaisOptimo(this)
        class(Grafica), intent(inout) :: this
        character(len = 100) :: nombrePais, banderaPais
        integer :: i, j, densidadPais, habitantesPais, densidadContinente
        type(Pais) :: tempPais
        ! Inicializando variables
        i = 1
        nombrePais = ''
        banderaPais = ''
        densidadPais = 100
        densidadContinente = 100
        habitantesPais = 0

        if ( SIZE(this%listaContinentes) > 0 ) then
            do while (i <= SIZE(this%listaContinentes))
                call this%listaContinentes(i)%densidadMedia() ! Obtenemos la densidad media del continente
                j = 1
                do while (j <= SIZE(this%listaContinentes(i)%paises))
                    tempPais = this%listaContinentes(i)%paises(j)
                    if ( tempPais%densidad < densidadPais ) then
                        densidadPais = tempPais%densidad
                        nombrePais = tempPais%nombre
                        habitantesPais = tempPais%poblacion
                        densidadContinente = this%listaContinentes(i)%densidad
                        banderaPais = tempPais%bandera
                    ! Si el porcentaje de densidad son los mismos
                    else if ( tempPais%densidad == densidadPais ) then
                        ! Se compara el porcentaje de densidad media del continente
                        if ( this%listaContinentes(i)%densidad < densidadContinente) then
                            densidadPais = tempPais%densidad
                            nombrePais = tempPais%nombre
                            habitantesPais = tempPais%poblacion
                            densidadContinente = this%listaContinentes(i)%densidad
                            banderaPais = tempPais%bandera
                            
                        end if
                    end if
                    j = j + 1
                end do
                i = i + 1
            end do
        end if
        ! Imprimimos los datos encontrados
        print *, TRIM(nombrePais) , ',' , habitantesPais ,  ',' , densidadPais , ',' , TRIM(banderaPais)
        
    end subroutine buscarPaisOptimo

    subroutine generarGrafo(this)
        class(Grafica), intent(in) :: this
        character(len = 20) :: codColor
        type(pais) :: tempPais
        integer :: i, j
        codColor = ""
        i = 1

        ! Abrir el archivo para escritura
        open(unit=11, file='grafo.dot', status='unknown')
        write(11,*) 'digraph G {'
        write(11,*) '    A [label = ' , TRIM(this%nombreGrafica) , '];'
        write(11,*) '    A [shape=Mdiamond];'

        if ( SIZE(this%listaContinentes) > 0 ) then
            do while (i <= SIZE(this%listaContinentes)) ! Recorremos la lista de 
                codColor = codColorObtain(this%listaContinentes(i)%densidad)
                write(11,*) '    ' , TRIM(this%listaContinentes(i)%nombre) , ' [shape=record, fillcolor=' , codColor , ' style=filled];'
                write(11,*) '    ' , TRIM(this%listaContinentes(i)%nombre) , ' [label="{"+' , TRIM(this%listaContinentes(i)%nombre) , '+"| ' , this%listaContinentes(i)%densidad ,'}"];'
                write(11,*) '    A -> ' , TRIM(this%listaContinentes(i)%nombre) , ';'

                j = 1
                do while (j <= SIZE(this%listaContinentes(i)%paises))
                    tempPais = this%listaContinentes(i)%paises(j) ! Pais temporal
                    codColor = codColorObtain(tempPais%densidad)
                    write(11,*) '    ' , TRIM(tempPais%nombre) , ' [shape=record, fillcolor=' , codColor , ' style=filled];'
                    write(11,*) '    ' , TRIM(tempPais%nombre) , ' [label="{"+' , TRIM(tempPais%nombre) ,'+"| ' , tempPais%densidad ,'}"];'
                    write(11,*) '    ' ,TRIM(this%listaContinentes(i)%nombre), ' -> ' , TRIM(tempPais%nombre) , ';'  

                    j = j + 1
                end do
                i = i + 1
            end do
        end if

        write(11,*) '}'
        ! Cerrar el archivo
        close(11)

        ! Llamar a Graphviz para crear la imagen PNG
        call system("dot -Tpng grafo.dot -o GRAFICA.png")

    end subroutine generarGrafo
    
    function codColorObtain(densidad) result(codColor)
        integer, intent(in) :: densidad
        character(len=20) :: codColor
        if ( densidad >= 0 .AND.  densidad <= 15 ) then
            codColor = 'white'
        else if ( densidad >= 16 .AND.  densidad <= 30 ) then
            codColor = 'blue'
            else if ( densidad >= 31 .AND.  densidad <= 45 ) then
            codColor = 'green'
        else if ( densidad >= 46 .AND.  densidad <= 60 ) then
            codColor = 'yellow'
        else if ( densidad >= 61 .AND.  densidad <= 75 ) then
            codColor = 'orange'
        else if ( densidad >= 76 .AND.  densidad <= 100 ) then
            codColor = 'red'
        end if
        
    end function codColorObtain
    
end module ModuloGrafica