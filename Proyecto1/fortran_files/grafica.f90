module GraficaModulo
    use ContinenteModulo
    implicit none
    type, public :: Grafica
        character(len=100) :: nombreGrafica
        integer :: numContinentes
        type(Continente), dimension(:), allocatable :: listaContinentes
    
    contains
        procedure :: addContinente
        procedure :: addPaisContinente
        procedure :: printDates
        
    end type Grafica
    
contains
    subroutine addContinente(this,  nombreContinente)
        class(Grafica), intent(inout) :: this
        class(Continente), allocatable :: temp(:)
        character(len = 100) :: nombreContinente
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
    
end module GraficaModulo