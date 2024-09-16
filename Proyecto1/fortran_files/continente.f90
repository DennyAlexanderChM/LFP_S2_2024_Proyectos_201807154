module ModuloContinente
    implicit none
    public :: Pais, Continente
    type :: Pais
        character(len=100) :: nombre
        integer :: poblacion
        integer :: densidad
        character(len=100) :: bandera
    end type  Pais

    type :: Continente
        character(len=100) :: nombre
        integer :: densidad
        type(Pais), dimension(:), allocatable :: paises
    contains
        procedure :: addPais
        procedure :: densidadMedia
    
    end type Continente
contains
    subroutine addPais(this,  nombrePais, poblacion, saturacion, bandera)
        class(Continente), intent(inout) :: this
        character(len=100), intent(in) :: nombrePais, bandera
        integer, intent(in) :: poblacion, saturacion
        class(Pais), allocatable :: temp(:)
        integer i

        if ( allocated(this%paises) ) then
            i = SIZE(this%paises) + 1
            allocate(temp(i-1))
            temp = this%paises
            deallocate(this%paises)
            allocate(this%paises(i))
            this%paises(1:i-1) = temp
            deallocate(temp)
        else
            i = 1
            allocate(this%paises(i))
        end if

        this%paises(i)%nombre = nombrePais
        this%paises(i)%poblacion = poblacion
        this%paises(i)%densidad =  saturacion
        this%paises(i)%bandera = bandera 
        
    end subroutine addPais

    subroutine densidadMedia(this)
        class(Continente), intent(inout) :: this
        integer :: i, media
        media = 0
        i = 1

        do while (i <= SIZE(this%paises))

            media = media + this%paises(i)%densidad
            
            i = i + 1
        end do

        this%densidad = media/SIZE(this%paises)
    
    end subroutine densidadMedia
    
end module ModuloContinente