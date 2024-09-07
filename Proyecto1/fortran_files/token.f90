module TokenModule
    implicit none
    private
    public :: Token
    type ::Token
        character(len = 100) :: lexema
        character(len = 100) :: type
        integer :: position_x
        integer :: posicion_y
    contains

    end type Token

contains
    
end module TokenModule