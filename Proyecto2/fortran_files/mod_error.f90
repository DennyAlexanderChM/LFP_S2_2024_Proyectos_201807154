module ModuloError
    
    use ModuloToken

    implicit none

    type, public :: Error_type

        integer :: fila, columna

        character(len=100) :: tipo_error, token_esperado, descripcion, lexema_encontrado

    end type Error_type

    type, public :: Error_List

        type(Error_type), dimension(:), allocatable :: lista_errores
        
    contains

    procedure :: text_error
    procedure :: agregar_error
    procedure :: agregar_error_lexico

    end type Error_List

    type(Error_List) :: errores ! Variable que almacena los errores encontrados

contains

    subroutine agregar_error_lexico(this, comentario, lexema, fila, columna) ! Agrega el Token a la lista
        class(Error_List), intent(inout) :: this
        character(len = :), intent(in), allocatable :: comentario, lexema
        integer, intent(in) :: fila, columna
        type(Error_type), dimension(:), allocatable :: temp ! Lista temporal de dimensión indefinida
        
        integer :: i

        if ( allocated(this%lista_errores) ) then ! Verifica si la lista tiene dimension definida
            i = SIZE(this%lista_errores) + 1 ! Obtenemos su dimension + 1
            allocate(temp(i-1)) ! Definimos la dimension de la lista temporal
            temp = this%lista_errores ! Copiamos la lista de tokens a la lista temporal
            deallocate(this%lista_errores) ! Liberamos el espacio en memoria de la lista de tokens
            allocate(this%lista_errores(i)) ! Asignamos la nueva dimensión de la lista de tokens
            this%lista_errores(1:i-1) = temp ! Copiamos los datos almacenados en lista temporal a la lista de tokens
            deallocate(temp) ! Liberamos el espacio en memoria de la lista temporal
        else
            i = 1 ! Si la lista no tiene ningun elemento
            allocate(this%lista_errores(i))
        end if
        ! Asignamos calores al elemento de la lista
        this%lista_errores(i)%lexema_encontrado = TRIM(lexema)
        this%lista_errores(i)%tipo_error = 'Lexico'
        this%lista_errores(i)%fila = fila
        this%lista_errores(i)%columna = columna
        this%lista_errores(i)%descripcion = TRIM(comentario)

    end subroutine agregar_error_lexico

    subroutine agregar_error(this, token_actual, comentario) ! Agrega el Token a la lista
        class(Error_List), intent(inout) :: this
        character(len = 100), intent(in) :: comentario
        type(Token), intent(in) :: token_actual
        type(Error_type), dimension(:), allocatable :: temp ! Lista temporal de dimensión indefinida
        
        integer :: i

        if ( allocated(this%lista_errores) ) then ! Verifica si la lista tiene dimension definida
            i = SIZE(this%lista_errores) + 1 ! Obtenemos su dimension + 1
            allocate(temp(i-1)) ! Definimos la dimension de la lista temporal
            temp = this%lista_errores ! Copiamos la lista de tokens a la lista temporal
            deallocate(this%lista_errores) ! Liberamos el espacio en memoria de la lista de tokens
            allocate(this%lista_errores(i)) ! Asignamos la nueva dimensión de la lista de tokens
            this%lista_errores(1:i-1) = temp ! Copiamos los datos almacenados en lista temporal a la lista de tokens
            deallocate(temp) ! Liberamos el espacio en memoria de la lista temporal
        else
            i = 1 ! Si la lista no tiene ningun elemento
            allocate(this%lista_errores(i))
        end if
        ! Asignamos calores al elemento de la lista
        this%lista_errores(i)%lexema_encontrado = TRIM(token_actual%lexema)
        this%lista_errores(i)%tipo_error = 'Sintactico'
        this%lista_errores(i)%fila = token_actual%position_y
        this%lista_errores(i)%columna = token_actual%position_x
        this%lista_errores(i)%descripcion = TRIM(comentario)

    end subroutine agregar_error

    subroutine text_error(this) ! Crea el archivo html con los datos de los tokens
        class(Error_List), intent(inout) :: this
        integer :: i, num_error
        num_error = 1
        
        if ( SIZE(this%lista_errores) > 0 ) then

            ! Abrir el archivo para escribir (crea el archivo si no existe)
            open(unit=10, file="errores.txt", status="replace", action="write", form="formatted")
          
            do i = 1, SIZE(this%lista_errores)

                    write(10, *) TRIM(this%lista_errores(i)%tipo_error),'&&', this%lista_errores(i)%fila ,'&&', this%lista_errores(i)%columna ,'&&', TRIM(this%lista_errores(i)%lexema_encontrado) ,'&&', TRIM(this%lista_errores(i)%descripcion)

                    num_error = num_error + 1 

            end do
            
            close(10)
            
        end if
        
    end subroutine text_error
    
end module ModuloError