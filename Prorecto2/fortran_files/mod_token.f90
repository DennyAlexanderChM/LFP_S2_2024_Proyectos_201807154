module ModuloToken
    implicit none
    public RESERVADA_CONTROLES, RESERVADA_ETIQUETA, RESERVADA_BOTON, RESERVADA_CHECK, RESERVADA_RADIOBOTON, RESERVADA_TEXTO, &
            RESERVADA_AREATEXTO, RESERVADA_CLAVE, RESERVADA_CONTENEDOR, RESERVADA_PROPIEDADES, RESERVADA_PROPIEDAD_COLOR_LETRA, RESERVADA_PROPIEDAD_TEXTO, &
            RESERVADA_PROPIEDAD_ALINEACION, RESERVADA_PROPIEDAD_COLOR_FONDO, RESERVADA_PROPIEDAD_MARCADA, RESERVADA_PROPIEDAD_GRUPO, RESERVADA_PROPIEDAD_ANCHO,&
            RESERVADA_PROPIEDAD_ALTO, RESERVADA_PROPIEDAD_POSICION, RESERVADA_COLOCACION, RESERVADA_THIS, RESERVADA_ADD, SIGNO_PUNTO, SIGNO_COMA, SIGNO_PUNTO_COMA,&
            SIGNO_MAYOR, SIGNO_MENOR,SIGNO_EXCLAMACION, SIGNO_GUION, PARENTESIS_ABRE, PARENTESIS_CIERRA, COMENTARIO, ENTERO, IDENTIFICADOR, RESERVADA_CADENA, ERROR, RESERVADA_FALSE,&
            RESERVADA_TRUE, RESERVADA_IZQUIERDA, RESERVADA_CENTRO, RESERVADA_DERECHA
    type ::Token ! Type Token
        character(len = 100) :: lexema
        integer :: type
        integer :: position_x
        integer :: position_y
    end type Token
    
    integer, parameter :: RESERVADA_CONTROLES = 1
    integer, parameter :: RESERVADA_ETIQUETA = 2
    integer, parameter :: RESERVADA_BOTON = 3
    integer, parameter :: RESERVADA_CHECK = 4
    integer, parameter :: RESERVADA_RADIOBOTON = 5
    integer, parameter :: RESERVADA_TEXTO = 6
    integer, parameter :: RESERVADA_AREATEXTO = 7
    integer, parameter :: RESERVADA_CLAVE = 8
    integer, parameter :: RESERVADA_CONTENEDOR = 9
    integer, parameter :: RESERVADA_PROPIEDADES = 10
    integer, parameter :: RESERVADA_PROPIEDAD_COLOR_LETRA = 11
    integer, parameter :: RESERVADA_PROPIEDAD_TEXTO = 12
    integer, parameter :: RESERVADA_PROPIEDAD_ALINEACION = 13
    integer, parameter :: RESERVADA_PROPIEDAD_COLOR_FONDO = 14
    integer, parameter :: RESERVADA_PROPIEDAD_MARCADA = 15
    integer, parameter :: RESERVADA_PROPIEDAD_GRUPO = 16
    integer, parameter :: RESERVADA_PROPIEDAD_ANCHO = 17
    integer, parameter :: RESERVADA_PROPIEDAD_ALTO = 18
    integer, parameter :: RESERVADA_PROPIEDAD_POSICION = 19
    integer, parameter :: RESERVADA_COLOCACION = 20
    integer, parameter :: RESERVADA_THIS = 21
    integer, parameter :: RESERVADA_ADD = 22
    integer, parameter :: SIGNO_PUNTO = 23
    integer, parameter :: SIGNO_COMA = 24
    integer, parameter :: SIGNO_PUNTO_COMA = 25
    integer, parameter :: SIGNO_MAYOR = 26
    integer, parameter :: SIGNO_MENOR = 27
    integer, parameter :: SIGNO_EXCLAMACION = 28
    integer, parameter :: SIGNO_GUION = 29
    integer, parameter :: PARENTESIS_ABRE = 30
    integer, parameter :: PARENTESIS_CIERRA = 31
    integer, parameter :: COMENTARIO = 32
    integer, parameter :: ENTERO = 33
    integer, parameter :: IDENTIFICADOR = 34
    integer, parameter :: RESERVADA_CADENA = 35
    integer, parameter :: ERROR = 36
    integer, parameter :: RESERVADA_TRUE = 37
    integer, parameter :: RESERVADA_FALSE = 38
    integer, parameter :: RESERVADA_IZQUIERDA = 39
    integer, parameter :: RESERVADA_CENTRO = 40
    integer, parameter :: RESERVADA_DERECHA = 41
    integer, parameter :: POSICION_X = 42
    integer, parameter :: POSICION_Y = 43


contains

    
end module ModuloToken