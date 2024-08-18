module GameModule 
    implicit none
    
    type :: Juego 
        character(len=100) :: nombre
        real :: puntuacion
        character(len=100) :: genero
    end type Juego

end module GameModule