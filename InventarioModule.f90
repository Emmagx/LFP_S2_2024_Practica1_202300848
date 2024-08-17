module InventarioModule
    implicit none
    
    type :: Equipo
        character(len=100) :: nombre
        integer :: cantidad
        real :: precio_unitario
        character(len=100) :: ubicacion
    end type Equipo

end module InventarioModule