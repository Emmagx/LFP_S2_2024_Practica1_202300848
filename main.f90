program main
    use InventarioModule
    use GestorInventario
    implicit none
    type(Equipo), allocatable :: inventario(:)
    integer :: opcion
    character(len=100) :: file_name

    do
        print *, 'Menu:'
        print *, '1. Cargar Inventario Inicial'
        print *, '2. Cargar Instrucciones de Movimientos'
        print *, '3. Crear Informe de Inventario'
        print *, '4. Informacion del estudiante'
        print *, '5. Salir'
        print *, 'Ingrese una opcion: '
        read *, opcion

        select case(opcion)
        case(1)
            print *, 'Ingrese el nombre del archivo de inventario: '
            read *, file_name
            call cargarInventario(file_name, inventario)
        case(2)
            print *, 'Ingrese el nombre del archivo de movimientos: '
            read *, file_name
            call procesarMovimientos(file_name, inventario)
        case(3)
            print *, 'Ingrese el nombre del archivo para el informe: '
            read *, file_name
            call crearInforme(file_name, inventario)
        case(4)
            print *, 'Brayan Emanuel Garcia '
            print *, '202300848'
        case(5)
            exit
        case default
            print *, 'Opcion no valida'
        end select
    end do
end program main
