program main
    use InventarioModule
    use GestorInventario
    implicit none
    type(Equipo), allocatable :: inventario(:)
    integer :: opcion
    character(len=100) :: file_name

    do
        print *, 'Menú:'
        print *, '1. Cargar Inventario Inicial'
        print *, '2. Cargar Instrucciones de Movimientos'
        print *, '3. Crear Informe de Inventario'
        print *, '4. Salir'
        print *, 'Ingrese una opción: '
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
            exit
        case default
            print *, 'Opción no válida'
        end select
    end do
end program main
