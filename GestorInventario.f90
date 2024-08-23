module GestorInventario
    use InventarioModule
    implicit none
    
    contains

    subroutine cargarInventario(file_name, inventario) 
        implicit none
        character(len=100), intent(in) :: file_name
        type(Equipo), allocatable, intent(out) :: inventario(:)
        character(len=100) :: line
        integer :: ios, unit_num

        unit_num = 10
        open(unit=unit_num, file=file_name, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error al abrir el archivo', file_name
            stop
        end if

        print *, 'Archivo de inventario abierto exitosamente.'

        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (trim(line) /= '') then
                print *, 'Leyendo linea de inventario:', line
                ! Llama a procesarInstruccion para procesar cada línea
                call procesarInstruccion(line, ';', inventario)
            end if
        end do

        close(unit_num)
        print *, 'Cierre del archivo de inventario.'
    end subroutine cargarInventario

    subroutine procesarMovimientos(file_name, inventario)
        implicit none
        character(len=100), intent(in) :: file_name
        type(Equipo), allocatable, intent(inout) :: inventario(:)
        character(len=100) :: line
        integer :: ios, unit_num

        unit_num = 11
        open(unit=unit_num, file=file_name, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error al abrir el archivo', file_name
            stop
        end if

        print *, 'Archivo de movimientos abierto exitosamente.'

        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (trim(line) /= '') then
                print *, 'Leyendo linea de movimientos:', line
                call procesarInstruccion(line, ';', inventario)
            end if
        end do

        close(unit_num)
        print *, 'Cierre del archivo de movimientos.'
    end subroutine procesarMovimientos

    subroutine extendedArray(inventario)
        implicit none
        type(Equipo), allocatable, intent(inout) :: inventario(:)
        type(Equipo), allocatable :: temp(:)
        integer :: n, new_size

        n = size(inventario)
        new_size = n+1 ! Incrementar el tamaño en un valor fijo, como 10

        print *, 'Redimensionando arreglo de tamaño', n, 'a', new_size
        allocate(temp(new_size))
        temp(1:n) = inventario
        deallocate(inventario)
        inventario = temp
    end subroutine extendedArray

subroutine procesarInstruccion(line, delimitador, inventario)
    implicit none
    character(len=100), intent(in) :: line
    character(len=1), intent(in) :: delimitador
    type(Equipo), allocatable, intent(inout) :: inventario(:)
    character(len=100) :: operacion
    character(len=100), allocatable :: datos(:)
    integer :: cantidad, i, n, ios
    real :: precio_unitario
    logical :: found, ubicacion_correcta

    ! Inicializar variables
    operacion = ''
    cantidad = 0
    precio_unitario = 0.0
    found = .false.
    ubicacion_correcta = .false.

    ! Separar los datos usando delimitador
    call separarDatos(line, delimitador, operacion, datos)

    print *, 'Operacion:', operacion
    print *, 'Datos:', datos

    ! Verifica si se ha leído correctamente la operación y los datos
    if (trim(operacion) == '') then
        print *, 'Error: No se ha encontrado una operación válida.'
        if (allocated(datos)) deallocate(datos)
        return
    end if

    ! Procesar datos según la operación
    n = size(inventario)

    if (operacion == 'crear_equipo') then
        ! Extraer cantidad y precio_unitario de los datos
        call obtenerCantidadPrecio(datos, cantidad, precio_unitario)

        ! Crear nuevo equipo y agregarlo al inventario
        if (allocated(inventario)) then
            call extendedArray(inventario)
        else
            allocate(inventario(1))
        end if

        ! Agregar el nuevo equipo
        n = size(inventario)
        inventario(n)%nombre = datos(1)
        inventario(n)%cantidad = cantidad
        inventario(n)%precio_unitario = precio_unitario
        inventario(n)%ubicacion = datos(4)

        print *, 'Equipo creado:'
        print *, 'Nombre:', inventario(n)%nombre
        print *, 'Cantidad:', inventario(n)%cantidad
        print *, 'Precio Unitario:', inventario(n)%precio_unitario
        print *, 'Ubicacion:', inventario(n)%ubicacion

    else if (operacion == 'agregar_stock') then
        ! Solo lee la cantidad
        read(datos(2), *, iostat=ios) cantidad
        if (ios /= 0) then
            print *, 'Error al leer cantidad:', trim(datos(2))
            stop
        end if

        ! Buscar el equipo en el inventario y agregar stock si la ubicación es correcta
        do i = 1, n
            if (trim(inventario(i)%nombre) == trim(datos(1))) then
                found = .true.
                if (trim(inventario(i)%ubicacion) == trim(datos(3))) then
                    inventario(i)%cantidad = inventario(i)%cantidad + cantidad
                    ubicacion_correcta = .true.
                    exit
                else
                    print *, 'Error: La ubicación no coincide para el equipo', trim(datos(1))
                end if
            end if
        end do

        if (.not. found) then
            print *, 'Error: No se encontró el equipo para agregar stock.'
        end if
        if (found .and. .not. ubicacion_correcta) then
            print *, 'Error: La ubicación no coincide para el equipo', trim(datos(1))
        end if

    else if (operacion == 'eliminar_equipo') then
        ! Solo lee la cantidad
        read(datos(2), *, iostat=ios) cantidad
        if (ios /= 0) then
            print *, 'Error al leer cantidad:', trim(datos(2))
            stop
        end if

        ! Buscar el equipo en el inventario y eliminar stock si la ubicación es correcta
        do i = 1, n
            if (trim(inventario(i)%nombre) == trim(datos(1))) then
                found = .true.
                if (trim(inventario(i)%ubicacion) == trim(datos(3))) then
                    if (inventario(i)%cantidad >= cantidad) then
                        inventario(i)%cantidad = inventario(i)%cantidad - cantidad
                    else
                        print *, 'Error: Cantidad a eliminar excede la cantidad disponible.'
                    end if
                    if (inventario(i)%cantidad < 0) inventario(i)%cantidad = 0
                    ubicacion_correcta = .true.
                    exit
                else
                    print *, 'Error: La ubicación no coincide para el equipo', trim(datos(1))
                end if
            end if
        end do

        if (.not. found) then
            print *, 'Error: No se encontró el equipo para eliminar stock.'
        end if
        if (found .and. .not. ubicacion_correcta) then
            print *, 'Error: La ubicación no coincide para el equipo', trim(datos(1))
        end if

    else
        print *, 'Error: Operación no válida:', operacion
    end if

    if (allocated(datos)) deallocate(datos)
end subroutine procesarInstruccion


    subroutine separarDatos(line, delimitador, operacion, datos)
        implicit none
        character(len=100), intent(in) :: line
        character(len=1), intent(in) :: delimitador
        character(len=100), intent(out) :: operacion
        character(len=100), allocatable, intent(out) :: datos(:)
        integer :: pos_delim, num_palabras, i
        character(len=100) :: buffer
        character(len=100) :: temp

        ! Inicializar variables
        operacion = ''
        buffer = trim(line)
        num_palabras = 1

        ! Separar la operación (antes del primer espacio)
        pos_delim = index(buffer, ' ')
        if (pos_delim > 0) then
            operacion = trim(buffer(1:pos_delim-1))
            buffer = trim(buffer(pos_delim+1:))
        else
            operacion = trim(buffer)
            return
        end if

        ! Contar el número de delimitadores para determinar el tamaño de 'datos'
        do i = 1, len(buffer)
            if (buffer(i:i) == delimitador) then
                num_palabras = num_palabras + 1
            end if
        end do

        ! Asignar memoria para datos
        allocate(datos(num_palabras))

        ! Separar los datos por el delimitador
        i = 1
        do
            pos_delim = index(buffer, delimitador)
            if (pos_delim > 0) then
                datos(i) = trim(buffer(1:pos_delim-1))
                buffer = trim(buffer(pos_delim+1:))
                i = i + 1
            else
                datos(i) = trim(buffer)
                exit
            end if
        end do

        print *, 'Operacion:', operacion
        print *, 'Datos:', datos
    end subroutine separarDatos

    subroutine obtenerCantidadPrecio(datos, cantidad, precio_unitario)
        implicit none
        character(len=100), dimension(:), intent(in) :: datos
        integer, intent(out) :: cantidad
        real, intent(out) :: precio_unitario
        integer :: ios

        ! Inicializar valores por defecto
        cantidad = 0
        precio_unitario = 0.0

        ! Leer cantidad
        if (len_trim(datos(2)) > 0) then
            read(datos(2), *, iostat=ios) cantidad
            if (ios /= 0) then
                print *, 'Error al leer cantidad:', trim(datos(2))
                stop
            end if
        else
            print *, 'Error: Cantidad no proporcionada.'
            stop
        end if

        ! Leer precio_unitario si está disponible
        if (size(datos) > 2 .and. len_trim(datos(3)) > 0) then
            read(datos(3), *, iostat=ios) precio_unitario
            if (ios /= 0) then
                print *, 'Error al leer precio_unitario:', trim(datos(3))
                stop
            end if
        end if
    end subroutine obtenerCantidadPrecio

    subroutine crearInforme(file_name, inventario)
        implicit none
        character(len=100), intent(in) :: file_name
        type(Equipo), allocatable, intent(in) :: inventario(:)
        integer :: unit_num, i
        real :: valor_total
        character(len=100) :: separator
        logical :: valid_entry
        unit_num = 12

        open(unit=unit_num, file=file_name, status='unknown', action='write')
        separator = '-------------------------------------------------------------------------'
        print *, 'Creando informe en el archivo:', file_name

        ! Escribir encabezado
        write(unit_num, '(A)') 'Informe de Inventario:'
        write(unit_num, '(A20, A20, A20, A20, A10)') &
            'Equipo', 'Cantidad', 'Precio Unitario', 'Valor Total', 'Ubicacion'
        write(unit_num, '(A)') separator
        do i = 1, size(inventario)
            print *, 'Item', i
            print *, 'Nombre:', trim(adjustl(inventario(i)%nombre))
            print *, 'Cantidad:', inventario(i)%cantidad
            print *, 'Precio Unitario:', inventario(i)%precio_unitario
            print *, 'Ubicacion:', trim(adjustl(inventario(i)%ubicacion))
        end do

        ! Escribir cada entrada de inventario
        do i = 1, size(inventario)
            print *, 'Processing entry:', i
            print *, 'Nombre:', trim(adjustl(inventario(i)%nombre))
            print *, 'Cantidad:', inventario(i)%cantidad
            print *, 'Precio Unitario:', inventario(i)%precio_unitario

            valid_entry = trim(adjustl(inventario(i)%nombre)) /= '' .and. &
                        inventario(i)%cantidad > 0 .and. &
                        inventario(i)%precio_unitario >= 0.00

            if (valid_entry) then
                valor_total = inventario(i)%cantidad * inventario(i)%precio_unitario
                write(unit_num, '(A20, I8, F14.2, F14.2, A10)') &
                    trim(adjustl(inventario(i)%nombre)), &
                    inventario(i)%cantidad, &
                    inventario(i)%precio_unitario, &
                    valor_total, &
                    trim(adjustl(inventario(i)%ubicacion))
            end if
        end do

        close(unit_num)
        print *, 'Informe creado exitosamente.'
    end subroutine crearInforme


end module GestorInventario
