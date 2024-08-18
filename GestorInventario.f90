module GestorInventario
    use InventarioModule
    implicit none
    
    contains

    subroutine cargarInventario(file_name, inventario) 
        implicit none
        character(len=100), intent(in) :: file_name
        type(Equipo), allocatable, intent(out) :: inventario(:)
        character(len=100) :: line
        integer :: ios, unit_num, n
        type(Equipo) :: equipo_tmp

        unit_num = 10
        open(unit=unit_num, file=file_name, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error al abrir el archivo', file_name
            stop
        end if

        print *, 'Archivo de inventario abierto exitosamente.'

        n = 0
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (trim(line) /= '') then
                print *, 'Leyendo linea de inventario:', line
                call separar(line, ';', equipo_tmp)
                print *, 'Equipo leido:'
                print *, 'Nombre:', equipo_tmp%nombre
                print *, 'Cantidad:', equipo_tmp%cantidad
                print *, 'Precio Unitario:', equipo_tmp%precio_unitario
                print *, 'Ubicacion:', equipo_tmp%ubicacion
                if (allocated(inventario)) then
                    call extendedArray(inventario)
                else
                    allocate(inventario(1))
                end if
                n = size(inventario)
                inventario(n) = equipo_tmp
                print *, 'Equipo agregado:'
                print *, 'Nombre:', inventario(n)%nombre
                print *, 'Cantidad:', inventario(n)%cantidad
                print *, 'Precio Unitario:', inventario(n)%precio_unitario
                print *, 'Ubicacion:', inventario(n)%ubicacion
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
        new_size = n + 10  ! Incrementar el tamaño en un valor fijo, como 10

        print *, 'Redimensionando arreglo de tamaño', n, 'a', new_size
        allocate(temp(new_size))
        temp(1:n) = inventario
        deallocate(inventario)
        inventario = temp
    end subroutine extendedArray

    subroutine extendedArrayCaracteres(array)
        implicit none
        character(len=100), allocatable, intent(inout) :: array(:)
        character(len=100), allocatable :: temp(:)
        integer :: n, new_size

        n = size(array)
        new_size = n + 10  ! Incrementar el tamaño en un valor fijo, como 10

        print *, 'Redimensionando arreglo de tamaño', n, 'a', new_size
        allocate(temp(new_size))
        temp(1:n) = array
        deallocate(array)
        array = temp
    end subroutine extendedArrayCaracteres

    subroutine procesarInstruccion(line, delimitador, inventario)
        implicit none
        character(len=100), intent(in) :: line
        character(len=1), intent(in) :: delimitador
        type(Equipo), allocatable, intent(inout) :: inventario(:)
        character(len=100) :: operacion
        character(len=100), allocatable :: datos(:)
        integer :: cantidad, i, n, ios
        logical :: found

        ! Inicializar variables
        operacion = ''
        cantidad = 0
        found = .false.

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

        if (operacion == 'agregar_stock') then
            ! Ahora solo lee la cantidad, ya que el precio_unitario no es necesario
            read(datos(2), *, iostat=ios) cantidad
            if (ios /= 0) then
                print *, 'Error al leer cantidad:', trim(datos(2))
                stop
            end if

            do i = 1, n
                if (trim(inventario(i)%nombre) == trim(datos(1))) then
                    inventario(i)%cantidad = inventario(i)%cantidad + cantidad
                    found = .true.
                    exit
                end if
            end do

            if (.not. found) then
                print *, 'Error: No se encontró el equipo para agregar stock.'
            end if
        else if (operacion == 'eliminar_equipo') then
            ! Ahora solo lee la cantidad
            read(datos(2), *, iostat=ios) cantidad
            if (ios /= 0) then
                print *, 'Error al leer cantidad:', trim(datos(2))
                stop
            end if

            do i = 1, n
                if (trim(inventario(i)%nombre) == trim(datos(1))) then
                    inventario(i)%cantidad = inventario(i)%cantidad - cantidad
                    if (inventario(i)%cantidad < 0) inventario(i)%cantidad = 0
                    found = .true.
                    exit
                end if
            end do

            if (.not. found) then
                print *, 'Error: No se encontró el equipo para eliminar.'
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

    subroutine reshapeDatos(datos, num_palabras)
        implicit none
        character(len=100), allocatable, intent(inout) :: datos(:)
        integer, intent(in) :: num_palabras
        character(len=100), allocatable :: temp(:)
        
        if (size(datos) > num_palabras) then
            allocate(temp(num_palabras))
            temp = datos(1:num_palabras)
            deallocate(datos)
            datos = temp
        end if
    end subroutine reshapeDatos

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

        unit_num = 12
        open(unit=unit_num, file=file_name, status='replace', action='write')

        print *, 'Creando informe en el archivo:', file_name

        write(unit_num, '(A)') 'Informe de Inventario:'
        write(unit_num, '(A)') 'Equipo    Cantidad    Precio Unitario    Valor Total    Ubicacion'
        do i = 1, size(inventario)
            valor_total = inventario(i)%cantidad * inventario(i)%precio_unitario
            write(unit_num, '(A, I6, F8.2, F10.2, A)') trim(inventario(i)%nombre) // '    ', &
                inventario(i)%cantidad, inventario(i)%precio_unitario, valor_total, &
                trim(inventario(i)%ubicacion)
        end do

        close(unit_num)
        print *, 'Informe creado exitosamente.'
    end subroutine crearInforme

    subroutine separar(line, delimitador, equipo_tmp)
        implicit none
        character(len=100), intent(in) :: line
        character(len=1), intent(in) :: delimitador
        type(Equipo), intent(out) :: equipo_tmp
        character(len=100), allocatable :: tokens(:)
        integer :: i, pos
        character(len=100) :: temp

        print *, 'Separando la linea:', line

        temp = trim(line)
        pos = 1

        ! Establecer un tamaño máximo para `tokens` (puedes ajustar este valor según sea necesario)
        allocate(tokens(4))

        do i = 1, 4
            if (index(temp, delimitador) > 0) then
                tokens(i) = trim(adjustl(temp(1:index(temp, delimitador) - 1)))
                temp = trim(adjustl(temp(index(temp, delimitador) + 1:)))
            else
                tokens(i) = trim(adjustl(temp))
                exit
            end if
        end do

        equipo_tmp%nombre = tokens(1)
        read(tokens(2), *) equipo_tmp%cantidad
        read(tokens(3), *) equipo_tmp%precio_unitario
        equipo_tmp%ubicacion = tokens(4)

        print *, 'Equipo separado:', equipo_tmp%nombre, equipo_tmp%cantidad, equipo_tmp%precio_unitario, equipo_tmp%ubicacion
    end subroutine separar

end module GestorInventario
