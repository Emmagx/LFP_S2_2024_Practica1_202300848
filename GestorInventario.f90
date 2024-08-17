module GestorInventario
    use InventarioModule
    implicit none

    contains

    subroutine cargarInventario(file_name, inventario)
        implicit none
        character(len=100), intent(in) :: file_name
        type(Equipo), allocatable, intent(out) :: inventario(:)
        character(len=100) :: line
        integer :: ios, unit_num, i, n
        character(len=1) :: delimitador
        type(Equipo) :: equipo

        delimitador = ';'
        unit_num = 10
        open(unit=unit_num, file=file_name, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error al abrir el archivo', file_name
            stop
        end if

        n = 0
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (trim(line) /= '') then
                call separar(line, delimitador, equipo)
                if (allocated(inventario)) then
                    call extendedArray(inventario)
                else
                    allocate(inventario(1))
                end if
                n = size(inventario)
                inventario(n) = equipo
            end if
        end do

        close(unit_num)
    end subroutine cargarInventario

    subroutine procesarMovimientos(file_name, inventario)
        implicit none
        character(len=100), intent(in) :: file_name
        type(Equipo), allocatable, intent(inout) :: inventario(:)
        character(len=100) :: line
        integer :: ios, unit_num
        character(len=1) :: delimitador

        delimitador = ';'
        unit_num = 11
        open(unit=unit_num, file=file_name, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error al abrir el archivo', file_name
            stop
        end if

        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (trim(line) /= '') then
                call procesarInstruccion(line, delimitador, inventario)
            end if
        end do

        close(unit_num)
    end subroutine procesarMovimientos

    subroutine procesarInstruccion(line, delimitador, inventario)
        implicit none
        character(len=100), intent(in) :: line
        character(len=1), intent(in) :: delimitador
        type(Equipo), allocatable, intent(inout) :: inventario(:)
        character(len=100) :: operacion, nombre, buffer
        integer :: cantidad, i, n, len_line, index
        real :: precio_unitario
        logical :: found

        len_line = len_trim(line)
        buffer = ''
        operacion = ''
        nombre = ''
        cantidad = 0
        precio_unitario = 0.0
        index = 1

        do i = 1, len_line
            if (line(i:i) == delimitador) then
                select case(index)
                case(1)
                    operacion = trim(buffer)
                case(2)
                    nombre = trim(buffer)
                case(3)
                    read(buffer, *) cantidad
                case(4)
                    read(buffer, *) precio_unitario
                end select
                buffer = ''
                index = index + 1
            else
                buffer = trim(buffer) // line(i:i)
            end if
        end do
        if (index == 4) then
            read(buffer, *) precio_unitario
        end if

        n = size(inventario)
        found = .false.

        if (operacion == 'agregar_stock') then
            do i = 1, n
                if (trim(inventario(i)%nombre) == nombre) then
                    inventario(i)%cantidad = inventario(i)%cantidad + cantidad
                    found = .true.
                    exit
                end if
            end do
            if (.not. found) then
                print *, 'Error: No se encontró el equipo para agregar stock.'
            end if
        else if (operacion == 'eliminar_equipo') then
            do i = 1, n
                if (trim(inventario(i)%nombre) == nombre) then
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
            print *, 'Error: Operación no válida.'
        end if
    end subroutine procesarInstruccion

    subroutine extendedArray(inventario)
        implicit none
        type(Equipo), allocatable, intent(inout) :: inventario(:)
        type(Equipo), allocatable :: temp(:)
        integer :: n

        n = size(inventario)
        allocate(temp(n+1))
        temp(1:n) = inventario
        deallocate(inventario)
        inventario = temp
    end subroutine extendedArray

    subroutine crearInforme(file_name, inventario)
        implicit none
        character(len=100), intent(in) :: file_name
        type(Equipo), allocatable, intent(in) :: inventario(:)
        integer :: unit_num, i
        real :: valor_total

        unit_num = 12
        open(unit=unit_num, file=file_name, status='replace', action='write')

        write(unit_num, '(A)') 'Informe de Inventario:'
        write(unit_num, '(A)') 'Equipo    Cantidad    Precio Unitario    Valor Total    Ubicación'
        do i = 1, size(inventario)
            valor_total = inventario(i)%cantidad * inventario(i)%precio_unitario
            write(unit_num, '(A, I6, F8.2, F10.2, A)') trim(inventario(i)%nombre), inventario(i)%cantidad, &
                inventario(i)%precio_unitario, valor_total, trim(inventario(i)%ubicacion)
        end do

        close(unit_num)
    end subroutine crearInforme

    subroutine separar(line, delimitador, equipo)
        implicit none
        character(len=100), intent(in) :: line
        character(len=1), intent(in) :: delimitador
        type(Equipo), intent(out) :: equipo
        character(len=100) :: buffer
        integer :: i, len_line, index

        len_line = len_trim(line)
        buffer = ''
        index = 1

        do i = 1, len_line
            if (line(i:i) == delimitador) then
                select case(index)
                case(1)
                    equipo%nombre = trim(buffer)
                case(2)
                    read(buffer, *) equipo%cantidad
                case(3)
                    read(buffer, *) equipo%precio_unitario
                case(4)
                    equipo%ubicacion = trim(buffer)
                end select
                buffer = ''
                index = index + 1
            else
                buffer = trim(buffer) // line(i:i)
            end if
        end do

        if (index == 4) then
            equipo%ubicacion = trim(buffer)
        end if
    end subroutine separar

end module GestorInventario
