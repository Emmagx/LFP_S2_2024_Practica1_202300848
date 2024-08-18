module leerArchivo 
    use GameModule
    implicit none
    
    contains

    subroutine readFile(max_lineas, contenido, file_name)
        implicit none
        integer, intent(in) :: max_lineas 
        character(len=100), dimension(max_lineas), intent(out) :: contenido
        character(len=100), intent(in) :: file_name
        integer :: unit_num, i, ios
        character(len=100) :: line
        logical :: fin_archivo

        ! inicializar variables
        i = 1 
        fin_archivo = .false.
        ! inicializar el array con cadenas vacias
        contenido = '' 
        ! numero de unidad para el archivo
        unit_num = 10

        ! abrir archivo en modo lectura
        open(unit=unit_num, file=file_name, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error al abrir el archivo'
            stop
        end if

        ! leemos el archivo linea por linea
        do 
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (i <= max_lineas) then
                contenido(i) = trim(line)
                i = i + 1
            end if
        end do 

        ! cerramos el archivo
        close(unit_num)
    end subroutine readFile

    ! función para separar una linea por un delimitador
    subroutine separar(line, delimitador, juegos)
        implicit none
        character(len=100), intent(in) :: line
        character(len=1), intent(in) :: delimitador
        type(Juego), allocatable, intent(inout) :: juegos(:)

        character(len=100) :: nombre, genero, buffer 
        integer :: i, n, column, len_line 
        real :: puntuacion
        type(Juego) :: j

        ! inicializar variables
        n = size(juegos)
        len_line = len_trim(line) ! longitud de la linea
        buffer = ''
        nombre = ''
        genero = ''
        puntuacion = 0.0
        column = 1

        ! recorremos la linea y con ayuda del buffer vamos a separar los datos 
        do i = 1, len_line
            if (line(i:i) == delimitador) then
                if (column == 1) then
                    nombre = trim(buffer)
                else if (column == 2) then
                    read(buffer, *) puntuacion
                end if
                column = column + 1
                buffer = ''
            else 
                buffer = trim(buffer) // line(i:i)
            end if
        end do

        ! añadir el ultimo dato
        if (column == 3) then
            genero = trim(buffer)
        end if

        j%nombre = nombre
        j%puntuacion = puntuacion
        j%genero = genero

        ! realocar el array de juegos y añadir el juego nuevo 
        if (allocated(juegos)) then 
            ! realocar el array
            call extendedArray(juegos)
        else
            allocate(juegos(1))
        end if 
        juegos(n+1) = j
    end subroutine separar

    ! función para extender el array de juegos
    subroutine extendedArray(juegos)
        implicit none 
        type(Juego), allocatable, intent(inout) :: juegos(:)
        type(Juego), allocatable :: temp(:)
        integer :: n 

        n = size(juegos)
        allocate(temp(n+1))
        temp(1:n) = juegos
        deallocate(juegos)
        juegos = temp
    end subroutine extendedArray

end module leerArchivo