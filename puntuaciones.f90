module Puntuaciones
    use GameModule 
    implicit none

    contains

    ! función para sumar las puntuaciones por genero
    subroutine sumatoriaPorGenero(juegos)
        implicit none
        type(Juego), allocatable, intent(in) :: juegos(:)
        integer :: i, j, n 
        real :: sumatoria 
        character(len=100) :: generos(100)
        integer :: n_generos(100)
        integer :: n_generos_unicos
        logical :: found

        ! inicializar variables
        n = size(juegos)
        n_generos_unicos = 0

        ! inicializar el array de generos 
        do i = 1, 100
            generos(i) = ''
            n_generos(i) = 0
        end do

        ! contar los juegos por genero
        do i = 1, n 
            found = .false.
            do j = 1, n_generos_unicos
                if (trim(juegos(i)%genero) == trim(generos(j))) then
                    n_generos(j) = n_generos(j) + 1
                    found = .true.
                end if
            end do 
            if (.not. found) then
                n_generos_unicos = n_generos_unicos + 1
                generos(n_generos_unicos) = trim(juegos(i)%genero)
                n_generos(n_generos_unicos) = 1
            end if
        end do 

        ! imprimir la sumatoria de juegos por genero    
        do i = 1, n_generos_unicos
            print *, 'Genero: ', generos(i), 'Juegos: ', n_generos(i)
        end do
    end subroutine sumatoriaPorGenero

end module Puntuaciones