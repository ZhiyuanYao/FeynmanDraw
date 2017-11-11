module string
    implicit none

    interface str
        module procedure strInt4
        module procedure strInt8
        module procedure strReal
        module procedure strDouble
    end interface

contains

    ! Fortran2003 has variable character length feature.
    !     character(len = :), allocatable :: strout
    ! declares variable length character variable
    function strInt4(i) result(strout)
        integer, intent(in) :: i
        character(:), allocatable :: strout
        character(range(i)+2) :: strtmp
        write(strtmp,'(I0)') i
        strout = trim(strtmp)
    end function


    function strInt8(i) result(strout)
        integer(kind=8), intent(in) :: i
        character(:), allocatable :: strout
        character(range(i)+2) :: strtmp
        write(strtmp,'(I0)') i
        strout = trim(strtmp)
    end function

    function strReal(fnum) result(strout)
        real, intent(in) :: fnum
        character(:), allocatable :: strout
        character(len=10) :: strtmp
        write (strtmp, "(F10.4)") fnum
        strtmp = adjustl(strtmp)
        strout = trim(strtmp)
    end function

    function strDouble(fnum) result(strout)
        double precision, intent(in) :: fnum
        character(:), allocatable :: strout
        character(len=10) :: strtmp
        write (strtmp, "(F10.4)") fnum
        strtmp = adjustl(strtmp)
        strout = trim(strtmp)
    end function

end module string
