program check
    use iso_fortran_env
    use stringmodlib
implicit none
    integer, allocatable :: a(:)

    a = string_to_int_array("102030401020304","0")
    print *, a
end program check
