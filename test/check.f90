program check
    use iso_fortran_env
    use stringmodlib
implicit none
    integer, allocatable :: a(:)

    a = string_to_int_array("1,2,3,4,1,2,3,4",",")
    print *, a

    a = string_to_int_array("1.2.3.4.1.2.3.4",".")
    print *, a
end program check
