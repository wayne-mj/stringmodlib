program check
    use iso_fortran_env
    use stringmodlib
implicit none
    integer, allocatable :: a(:)

    a = string_to_int_array("1,2,3,4,1,2,3,4",",")
    print *, a

    a = string_to_int_array("1.2.3.4.1.2.3.4",".")
    print *, a


    print '(A,A,I10)',newline, "Size of A Array before resize:", size(a)
    call resize_array(a,16)
    print '(A,A,I10)',newline, "Size of A Array after resize: ", size(a)

    print '(A,A)',newline, "What happens if undersizes array"
    call resize_array(a,8)
end program check
