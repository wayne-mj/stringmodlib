program check
    use iso_fortran_env
    use stringmodlib
implicit none
    integer, allocatable :: a(:)
    character(len=32), allocatable :: input(:) 
    character(len=32), allocatable :: output (:)
    integer :: b;
    integer(int64) :: b64 
    character(len=32) :: bstr = "123"

    a = string_to_int_array("1,2,3,4,1,2,3,4",",")
    print *, a

    a = string_to_int_array("1.2.3.4.1.2.3.4",".")
    print *, a


    print '(A,A,I10)',newline, "Size of A Array before resize:", size(a)
    call resize_array(a,16)
    print '(A,A,I10)',newline, "Size of A Array after resize: ", size(a)

    ! print '(A,A)',newline, "What happens if undersizes array"
    ! call resize_array(a,8)

    allocate(input(3))
    input(1) = "One"
    input(2) = "Three"
    input(3) = "Five"
    print *, ""

    print *, input
    call resize_array(input, output, 5)
    output(4) = "Four"
    output(5) = "Six"
    
    print *, output

    b64  = string_to_integer64("4342223333a")
    print *, b64
end program check
