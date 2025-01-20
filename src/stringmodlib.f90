module stringmodlib
  use iso_fortran_env
  implicit none
  private

  public :: string_to_int_array, resize_array, &
            string_to_integer32, string_to_integer64, &
            string_contains, string_location, &
            cr, newline, esc, NULL
            

  !> Predefined delimiters for internal use
  character (len=1), parameter  :: tabdelim      = char(9)
  character (len=1), parameter  :: spacedelim    = ' '
  character (len=1), parameter  :: commadelim    = ','
  character (len=1), parameter  :: colondelim    = ":"
  character (len=1), parameter  :: pipedelim     = "|"
  character (len=1), parameter  :: perioddelim   = "."

  !> ASCII Codes
  character (len=1), parameter  :: newline     = char(10)
  character (len=1), parameter  :: cr          = char(13)
  character (len=1), parameter  :: esc         = char(27)
  character (len=1), parameter  :: NULL        = char(0)

  !> This sets up a graceful fail
  logical                       :: graceful   = .false.

  !> Resize Array interface
  interface resize_array 
    module procedure    resize_int_array, &
                        resize_string_array
  end interface
contains

  !> This is a rewrite of the above and a more complex splitting that used multiple delimiters
  function string_to_int_array(str, delim) result(array)
    character (len=*), intent(in) :: str
    character (len=1), intent(in) :: delim
    character (len=128) :: temp_str
    integer, allocatable :: array (:)
    integer :: count, i, delimcount, length
    integer, allocatable :: delimpos(:)
    
    delimcount = 0
    count = 0
    
    length = len_trim(str)
    !print *, "stringmod: length: ", length, " str:", str

    do i = 1, length
        if (str(i:i) .eq. delim) then
            delimcount = delimcount + 1
        end if
    end do

    if (delimcount .gt. 0) then
        allocate(delimpos(delimcount))
        allocate(array(delimcount+1))
        count = 0
        do i=1,length
            if (str(i:i) .eq. delim) then
                count = count + 1
                delimpos(count) = i
            end if
        end do

        do i=1,delimcount+1
            if (i .eq. 1) then
                ! print "(A,I10)", "I=", i
                temp_str = str(1:delimpos(i)-1)
                ! print "(A, I10, A, A)", "I=", i, " ", temp_str
                array(i) = string_to_integer32(temp_str)
            else if (i <= delimcount) then
                ! print "(A,I10)", "I=", i
                temp_str = str(delimpos(i-1)+1:delimpos(i)-1)
                ! print "(A, I10, A, A)", "I=", i, " ", temp_str
                array(i) = string_to_integer32(temp_str)
            else 
                ! print "(A,I10)", "I=", i
                temp_str = str(delimpos(delimcount)+1:length)
                ! print "(A, I10, A, A)", "I=", i, " ", temp_str
                array(i) = string_to_integer32(temp_str)
            end if
        end do
    end if
  end function string_to_int_array

  !> Subroutine to resize the source array to a new size
  subroutine resize_int_array(array, newsize)
    integer, allocatable    :: array(:)
    integer, intent(in)     :: newsize
    integer, allocatable    :: temp(:)
    integer                 :: sizea, sizeb

    sizea = 0
    sizeb = 0

    allocate(temp(newsize))
    
    if (allocated(array)) then
      sizea = size(temp)
      sizeb = size(array)
      if (sizea .ge. sizeb) then
        temp(1:size(array)) = array
        deallocate(array)
      else
        print "(A)", "*** ARRAY SIZE MISMATCH *** "
        print "(A, I10)", "Current:", sizeb
        print "(A, I10)", "New:    ", sizea
        call exit
      end if
    end if
    array = temp

    deallocate(temp)
  end subroutine resize_int_array

  !> Subroutine to resize the source array to a new size
  !> while outputing to a new array
  subroutine resize_string_array(array,newarray,newsize)
    character(*), allocatable  :: array (:)
    character(*), allocatable  :: newarray (:)
    integer, intent(in)        :: newsize
    integer                    :: sizea, sizeb

    sizea = 0
    sizeb = 0

    if (.not. allocated(newarray)) then
      allocate(newarray(newsize))
    else
      print '(A)', "*** Cannot copy back onto existing array ***"
      call exit
    end if

    if (allocated(array)) then
      sizea = size(array)
      sizeb = size(newarray)
      if (sizeb .ge. sizea) then
        newarray(1:sizea) = array
        deallocate(array)
      else
        print "(A)", "*** ARRAY SIZE MISMATCH *** "
        print "(A, I10)", "Current:", sizeb
        print "(A, I10)", "New:    ", sizea
        call exit
      end if
    end if

  end subroutine

  !> Converts a string to an integer otherwise it bombs out.
  !> Return a wildly absurd negative number if set to gracefully fail.
  !> Unless that is what they are looking for - oops!
  !> Plausible error codes are 5010 and 0.
  function string_to_integer32(str) result(i)
    character (*), intent(in)   :: str
    integer(int32)                    :: i
    integer                           :: ioerr

    read (str, '(I10)', iostat=ioerr) i
    
    if (ioerr .ne. 0) then
      if (graceful) then
        i = -huge(i)
      else 
        print "(A,I10)", "Failed copying INTEGER, ERROR:", ioerr
        print "(A)", "String passed:"
        print "(A)", str 
        call exit
      end if 
    end if
  end function string_to_integer32

  !> Converts a string to an integer otherwise it bombs out.
  !> Return a wildly absurd negative number if set to gracefully fail.
  !> Unless that is what they are looking for - oops! 
  !> Plausible error codes are 5010 and 0.
  function string_to_integer64(str) result(i)
    character (*), intent(in)   :: str
    integer(int64)                    :: i
    integer                           :: ioerr

    read (str, '(I19)', iostat=ioerr) i

    if (ioerr .ne. 0) then
      if (graceful) then
        i = -huge(i)
      else 
        print "(A,I10)", "Failed copying INTEGER, ERROR:", ioerr
        print "(A)", "String passed:"
        print "(A)", str 
        call exit
      end if 
    end if
  end function string_to_integer64

  !> Function to return if a string contains a pattern
  function string_contains(str, pattern) result(r)
    character (*), intent(in)   :: str, pattern
    logical                     :: r 
    integer                     :: s_length, p_length, i 
    
    s_length = len(str)
    p_length = len(pattern)
    r = .false.
    
    if (p_length .le. s_length) then
      do i=1, (s_length-p_length) + 1
        if(str(i:i+p_length-1) .eq. pattern) then
          r = .true.
          return
        end if
      end do
    else
      print "(A)", "Pattern is larger or the same size as the string being searched"
      print "(A)", "STRING:"
      print "(A)", str
      print "(A)", "PATTERN:"
      print "(A)", pattern
    end if
  end function

  !> Function to return the location of a search pattern
  function string_location(str, pattern) result(loc)
    character (*), intent(in)   :: str, pattern
    integer, allocatable        :: loc (:)
    integer                     :: s_length, p_length, i, count

    count = 0
    allocate(loc(count))
    s_length = len(str)
    p_length = len(pattern)

    if (p_length .le. s_length) then
      do i =1, (s_length-p_length) + 1
        if(str(i:i+p_length-1) .eq. pattern) then
          count = count + 1
          call resize_int_array(loc, count)
          loc(count) = i
        end if
      end do
    else
      print "(A)", "Pattern is larger or the same size as the string being searched"
      print "(A)", "STRING:"
      print "(A)", str
      print "(A)", "PATTERN:"
      print "(A)", pattern
    end if
  end function
  
end module stringmodlib
