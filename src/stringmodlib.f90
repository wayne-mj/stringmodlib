module stringmodlib
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, stringmodlib!"
  end subroutine say_hello
end module stringmodlib
