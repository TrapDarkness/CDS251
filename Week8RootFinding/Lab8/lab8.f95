program lab8
    ! Alexander Shumway
    ! CDS251
    ! Lab 8
    ! 03/14/2024
    
    ! This program uses bisection to find roots 

    implicit none

    !declare variables here
    integer :: Count
    real*4 :: a, b, m, Tolerance, MyFunc
   
    ! Prompt and input the fraction.
    print*, "Enter the first x value"
    read*, a
    print*, "Enter the second x value"
    read*, b

    ! Quit if 2 x-values do not produce opposite signs in f(x)
    if (MyFunc(a) * MyFunc(b) .ge. 0.0) then
        print*, "x-values do not produce opposite signs! quitting."
        stop   
    endif


    !Loop Control Variables
    Count = 0
    Tolerance = 1.0e-6

    !bisection loop
    do
        Count = Count + 1
        m = (a + b) / 2.0 ! Computes x midpoint
        if (abs(MyFunc(m)) .lt. Tolerance) exit
        if (Count .gt. 1000) exit
        if (sign(1.0,MyFunc(a)) .eq. sign(1.0,MyFunc(m))) then
            a = m
        else
            b = m
        endif
    enddo
        

    print*, "root: ", m, " f(m): ", MyFunc(m), " Count: ", count !Print decoded message to string

    !clean-up

end program lab8

function MyFunc(x) result(fx)

   implicit none

   real*4 :: x, fx

    fx = sin(x) + 1.5 -0.15*x
      
end function MyFunc



