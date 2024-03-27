program hw9
    ! Alexander Shumway
    ! CDS251
    ! Homework9
    ! 03/27/2024
    
    ! This program uses implements the Midpoint Rule 

    implicit none

    !declare variables here
    integer :: Count
    real*8 :: a, b, m, Tolerance, MyFunc, MyFuncDer, x1, x2, x3
   
    ! Prompt and input the starting x values.
    print*, "Enter the first x value"
    read*, a
    print*, "Enter the second x value"
    read*, b

    ! Quit if 2 x-values do not produce opposite signs in f(x)
    if (MyFunc(a) * MyFunc(b) .ge. 0.0) then
        print*, "x-values do not produce opposite signs! quitting."
        stop   
    endif

    !Bisection Method
    !Loop Control Variables
    Count = 0
    Tolerance = 1.0d-14

    !bisection loop
    do
        Count = Count + 1
        m = (a + b) / 2.0 ! Computes x midpoint
        if (abs(MyFunc(m)) .lt. Tolerance) exit
        if (Count .gt. 1000) exit
        if (sign(1.0d0,MyFunc(a)) .eq. sign(1.0d0,MyFunc(m))) then
            a = m
        else
            b = m
        endif
    enddo
        
    print*, "Bisection Method:"
    print*, "root: ", m, " f(m): ", MyFunc(m), " Count: ", Count !Print results

    !Newton's Method

    !Loop Control Variables
    Count = 0
    Tolerance = 1.0d-14

    !Newton's loop
    x1 = a
    do
        Count = Count + 1
        if (abs(MyFunc(x1)) .lt. Tolerance) exit
        if (Count .gt. 50) exit
        x1 = x1 - MyFunc(x1)/MyFuncDer(x1)
    enddo
        
    print*, "Newton's Method:"
    print*, "root: ", x1, " f(x): ", MyFunc(x1), " Count: ", Count !Print decoded message to string

    !Newton's Method

    !Loop Control Variables
    Count = 0

    !Secant's loop
    x1 = a
    x2 = b
    do
        Count = Count + 1
        if (abs(MyFunc(x1)) .lt. Tolerance) exit
        if (Count .gt. 50) exit
        x3 = x2 - MyFunc(x2)* ((x2 - x1)/(MyFunc(x2) - MyFunc(x1)))

        x1 = x2
        x2 = x3
    enddo
        
    print*, "Secant's Method:"
    print*, "root: ", x3, " f(x): ", MyFunc(x3), " Count: ", Count !Print decoded message to string


    !clean-up

end program hw9

function MyFunc(x) result(fx)

   implicit none

   real*8 :: x, fx

    fx = sin(x) + 1.5 -0.15*x
      
end function MyFunc

function MyFuncDer(x) result(derfx)

    implicit none
 
    real*8 :: x, derfx
 
    derfx = cos(x) - 0.15
       
 end function MyFuncDer
